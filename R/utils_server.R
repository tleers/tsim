list.of.packages = c(
  "matrixcalc",
  "dplyr",
  "stringr",
  "VARshrink",
  "mlVAR",
  "roxygen2",
  "devtools",
  "golem",
  "mvtnorm"
)
AR_METHOD <<- 'ols'
options(warn=-1)

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)


#compute mean while ignoring zero values
nzmean <- function(x) {
  zvals <- x == 0
  if (all(zvals))
    0
  else
    mean(x[!zvals])
}

pop.var <- function(x)
  var(x) * (length(x) - 1) / length(x)
pop.sd <- function(x)
  sqrt(pop.var(x))

standardize.popsd <- function(dataset) {
  means <- apply(dataset, 2, mean)
  stdevs <- apply(dataset, 2, pop.sd)
  dataCent <- sweep(dataset, 2, means)
  dataSt <- sweep(dataCent, 2, stdevs, "/")
  return(dataSt)
}

#specific functions

#parameter accuracy
computeAccuracy <- function(beta_est, beta_true) {
  return((beta_true - beta_est) ^ 2)
}

# Phi -> Lagged relations
computePhi <- function(N, diagVal, offdiagVal) {
  Phi <- matrix(0, N, N)
  diag(Phi) <- diagVal # The diagonal elements
  Phi[diag(1, N) == 0] <- offdiagVal # The off-diagonal elements
  return(Phi)
}

#return FALSE if stationarity violated
# alternative is the augmented dickey-fuller test!
# alternative is autocorrelation
# https://www.kaggle.com/grosvenpaul/eda-and-time-series-modeling

stationarity <- function(phi) {
  eigenvalues <- eigen(phi)
  eigenvalues <- eigenvalues$values
  if (length(eigenvalues[eigenvalues >= 1] != 0)) {
    return(FALSE)
  } else
    return(TRUE)
}

#compute Sigma for MASS::mvrnorm
computeSigma <- function(N, diagVal, offdiagVal) {
  Sigma <- matrix(0, N, N)
  diag(Sigma) <- diagVal
  Sigma[diag(1, N) == 0] <- offdiagVal
  return(Sigma)
}

extractPhi <- function(model,nvar){
  if(as.logical(lapply(tolower(model$call),grepl,'var')) %>% any()){
    nvar <- ncol(model$y)
    VAR_coeff <- matrix(0, nvar, nvar)
    for (i in 1:length(model$varresult)) {
      VAR_coeff[i, ] <-
        model$varresult[[i]]$coefficients[1:nvar]
    }
    phi <- VAR_coeff
  } else if(as.logical(lapply(tolower(model$call),grepl,'ar.multivariate')) %>% any() | 
            any(names(model)=='ar')==TRUE){
    tmp <- matrix(0,length(model$ar),length(model$ar))
    diag(tmp) <- model$ar
    phi <- tmp
  }
  return(phi)
}


extractInno.varest <- function(model){
  nvar <- ncol(model$y)
  VAR_coeff <- matrix(0, nvar, nvar)
  for (i in 1:length(model$varresult)) {
    VAR_coeff[i, ] <-
      summary(model$varresult[[i]])$coefficients[,2][1:nvar]
  }
  return(VAR_coeff)
}


extractInno.ar <- function(model){
  l <- ncol(model$resid)
  tmp <- matrix(0,ncol=l,nrow=l)
  diag(tmp)<-model$asy.coef
  return(tmp)
}


extractInno.var <- extractInno.varest
extractInno.arest <- extractInno.ar

extractInno <- function(model){
  m <- toupper(class(model))
  UseMethod("extractInno",model)
}

#multivariate support for AR
#handles a matrix column per column
#returns 
ar.multivariate <- function(x,aic=TRUE,order.max=1,na.action=na.fail,demean=TRUE,intercept=demean,type='const',...){
  method<-AR_METHOD
  print(AR_METHOD)
  arest<-NULL
  arest$resid <- matrix(0,ncol=ncol(x),nrow=nrow(x))
  for(i in 1:ncol(x)){
    tmp<-ar(x=x[,i],
            aic=aic,
            order.max=order.max,
            type=type,
            method=method,
            order=order.max
    )
    arest$ar[i]<-tmp$ar
    arest$var.pred[i]<-tmp$var.pred
    arest$x.mean[i] <- tmp$x.mean
    arest$aic[i] <- head(tmp$aic,n=order.max)
    arest$partialacf[i] <- tmp$partialacf
    if(AR_METHOD == 'burg'){
      arest$asy.coef[i] <- tmp$asy.var.coef
    } else if (AR_METHOD == 'ols'){
      arest$asy.coef[i] <- tmp$asy.se.coef[[2]]
    }
    arest$fit_list[[i]] <- tmp
    arest$resid[,i]<- tmp$resid
  }
  arest$call <- match.call()
  class(arest)<-'arest'
  return(arest)
}

#Innovation matrix may still be wrong?
#Based on a model, and a given dataset, fit the model and then extract both the residuals, Phi and Innovation matrix
#
computeParametersFromDataset <- function(model, dataset,lagNum,index_vars){
  dataset <- na.omit(dataset)
  tmod <- modelData(model,dataset,lagNum,index_vars)
  colnames <- c('nvar','num','measerror')
  nvar <- ncol(dataset)
  if(model=='ar'){
    resid <- tmod$resid
    phi <- extractPhi(tmod,nvar)
    inno <- extractInno(tmod)
  } else if (model == 'var'){
    resid<-tmod$y
    # descr<-c(tmod$order,tmod$)
    # names(descr)<-colnames
    phi <- extractPhi(tmod,nvar)
    inno <- extractInno(tmod)
    # resid<-matrix(tmod$resid,nrow(dataset),ncol(dataset))
    # # names(descr)<-colnames
    # phi <- tmod$ar
    # inno <- tmod$asy.var.coef
    # return(list(resid,matrix(phi,ncol=ncol(dataset),nrow=ncol(dataset)),inno))  
  }
  colnames(phi) <- names(dataset)
  rownames(phi) <- names(dataset)
  colnames(inno) <- names(dataset)
  rownames(inno) <- names(dataset)
  return(list(resid,phi,inno))
}

modelData.varest <- function(model, dataset, lagNum, index_vars = NULL) {
  require(vars)
  # variables<-na.omit(str_match(names(dataset),id))
  # day_index <- 'UNIT'
  # beep_index <- 'OCCASION'
  # mlVAR(dataset,variables,id,lags=1,
  #       dayvar=day_index,
  #       beepvar=beep_index,
  #       estimator='default',
  #       temporal='correlated')
  if(!is.null(index_vars)) {
    VAR(dataset, type = "const", p = lagNum,exogen = dataset[,index_vars])
  } else {
    VAR(dataset, type = "const", p = lagNum)
  }
  # ar(
  #   dataset,
  #   type = 'const',
  #   p = lagNum,
  #   method = 'burg',
  #   aic=FALSE,
  #   order.max=ncol(dataset)*ncol(dataset)
  # 
}

modelData.var <- modelData.varest

modelData.ar <- function(model, dataset,lagNum,index_vars = NULL) {
  ar.multivariate(
    dataset, #%>% dplyr::select(-index_vars),
    type = 'const',
    method = AR_METHOD,
    aic=FALSE,
    order.max=lagNum
  )
}

modelData <- function(model, dataset,lagNum,index_vars) {
  class(model)<-tolower(model)
  UseMethod("modelData",model)
}

symmetrize.matrix <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}

validate_phi <- function(phi){
  if(any(is.na(phi))){
    print("Coefficients are missing. This is often due to collinearity between variables.")
    return(FALSE)
  }
  if (is.complex(eigen(phi)$vectors[1])) {
    ifelse(is.complex(phi[1]),"Complex eigenvalues were detected - coefficient matrix is highly unstable","No complex eigenvalues detected.")
    print("If you have estimated the coefficient matrix based on a dataset, the likely reason is collinearity. 
      Make sure that you define the exogeneous variables.
            Alternatively, use a model that reduces dimensions, remove variables manually, or change the values in the coefficient matrix manually.")
    return(FALSE)
  } 
  
  if(!stationarity(phi)){
    print("Stationarity violated, choose different values for the coefficient matrix phi.")
    return(FALSE)
  }
  
  return(TRUE)
}

validate_inno <- function(inno){
  if(!is.positive.definite(inno)){
    print("Innovation matrix is not positive definite.")
    return(FALSE)
  }
  return(TRUE)
}

fix_inno <- function(inno){
  if(!is.positive.definite(abs(inno))){
    print("Innovation matrix is not positive definite. Nearest positive definite matrix has been estimated and used.")
    # inno <- tryCatch(
    #   {
    #     matrix(Matrix::nearPD(inno)$mat,ncol(inno))
    #   }
    # )
    inno <- matrix(Matrix::nearPD(abs(inno))$mat,ncol(inno))
    print(inno)
  }
  # if(!validate_inno(inno)){
  #   return(NULL)
  # }
}

# 
# phi <- current_phi_input()
# inno <- current_inno_input()
# model <- 'ar'
# nTime <- 100
# nVar <- ncol(phi)
# error <- 0.1

computeData_noval <-
  function(nVar,
           nTime,
           error,
           model,
           phi,
           inno) {
    nIntro <- 2000
    if (model == 'ar') {
      #innovations <- rnorm((nTime + nIntro) * nVar, 0, diag(inno))
      #innovations <- MASS::mvrnorm(nTime + nIntro, rep(0, nVar), inno)
      innovations <- rmvnorm(nTime+nIntro,rep(0,nVar),inno)
    } else {
      #innovations <- rnorm((nTime + nIntro) * nVar, 0, diag(inno))
      #innovations <- MASS::mvrnorm(nTime + nIntro, rep(0, nVar), inno)
      innovations <- rmvnorm(nTime+nIntro,rep(0,nVar),inno)
    }
    
    U <- matrix(innovations, nTime + nIntro, nVar)
    simdata <- matrix(0, nTime + nIntro, nVar)
    simdata[1, ] <- U[1, ]
    #withProgress(message = 'Simulating data', value = 0, {
    for (row in 2:(nTime + nIntro)) {
      simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
    }
    randomError <- matrix(rnorm(nTime * nVar, 0, 1), nTime, nVar)
    E <- sqrt(error) * randomError
    Y <- simdata[-(1:nIntro), ]  + E
    #})
    return(Y)
  }

computeData <-
  function(nVar,
           nTime,
           error,
           model,
           phi,
           inno) {
    nIntro <- 2000
    if(!validate_phi(phi)){
      return(NULL)
    }
    
    if(!validate_inno(inno)){
      inno<-fix_inno(inno)
      if(is.null(inno)){
        return(NULL)
      }
    }
    
    if (model == 'ar') {
      # inno[lower.tri(inno)]<-0
      # inno[upper.tri(inno)]<-0
      innovations <- rnorm((nTime + nIntro) * nVar, 0, diag(inno))
    } else {
      Sigma <- inno
      innovations <- MASS::mvrnorm(nTime + nIntro, rep(0, nVar), Sigma)
    }
    
    U <- matrix(innovations, nTime + nIntro, nVar)
    simdata <- matrix(0, nTime + nIntro, nVar)
    simdata[1, ] <- U[1, ]
    withProgress(message = 'Simulating data', value = 0, {
      for (row in 2:(nTime + nIntro)) {
        simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
      }
      randomError <- matrix(rnorm(nTime * nVar, 0, 1), nTime, nVar)
      E <- sqrt(error) * randomError
      Y <- simdata[-(1:nIntro), ]  + E
    })
    return(Y)
  }


compareAccuracy <- function(data,
                            phi,
                            inno,
                            within,
                            lagNum,
                            indexvars,
                            model1,
                            model2) {
  m1 <- modelData(model1,data,lagNum,indexvars)
  m1_acc <- computeAccuracy(extractPhi(m1),phi)
  m2 <- modelData(model2,data,lagNum,indexvars)
  m2_acc <- computeAccuracy(extractPhi(m2),phi)
  return(list(m1_acc,m2_acc))
}



compareCV <- function(data, nVar, nTime, lagNum, index_vars) {
  dataSt <- standardize.popsd(data)
  K <- 10
  CV <- as.numeric(factor(sort(rank(1:nTime %% 10))))
  ### CV loop
  indLastElFold <- tapply(seq_along(CV), CV, max)
  indFirstElFold <- c(1, indLastElFold[-K])
  mse_ar <<- matrix(0, K, 1)
  mse_var <<- matrix(0, K, 1)
  withProgress(message = 'Comparing blocked CV performance', value = 0, {
    for (k in 1:K) {
      # Prepare test data
      DataTest <- dataSt[indFirstElFold[k]:indLastElFold[k], ]
      XTest <-
        DataTest[-nrow(DataTest), ]
      YTest <- DataTest[-1, ]
      # Prepare training data
      DataTrain <-
        dataSt[-(indFirstElFold[k]:(indLastElFold[k] - 1)), ]
      
      res <- modelData('ar',
                       DataTrain,
                       lagNum,
                       index_vars)$resid
      
      #make sure to remove NA's since first and last observation usually not predicted or taken into account
      res <- na.omit(as.data.frame(res))
      #melt dem bois
      res2 <- reshape2::melt(res, na.rm = TRUE)
      res2_y <- rep(1:length(res[, 1]), nVar)
      res2 <- cbind(res2, res2_y)
      mse_ar[k] <- sum(res ^ 2) / length(as.vector(res))
      res <- residuals(modelData('var',
                                 DataTrain,
                                 lagNum,
                                 index_vars))
      res <- na.omit(as.data.frame(res))
      res2 <- reshape2::melt(res, na.rm = TRUE)
      res2_y <- rep(1:length(res[, 1]), nVar)
      res2 <- cbind(res2, res2_y)
      mse_var[k] <- sum(res ^ 2) / length(as.vector(res))
      incProgress(1 / K, detail = paste("Block ", k))
    }
    MSE_AR <- sum(mse_ar) / K
    MSE_VAR <- sum(mse_var) / K
  })
  if (MSE_AR > MSE_VAR) {
    return("VAR")
  } else {
    return("AR")
  }
}


computeCV <- function(data, model, nTime, nVar, lagNum) {
  dataSt <- standardize.popsd(data)
  K <- 10
  CV <- as.numeric(factor(sort(rank(1:nTime %% 10))))
  ### CV loop
  indLastElFold <- tapply(seq_along(CV), CV, max)
  indFirstElFold <- c(1, indLastElFold[-K])
  mse <<- matrix(0, K, 1)
  #withProgress(message = 'Computing block-based MSE', value = 0, {
  for (k in 1:K) {
    # Prepare test data
    DataTest <- dataSt[indFirstElFold[k]:indLastElFold[k], ]
    XTest <-
      DataTest[-nrow(DataTest), ]
    YTest <- DataTest[-1, ]
    # Prepare training data
    DataTrain <-
      dataSt[-(indFirstElFold[k]:(indLastElFold[k] - 1)), ]
    if (model == 'ar') {
      res <- ar.multivariate(DataTrain,
                             type = "const",
                             method = AR_METHOD,
                             aic=FALSE,
                             order.max=lagNum)$resid
      
      if(is.null(res)){
        showNotification('Singular matrix - computation not possible. Change parameters.',type="error")
        return(NULL)
      }
    } else if (model == 'var') {
      res <- residuals(VAR(DataTrain, type = "const", p = 1))
    }
    res <- na.omit(matrix(as.vector(res),ncol=ncol(data)))
    res2 <- reshape2::melt(res, na.rm = TRUE)
    names(res2)<-c('X','variable','value')
    res2_y <- rep(1:length(res[, 1]), nVar)
    res2 <- cbind(res2, res2_y) %>% dplyr::select(-starts_with('X'))
    mse[k] <- sum(res ^ 2) / length(as.vector(res))
    #incProgress(1 / K, detail = paste("Block ", k))
  }
  #})
  list(sum(mse) / K, res2)
}



#' @inheritParams 
extractResiduals.ar <- function(model){
  model$resid
}

extractResiduals.var <- function(model){
  residuals(model)
}

#' Parent function pointing to other functions that extract the residuals from a fitted timeseries model
#' 
#' \code{extractResiduals} returns the residuals in matrix-format from a fitted timeseries model.
#' @param model A fitted ts model, currently ar, arest (from ar.multivariate) and var are supported
#' @return A matrix of residuals, with columns being the variables.
#' @family extractresiduals.ar  
extractResiduals <- function(model){
  UseMethod('extractResiduals',model)
}

#' Parent function for computing the MSE for a particular model
#' 
#' \code{extractResiduals} returns the residuals in matrix-format from a fitted timeseries model.
#' @param model A fitted ts model, currently ar, arest (from ar.multivariate) and var are supported
#' @return A matrix of residuals, with columns being the variables.
#' @family extractresiduals.ar  
computeTP <- function(nVar,
                      time,
                      error,
                      gen_model,
                      model1='ar',
                      model2='var',
                      phi,
                      inno,
                      K = 5,
                      index_vars,
                      lagNum = 1,
                      error_metric = 'mse') {
  
  print(paste0('timepoint: ',time))
  #simulate data from the assumed data generating model
  data <- computeData_noval(nVar,
                            time,
                            error,
                            gen_model,
                            phi,
                            inno)
  
  #data <- standardize.popsd(data)
  CV <- as.numeric(factor(sort(rank(1:(
    nrow(data) - 1
  ) %% K))))
  ### CV loop
  indLastElFold <- tapply(seq_along(CV), CV, max)
  indFirstElFold <- c(1, indLastElFold[-K])
  error1<- matrix(0, K, 1)#error metric model1
  error2<- matrix(0, K, 1)#error metric model2
  se1<- matrix(0,K,1)#SE model1
  se2<- matrix(0,K,1)#SE model2
  
  #withProgress(message = 'Computing blocked CV', value = 0, {
  for (k in 1:K) {
    # Prepare test data
    DataTest <- data[indFirstElFold[k]:indLastElFold[k], ]
    XTest <-
      DataTest[-nrow(DataTest), ]
    YTest <- DataTest[-1, ]
    # Prepare training data
    DataTrain <-
      data[-(indFirstElFold[k]:(indLastElFold[k] - 1)), ]
    colnames(DataTrain)<-1:ncol(DataTrain)
    
    #model1
    mod <- modelData(model1,
                     DataTrain,
                     lagNum,
                     index_vars)
    pred <- predict(mod,XTest,n.ahead=nrow(XTest))
    error <- extractError(mod,pred,XTest)
    
    error1[k] <- error[[error_metric]]
    se1[k] <- error[['sd']]
    
    #model2
    mod <- modelData(model2,
                     DataTrain,
                     lagNum,
                     index_vars)
    pred <- predict(mod,XTest,n.ahead=nrow(XTest))
    error <- extractError(mod,pred,XTest)
    
    error2[k] <- error[[error_metric]]
    se2[k] <- error[['sd']]
  }
  #})
  return(data.frame(cbind(error1,error2,se1,se2)))
}

extractError <- function(model,pred,dat){
  UseMethod('extractError',model)
}

extractError.arest <- function(model,pred,dat){
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
  sqr_resid <- (dat-pred[[1]])^2
  #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
  sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
  error<-list.append(error,sd)
  names(error)[[7]] <- 'sd'
  return(error)
}

extractError.varest <- function(model,pred,dat){
  tmp <- NULL
  for (i in 1:length(pred$fcst)){
    tmp<-cbind(tmp,pred$fcst[[i]][,1])
  }
  pred<-tmp
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred %>% as.vector())
  
  sqr_resid <- (dat-pred)^2
  sd <- sd(unlist(sqr_resid))/sqrt(nrow(dat))
  
  error<-list.append(error,sd)
  names(error)[[7]] <- 'sd'
  
  return(error)
  
}


predict.arest <- function(model,data,n.ahead, ...){
  pred <- matrix(0,n.ahead,ncol(data))
  se <- matrix(0,n.ahead,ncol(data))
  for (i in 1:ncol(data)){
    tmp<-stats:::predict.ar(model$fit_list[[i]],data[,i],n.ahead=n.ahead)
    pred[,i] <- as.vector(tmp$pred)
    se[,i] <- as.vector(tmp$se)
  }
  return(list(pred,se))
}

#out<-searchTP(3,nrow(sim_var),0,'var','ar','var',extractPhi(vartest,3),symmetrize.matrix(extractInno(vartest)),1,5,50,10,.5,NULL,'mse')

searchTP <- function(nVar,
                     nTime,
                     error,
                     gen_model,
                     model1='ar',
                     model2='var',
                     phi,
                     inno,
                     lagNum,
                     K = 5,
                     max_iter = 25,
                     stepsize_init = 10,
                     stepsize_scaler = .1,
                     index_vars,
                     error_metric) {
  
  sampling_k <- 5
  if(!validate_phi(phi)){
    return(NULL)
  }
  if(!validate_inno(inno)){
    inno<-fix_inno(inno)
    if(is.null(inno)){
      return(NULL)
    }
  }
  #Minimum number of data points needed to even consider computing a crossvalidation. 
  #Needed to prevent crashes
  minfold <- 1
  
  #initialize
  stepsize <- stepsize_init
  t <- nTime
  
  #used as an indicator of whether tp was found
  found = FALSE
  counter = 0
  
  #"modifier" for stepsize, helps us with 
  mod <- 1
  
  #remembers the last lowest tp
  backup <- 0
  
  #iterator for msedf
  #msedf keeps information on mse per fold
  dfc <- 1
  mse_df <- list()
  
  info <- computeTP(nVar,
                    nTime,
                    error,
                    gen_model,
                    model1,
                    model2,
                    phi,
                    inno,
                    K,
                    index_vars,
                    lagNum,
                    error_metric)
  print(info)
  if(any(is.null(info))){
    print(paste0(c('Error occurred in the creation of simulation with timepoints: ',nTime
    )
    )
    )
    return(NULL)
  }
  
  MSE_MOD1 <-  mean(na.omit(info[,1]))
  MSE_MOD2 <- mean(na.omit(info[,2]))
  
  SE_MOD1 <- mean(na.omit(info[,3]))
  SE_MOD2 <- mean(na.omit(info[,4]))
  
  mse_df[[dfc]] <- list(t,MSE_MOD1,MSE_MOD2,info[,1],info[,2],SE_MOD1,SE_MOD2,info[,3],info[,4])
  dfc <- dfc + 1
  
  #find out if our timepoint of starting already has lower or higher AR-VAR MSE, then we wanna srch in opposite direction
  if (MSE_MOD1 >= (round(MSE_MOD2,2))) {
    stepsize <- -stepsize
    #mod <- -1
  }
  t <- t + stepsize
  found2=FALSE
  error=FALSE
  backup_counter <- 0
  mod1_best <- 0
  mod2_best <- 0
  withProgress(message = 'Searching optimal timepoints', value = 0, {
    while (counter < max_iter && found2 == FALSE ){
      while (found == FALSE && error == FALSE && counter < max_iter ) {
        
        
        for(l in 1:sampling_k){
          info <- info +
            computeTP(nVar,
                      t,
                      error,
                      gen_model,
                      model1,
                      model2,
                      phi,
                      inno,
                      K,
                      index_vars,
                      lagNum,
                      error_metric)
          
        }
        info <- info / sampling_k
        MSE_MOD1 <- mean(na.omit(info[,1]))
        MSE_MOD2 <- mean(na.omit(info[,2]))
        SE_MOD1 <- mean(na.omit(info[,3]))
        SE_MOD2 <- mean(na.omit(info[,4]))
        
        print(model1)
        print(MSE_MOD1)
        print(abs(SE_MOD1))
        print(model2)
        print(MSE_MOD2)
        print(abs(SE_MOD2))
        
        mse_df[[dfc]] <- list(t,MSE_MOD1,MSE_MOD2,info[,1],info[,2],SE_MOD1,SE_MOD2,info[,3],info[,4])
        dfc <- dfc + 1
        
        if(mod == 1){ # In this case, we are trying to find any point at which the MSE of model 1 exceeds that of model2
          if (MSE_MOD1 >= round(MSE_MOD2,2)) {
            #if our previous best mse difference is smaller than our current mse difference, we have a new best point.
            if(mod2_best - mod1_best > MSE_MOD2-MSE_MOD1){
              mod2_best <- MSE_MOD2
              mod1_best <- MSE_MOD1
              backup <- t
            }
            found = TRUE
          } else {
            t = t + stepsize
          }
        } else if (MSE_MOD1 <= MSE_MOD2) {
          found = TRUE
        } else {
          if(mod2_best - mod1_best > MSE_MOD2-MSE_MOD1){
            mod2_best <- MSE_MOD2
            mod1_best <- MSE_MOD1
            backup <- t
          }
          t = t + stepsize
        }
        
        #basically we want at least 2 tps per fold. otherwise we get dirty ol' crashes
        if(t <= K*2){
          stepsize <- -stepsize
          mod <- mod * -1
          t <- time + stepsize
        }
        counter = counter + 1
        incProgress(1 / max_iter)
      }
      print(t)
      
      if (backup == t){
        backup_counter <- backup_counter + 1
      } else {
        backup_counter <- 0
      }
      
      if (backup_counter >= 10 && mod == 1) {
        found2 = TRUE
      } else {
        mod <- -mod
      }
      
      #mod <- mod * -1
      stepsize <- as.integer(stepsize*mod*stepsize_scaler)
      found = FALSE
    }
  })
  
  if (backup_counter >= 10) {
    found2 = TRUE
  }
  
  if(found2 == FALSE | is.null(backup)){
    backup <- paste0('No result in ', max_iter,' iterations.')
  } else if (found == FALSE && !is.null(backup)){
    t <- backup
  }
  return(list(backup,convertmsedf(mse_df,K,model1,model2),mod1_best,mod2_best))
}



convertmsedf <- function(msedf, K, model1,model2){
  l <- length(msedf)
  tl<-data.frame(matrix(matrix(0,ncol=1,nrow=l)))
  arl<-data.frame(matrix(matrix(0,ncol=1,nrow=l)))
  varl<-data.frame(matrix(matrix(0,ncol=1,nrow=l)))
  farl <-data.frame(matrix(0,nrow=l,ncol=K))
  fvarl <- data.frame(matrix(0,nrow=l,ncol=K))
  
  for(i in 1:l){
    tl[i,1] <- msedf[[i]][[1]]
    arl[i,1] <- msedf[[i]][[2]]
    varl[i,1] <- msedf[[i]][[3]]
    farl[i,] <- msedf[[i]][[4]]
    fvarl[i,] <- msedf[[i]][[5]]
  }
  
  tmp1<-cbind(tl,farl)
  names(tmp1)<-c('tl',1:K)
  tmp1<-reshape2::melt(tmp1,id.vars=1)
  
  tmp<-cbind(tl,fvarl)
  names(tmp)<-c('tl',1:K)
  tmp<-reshape2::melt(tmp,id.vars=1)
  
  tmp<-cbind(model2,tmp)
  tmp1<-cbind(model1,tmp1)
  names(tmp)<-c('model','tl','fold','mse')
  names(tmp1)<-c('model','tl','fold','mse')
  
  fold_df <- rbind(tmp1,tmp)
  
  pldf <- cbind(tl,arl,varl)
  colnames(pldf)<-c('tl','ar','var')
  pldf<-reshape2::melt(pldf,id.vars=1)
  names(pldf) <- c('tl','model','mse')
  return(list(pldf,fold_df))
}

# #future plotting
# library(ggTimeSeries)
# MindMaastricht <- read.csv('../input/MindMaastricht.csv')
# ggplot_waterfall(MindMaastricht,'dayno','pieker')
# ggplot_horizon(MindMaastricht,'dayno','pieker',bandwidth=.1)
# library(ggplot2)
# dfData = data.frame(x = 1:1000, y = cumsum(rnorm(1000)))
# p1 = ggplot_horizon(dfData,'x','y')
# p1# add new geoms or coloursp1 +geom_text(label ='!!!') +scale_colour_continuous(low ='red', high ='green')
# 
# #autofitting
# library(autovarCore)
# #autovar(MindMaastricht)


