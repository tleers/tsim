list.of.packages = c(
  "matrixcalc",
  "dplyr",
  "stringr",
  "VARshrink",
  "mlVAR",
  "roxygen2",
  "devtools",
  "golem"
)
#source('datasets.R')


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

#compute Sigma for mvrnorm
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
            any(names(arcmv)=='ar')==TRUE){
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
  diag(tmp)<-model$asy.var.coef
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
  method<-'burg'
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
    arest$asy.var.coef[i] <- tmp$asy.var.coef
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
    method = 'ols',
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
    nIntro <- 1000
    if (model == 'ar') {
      innovations <- rnorm((nTime + nIntro) * nVar, 0, diag(inno))
    } else {
      
      innovations <- mvrnorm(nTime + nIntro, rep(0, nVar), inno)
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

computeData <-
  function(nVar,
           nTime,
           error,
           model,
           phi,
           inno) {
    nIntro <- 1000
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
      innovations <- mvrnorm(nTime + nIntro, rep(0, nVar), Sigma)
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
      res2 <- melt(res, na.rm = TRUE)
      res2_y <- rep(1:length(res[, 1]), nVar)
      res2 <- cbind(res2, res2_y)
      mse_ar[k] <- sum(res ^ 2) / length(as.vector(res))
      res <- residuals(modelData('var',
                       DataTrain,
                       lagNum,
                       index_vars))
      res <- na.omit(as.data.frame(res))
      res2 <- melt(res, na.rm = TRUE)
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

computeCV <- function(data, model, nTime, nVar) {
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
                               method = "burg",
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
      res2 <- melt(res, na.rm = TRUE)
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

computeTP <- function(nVar,
                      time,
                      error,
                      model,
                      phi,
                      inno,
                      K = 5) {
  lagNum <- 1
  if(!validate_phi(phi)){
    return(NULL)
  }
  
  if(!validate_inno(inno)){
    inno<-fix_inno(inno)
    if(is.null(inno)){
      return(NULL)
    }
  } 
  print('tp')
  print(time)
  
  data <- computeData_noval(nVar,
                            time,
                            error,
                            model,
                            phi,
                            inno)
  #set.seed('1')
  
  #cv data set one
  dataSt <- standardize.popsd(data)
  CV <- as.numeric(factor(sort(rank(1:(
    time - 1
  ) %% 10))))
  ### CV loop
  indLastElFold <- tapply(seq_along(CV), CV, max)
  indFirstElFold <- c(1, indLastElFold[-K])
  mse_ar <<- matrix(0, K, 1)
  mse_var <<- matrix(0, K, 1)
  withProgress(message = 'Computing blocked CV', value = 0, {
    for (k in 1:K) {
      # Prepare test data
      DataTest <- dataSt[indFirstElFold[k]:indLastElFold[k], ]
      XTest <-
        DataTest[-nrow(DataTest), ]
      YTest <- DataTest[-1, ]
      # Prepare training data
      DataTrain <-
        dataSt[-(indFirstElFold[k]:(indLastElFold[k] - 1)), ]
      colnames(DataTrain)<-1:ncol(DataTrain)
      res <- modelData('ar',
                         DataTrain,
                         lagNum,
                         index_vars)$resid
      #make sure to remove NA's since first and last observation usually not predicted or taken into account
      res <- as.vector(na.omit((res)))
      mse_ar[k] <- sum(res ^ 2) #/ length(res)
      
      res <- residuals(modelData('var',
                                   DataTrain,
                                   lagNum,
                                   index_vars))
      res <- as.vector(na.omit(res))
      mse_var[k] <- sum(res ^ 2) #/ length(res)
      
      incProgress(1 / K)
    }
  })
  #return(list(sum(mse_ar) / K, sum(mse_var) / K))
  return(data.frame(cbind(mse_ar,mse_var)))
}



searchTP <- function(nVar,
                     nTime,
                     error,
                     model,
                     phi,
                     inno,
                     K = 5,
                     max_iter = 25,
                     stepsize_init = 10,
                     stepsize_scaler = .1) {
  #Minimum number of data points needed to even consider computing a crossvalidation. 
  minfold <- 10
  stepsize <- stepsize_init
  t <- nTime
  found = FALSE
  counter = 0
  mod <- 1
  backup <- NULL
  
  #iterator for msedf
  #msedf keeps information on mse per fold
  dfc <- 1
  mse_df <- list()
  
  if(as.integer(t / K) <= minfold){
    showNotification("Amount of timepoints too small. Start with higher initial timepoints and/or reduce the fold size.",
                     type="warning")
    return(NULL)
  }
  
  info <- computeTP(nVar,
              nTime,
              error,
              model,
              phi,
              inno,
              K)
  print(info)
  if(any(is.null(info))){
    print(paste0(c('Error occurred in the creation of simulation with timepoints: ',nTime
    )
    )
    )
    return(NULL)
  }
  
  MSE_AR <-  sum(info[,1]) / K
  MSE_VAR <- sum(info[,2]) / K
  
  print(MSE_AR)
  print(MSE_VAR)
  
  mse_df[[dfc]] <- list(t,MSE_AR,MSE_VAR,info[,1],info[,2])
  dfc <- dfc + 1

  #find out if our timepoint of starting already has lower or higher AR-VAR MSE, then we wanna srch in opposite direction
  if (as.integer(MSE_AR) >= as.integer(MSE_VAR)) {
    stepsize <- -stepsize
    mod <- -1
  }
  t <- t + stepsize
  tc <- 1
  withProgress(message = 'Finding optimal timepoints', value = 0, {
    while (tc < 10){
      while (found == FALSE & counter < max_iter) {
        
        if(as.integer(t / K) <= minfold){
          showNotification("Amount of timepoints too small. Start with higher initial timepoints and/or reduce the fold size.",
                           type="warning")
          return(list(t,convertmsedf(mse_df,K)))
        }
        
        info <-
          computeTP(nVar,
                    t,
                    error,
                    model,
                    phi,
                    inno,
                    K)
        
        MSE_AR <-  sum(info[,1]) / K
        MSE_VAR <- sum(info[,2]) / K
        
        mse_df[[dfc]] <- list(t,MSE_AR,MSE_VAR,info[,1],info[,2])
        
        dfc <- dfc + 1
        
        print('mse ar')
        print(MSE_AR)
        print('mse var')
        print(MSE_VAR)
        print('--')
        
        
        if(mod == 1){
          if (MSE_AR >= MSE_VAR) {
            found = TRUE
          } else {
            t = t + stepsize
          }
        } else if (MSE_AR <= MSE_VAR) {
            found = TRUE
        } else {
            t = t + stepsize
        }
  
        if(as.integer(t / K-1) <= minfold){
          showNotification(paste0("Choose different timepoint/fold size. MSE is already better for more complex model at estimation iteration: ",dfc),
                           type="warning")
          return(list(t - (stepsize*mod),convertmsedf(mse_df,K)))
        }
        
        counter = counter + 1
        incProgress(1 / (max_iter * 2))
        
      }
      if(found == TRUE){
        backup <- t
      }
      if(counter < max_iter){
        found = FALSE
        counter = 0
      } else {
        mod <- mod * -1
        stepsize <- stepsize*mod
      }
      stepsize <- as.integer(stepsize*stepsize_scaler)
    }
    tc <- tc + 1
  })
    
  if(found == FALSE && is.null(backup)){
    #t <- paste0('No result in ', max_iter,' iterations.')
    t
  } else if (found == FALSE && !is.null(backup)){
    t <- backup
  }
  return(list(t-stepsize,convertmsedf(mse_df,K)))
}



convertmsedf <- function(msedf, K){
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
  tmp1<-melt(tmp1,id.vars=1)
  
  tmp<-cbind(tl,fvarl)
  names(tmp)<-c('tl',1:K)
  tmp<-melt(tmp,id.vars=1)
  
  tmp<-cbind('var',tmp)
  tmp1<-cbind('ar',tmp1)
  names(tmp)<-c('model','tl','fold','mse')
  names(tmp1)<-c('model','tl','fold','mse')
  
  fold_df <- rbind(tmp1,tmp)
  
  pldf <- cbind(tl,arl,varl)
  colnames(pldf)<-c('tl','ar','var')
  pldf<-melt(pldf,id.vars=1)
  names(pldf) <- c('tl','model','mse')
  return(list(pldf,fold_df))
}



