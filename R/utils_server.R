####prep----
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

#general functions----

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

#general model functions----
computeError <- function(model,pred,dat){
  UseMethod('computeError',model)
}


#Used for rendering parameter config in simulation 
simRenderUI<-function(id){
  UseMethod('simRenderUI',id)
}

#Used for rendering editable tables for parameter config in simulation
simRenderEUI<-function(id){
  UseMethod('simRenderEUI',id)
}

#Server logic behind editable tables
simRenderE<-function(input, output, session, input_df, r, estParams){
  UseMethod('simRenderE',match.call())
}

modelName <- function(model){
  class(model)<-tolower(model)
  useMethod('modelName',model)
}

altpredict <- function(model,data){
  UseMethod('altpredict',model)
}

modelData <- function(model, dataset,lagNum,index_vars,...) {
  class(model)<-tolower(model)
  UseMethod("modelData",model)
}

computeData <-
  function(nVar,
           time,
           error,
           model,
           val,
           burn=1000,
           ...
  ) {
    class(model)<-tolower(model)
    UseMethod("computeData",model)
  }
#model parameter functions-----

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

#' Parent function for computing the residual covariance matrix for a particular model
#' 
#' \code{extractInno} returns the residual covariance matrix of a timeseries model.
#' @param model A fitted ts model, currently ar, arest (from ar.multivariate) and var are supported
#' @return A covariance matrix of the residuals.
#' @family extractInno.ar, extractInno.varest, extractInno.arest
extractInno <- function(model){
  #m <- toupper(class(model))
  UseMethod("extractInno",model)
}

#compute Sigma for MASS::mvrnorm
computeSigma <- function(N, diagVal, offdiagVal) {
  Sigma <- matrix(0, N, N)
  diag(Sigma) <- diagVal
  Sigma[diag(1, N) == 0] <- offdiagVal
  return(Sigma)
}

extractPhi <- function(model){
  UseMethod('extractPhi',model)
}

extractAIC <- function(model){
  UseMethod('extractAIC',model)
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

#extract loading matrix for PCA-based methods
extractLM <- function(model){
  UseMethod('extractLM',model)
}

#return FALSE if stationarity violated
# alternative is the augmented dickey-fuller test!
# alternative is autocorrelation
# https://www.kaggle.com/grosvenpaul/eda-and-time-series-modeling

#model parameter validation-----
stationarity <- function(phi) {
  eigenvalues <- eigen(phi)
  eigenvalues <- eigenvalues$values
  eigenvalues <- as.numeric(eigenvalues)
  if (length(eigenvalues[eigenvalues >= 1] != 0)) {
    return(FALSE)
  } else
    return(TRUE)
}

symmetrize.matrix <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}

validate_phi <- function(phi){
  if(any(is.na(phi))){
    warning("Coefficient estimates are missing from input phi matrix.")
    return(FALSE)
  }
  if(!stationarity(phi)){
    warning("Stationarity violated, choose different values for the coefficient matrix phi.")
    return(FALSE)
  }
  return(TRUE)
}

validate_inno <- function(inno){
  if(!is.positive.definite(inno)){
    warning("Innovation matrix is not positive definite.")
    return(FALSE)
  }
  return(TRUE)
}

fix_inno <- function(inno){
  if(!is.positive.definite(abs(inno))){
    warning("Innovation matrix is not positive definite. Nearest positive definite matrix has been estimated and used.")
    inno <- matrix(Matrix::nearPD((inno))$mat,ncol(inno))
    print('Nearest innovation matrix:')
    print(inno)
  }
}

####server function-----
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
  K <- 3
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
      #incProgress(1 / K, detail = paste("Block ", k))
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


prepCompTP <- function(model,...){
  class(model)<-tolower(model)
  UseMethod('prepSearchTP')
}

prepCompTP.ar <- function(model,model_params){

  phi <- model_params$phi
  inno <- model_params$inno
  return(list(phi=phi,inno=inno))
}

prepCompTP.var <- function(model,model_params){

  phi <- model_params$phi
  inno <- model_params$inno
  return(list(phi=phi,inno=inno))
}


prepCompTP.var <- function(model,model_params){

  phi <- model_params$phi
  inno <- model_params$inno
  lm <- model_params$lm
  return(list(phi=phi,inno=inno,lm=lm))
}

#' Parent function for computing the MSE for a particular model
#' 
#' \code{computeTP} returns the residuals in matrix-format from a fitted timeseries model.
#' @param model A fitted ts model, currently ar, arest (from ar.multivariate) and var are supported
#' @return A matrix of residuals, with columns being the variables.
computeTP <- function(nVar,
                      time,
                      error,
                      gen_model,
                      model1='ar',
                      model2='var',
                      K = 5,
                      index_vars,
                      lagNum = 1,
                      error_metric = 'mse',
                      model_params) {
  #print(paste0('timepoint: ',time))
  #simulate data from the assumed data generating model
  data <- computeData(nVar,
                      time,
                      error,
                      gen_model,
                      val=FALSE,
                      burn=1000,
                      model_params
                      )
  
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
    pred <- altpredict(mod,XTest)
    error <- computeError(mod,pred,XTest)
    
    error1[k] <- error[[error_metric]]
    se1[k] <- error[['sd']]
    
    #model2
    mod <- modelData(model2,
                     DataTrain,
                     lagNum,
                     index_vars)
    pred <- altpredict(mod,XTest)
    error <- computeError(mod,pred,XTest)
    
    error2[k] <- error[[error_metric]]
    se2[k] <- error[['sd']]
  }
  #})
  return(data.frame(cbind(error1,error2,se1,se2)))
}


  
searchTP <- function(nVar,
                     nTime,
                     error,
                     gen_model,
                     model1='ar',
                     model2='var',
                     lagNum,
                     K = 5,
                     max_iter = 25,
                     stepsize_init = 10,
                     stepsize_scaler = .1,
                     index_vars,
                     error_metric,
                     model_params
                     ) {
  sig <- 1
  sampling_k <- 5

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
  
  found2=FALSE
  error=FALSE
  backup_counter <- 0
  mod1_best <- 0
  mod2_best <- 0
  withProgress(message = 'Searching optimal timepoints', value = 0, {
    #stop searching if we reach max iterations or if we reach a consensus of 10
    while (counter < max_iter && found2 == FALSE ){
      #stop searching if we find a point at which model1 is better than model2 or vice versa, depending on mod value
      while (found == FALSE && error == FALSE && counter < max_iter ) {
        info <- NULL
        #basically we want at least 2 tps per fold. otherwise we get dirty ol' crashes
        if(t <= 15){
          t <- 16
          # stepsize <- stepsize * -1
          # mod <- mod * -1
        }
        for(l in 1:sampling_k){
          if(is.null(info)){
            info <- computeTP(nVar,
                        t,
                        error,
                        gen_model,
                        model1,
                        model2,
                        K,
                        index_vars,
                        lagNum,
                        error_metric,
                        model_params)
            } else {
            info <- info +
              computeTP(nVar,
                        t,
                        error,
                        gen_model,
                        model1,
                        model2,
                        K,
                        index_vars,
                        lagNum,
                        error_metric,
                        model_params)
            }
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
          if (round(MSE_MOD1,4) > round(MSE_MOD2,4)) {
            #if our previous best mse difference is smaller than our current mse difference, we have a new best point.
            if(mod2_best - mod1_best > MSE_MOD2-MSE_MOD1){
              mod2_best <- MSE_MOD2
              mod1_best <- MSE_MOD1
              backup <- t
            }
            found = TRUE

            if (backup == t){
              backup_counter <- backup_counter + 1
            } else {
              backup_counter <- 0
            }
            
            if (backup_counter >= 20 && mod == 1) {
              found2 = TRUE
            } else {
              mod <- mod
            }
            
          } else {
            print(t)
            print(stepsize)
            print('-')
            t = t + stepsize
          }
        }
        # } else if (MSE_MOD1 <= MSE_MOD2) {
        #   found = TRUE
        # } else {
        #   if(mod2_best - mod1_best > MSE_MOD2-MSE_MOD1){
        #     mod2_best <- MSE_MOD2
        #     mod1_best <- MSE_MOD1s
        #     backup <- t
        #   }
        #   t = t + stepsize
        # }
        

        counter = counter + 1
        incProgress(1 / max_iter)
      }
      print(t)
      print(stepsize)
      

      
      
      
      #mod <- mod * -1
      if(abs(stepsize) != 1 && abs(stepsize)*stepsize_scaler < 1){
        stepsize <- round(stepsize*mod*stepsize_scaler)
      } 
      
      if(stepsize <= 1){
        stepsize <- 1
      }
      
      found = FALSE
    }
  })
  
  if (backup_counter >= 10) {
    found2 = TRUE
  }
  
  if(found2 == FALSE | is.null(backup)){
    backup <- paste0('No result in ', max_iter,' iterations.')
  } else if (found == FALSE && !is.null(backup)){
    t <- backup + stepsize
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
  colnames(pldf)<-c('tl',model1,model2)
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


