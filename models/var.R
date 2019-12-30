#VAR(p) implementation based on Bulteel et al. 2018, modified by Tim Leers

#shiny-specific arguments needed for computedata
computeDataArgs.var <- function(model){
  list(input$nVar,
       input$nTime,
       0,
       input$selection1,
       val=TRUE,
       burn=1000,
       #model-specific parameters
       current_phi_input(),
       current_inno_input())
}
modelDataArgs.var <- function(model){
  return(
    list(
      input$selection1,
      filedata_updated() %>% standardize.popsd,
      selectedLagNum(),
      loaded_dataset_index_variable())
  )
}

relevantModelParameters.varest<-function(tmod){
  return(list(phi=extractPhi(tmod),
              inno=extractInno(tmod)
  ))
}

currentModelParameters.varest <-function(tmod){
  return(list(phi=current_phi_input(),
              inno=current_inno_input())
  )
}
currentModelParameters.var <- currentModelParameters.varest

relevantModelParameters.var <- relevantModelParameters.varest


####Name function-----
modelName.var<-function(model){
  return('Vector Autoregression')
}
modelName.varest<-modelName.var

####Data-generating function----
computeData.var <-function(nVar,
                          time,
                          error,
                          model,
                          val=TRUE,
                          burn=1000,
                          mod_vars,
                          ...){
  
  
  inno <- mod_vars$inno
  phi <- mod_vars$phi
  
  #To avoid crashes, we validate the phi matrix and the innovation matrix.
  if(val){
    if(!validate_phi(phi)){
      warning("Transition matrix invalid")
      return(NULL)
    }
    
    if(!validate_inno(inno)){
      inno<-fix_inno(inno)
      if(is.null(inno)){
        warning("Innovation matrix invalid")
        return(NULL)
      }
    }
  }
  
  #Generate errors
  innovations <- mvtnorm::rmvnorm(time+burn,rep(0,nVar),inno)
  
  #Create empty matrix
  U <- matrix(innovations, time + burn, nVar)
  simdata <- matrix(0, time + burn, nVar)
  simdata[1, ] <- U[1, ]
  
  #withProgress(message = paste0('Simulating ',model), value = 0, {
  for (row in 2:(time + burn)) {
    simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
  }
  randomError <- matrix(rnorm(time * nVar, 0, 1), time, nVar)
  #E <- sqrt(error) * randomError
  E <- 0
  Y <- simdata[-(1:burn), ]  + E
  #})
  
  return(Y)
}

computeData.varest<-computeData.var

####Model fit function----
modelData.varest <- function(model, dataset, lagNum, index_vars = NULL,...) {
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
    VAR(dataset, type = "const", p = lagNum, exogen = dataset[,index_vars])
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

####Parameter extraction functions-----
extractPhi.varest <- function(model){
  nvar <- ncol(model$y)
  VAR_coeff <- matrix(0, nvar, nvar)
  for (i in 1:length(model$varresult)) {
    VAR_coeff[i, ] <-
      model$varresult[[i]]$coefficients[1:nvar]
  }
  phi <- VAR_coeff
}

extractInno.varest <- function(model){
  nvar <- ncol(model$y)
  resid_matrix <- matrix(0,length(model$varresult[[1]]$residuals),nvar)
  VAR_coeff <- matrix(0, nvar, nvar)
  for (i in 1:length(model$varresult)) {
    resid_matrix[,i] <- model$varresult[[i]]$residuals
  }
  return(cov(na.omit(resid_matrix)))
}


####Residual extraction function----
extractResiduals.var <- function(model){
  residuals(model)
}

extractResiduals.varest <- extractResiduals.var

####Error computation function----

computeError.varest <- function(model,pred,dat){
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
  sqr_resid <- (dat-pred[[1]])^2
  #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
  sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
  error<-rlist::list.append(error,sd)
  names(error)[[7]] <- 'sd'
  return(error)
}

computeError.var <- computeError.varest


####Model prediction function----
altpredict.varest <- function(model,data){
  pred <- matrix(0,nrow(data),ncol(data))
  #se <- matrix(0,n.ahead,ncol(data))
  se <- NULL
  
  data <- matrix(unlist(data),ncol=ncol(extractResiduals.var(model)))
  for (i in 1:ncol(data)){
    # tmp<-try(suppressWarnings(stats:::predict.ar(model$fit_list[[i]],data[,i],n.ahead=n.ahead)))
    # #print(class(tmp))
    # if(class(tmp)!='list'){
    #   tmp<-stats:::predict.ar(model$fit_list[[i]],data[,i],n.ahead=n.ahead,method='burg')
    #   warning('predict.ar failed using ols - using burg instead')
    # }
    #withProgress(message = 'Simulating data', value = 0, {
    phi <- extractPhi(model)
    pred[1,] <- data[1,]
    for (row in 2:nrow(data)) {
      pred[row, ] = phi %*% data[row-1,]
    }
    #se[,i] <- as.vector(tmp$se)
  }
  return(list(pred,se))
}

altpredict.var<-altpredict.varest

