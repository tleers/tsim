

#PC-VAR(p) implementation based on Bulteel et al. 2018, modified by Tim Leers

#shiny-specific arguments needed for computedata
computeDataArgs.pcvar <- function(model){
  list(input$nVar,
       input$nTime,
       0,
       input$selection1,
       val=TRUE,
       burn=1000,
       #model-specific parameters
       current_phi_input(),
       current_inno_input(),
       current_lm_input())
}

computeDataArgsRAW.pcvar <- function(model){
  list(nVar,
       time,
       error,
       gen_model,
       val=FALSE,
       burn=1000,
       #model-specific parameters
       model_params$phi,
       model_params$inno,
       model_params$lm
  )
}

modelDataArgsRAW.pcvar <- function(model){
  return(
    list(
      input$selection1,
      filedata_updated(),
      selectedLagNum(),
      loaded_dataset_index_variable(),
      input$ncomp)
  )
}

modelDataArgs.pcvar <- function(model){
  return(
    list(
      input$selection1,
      filedata_updated() %>% standardize.popsd,
      selectedLagNum(),
      loaded_dataset_index_variable(),
      ncol(filedata_updated())#input$ncomp)
  )
  )
}

#parameters that are estimated based on the model fit to the dataset, and which should be editable for the user
relevantModelParameters.pcvar<-function(tmod){
  return(list(phi=extractPhi(tmod),
              inno=extractInno(tmod),
              lm=extractLM(tmod)
  ))
}

currentModelParameters.pcvar<-function(tmod){
  return(list(phi=current_phi_input(),
              inno=current_inno_input(),
              lm=current_lm_input()
  ))
}


####Name function-----
modelName.pcvar<-function(model){
  return('Principal Component Vector Autoregression')
}

####Data-generating function----
computeData.pcvar <- function(nVar,
                              time,
                              error,
                              model,
                              val=TRUE,
                              burn=1000,
                              mod_vars,
                              ....){
  

  inno <- mod_vars$inno
  phi <- mod_vars$phi
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
  
  loading_matrix <- mod_vars$lm
  
  #Generate errors
  innovations <- mvtnorm::rmvnorm(time+burn,rep(0,nVar),inno)
  
  #Create empty matrix
  U <- matrix(innovations, time + burn, nVar)
  simdata <- matrix(0, time + burn, nVar)
  simdata[1, ] <- U[1, ]
  
  for (row in 2:(time + burn)) {
    simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
  }
  randomError <- matrix(rnorm(time * nVar, 0, 1), time, nVar)
  #E <- sqrt(error) * randomError
  E <- 0
  Y <- simdata[-(1:burn), ] 
  # 4. Combine component scores F, with loading matrix B and error values E to obtain latent structure
  Y <- Y %*% t(loading_matrix) + E # Y is the toy data
  return(Y)
}

####Model fit function----
modelData.pcvar <- function(model, dataset, lagNum,index_vars = NULL, ncomp=ncol(dataset)) {
  require(psych)
  if(is.null(ncomp)){
    ncomp <- ncol(dataset)
  }
  PCA_varimax<-principal(dataset,
                         nfactors=ncomp,
                         rotate="varimax")
  F_rot<-PCA_varimax$scores
  B_rot<-PCA_varimax$loadings
  PCVARfit <- lsfit(head(F_rot,-1),tail(F_rot,-1),intercept = FALSE) # VAR(1) analysis on rotated scores
  class(PCVARfit)<-'pcvar'
  PCVARfit$F_rot<-F_rot
  PCVARfit$B_rot<-B_rot
  return(PCVARfit)
}

####Parameter extraction functions-----
extractPhi.pcvar <- function(model){
  return(model$coefficients)
}

extractInno.pcvar <- function(model){
  return(cov(na.omit(extractResiduals.pcvar(model))))
}

####Residual extraction function----
extractResiduals.pcvar <- function(model){
  return(model$residuals)
} 

extractLM.pcvar<-function(model){
  return(unclass(model$B_rot))
}

####Error computation function----

computeError.pcvar <- function(model,pred,dat){
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
  sqr_resid <- (dat-pred[[1]])^2
  #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
  sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
  error<-rlist::list.append(error,sd)
  names(error)[[7]] <- 'sd'
  return(error)
}

####Model prediction function----
altpredict.pcvar <- function(model,data){
  pred <- matrix(0,nrow(data),ncol(data))
  #se <- matrix(0,n.ahead,ncol(data))
  se <- NULL
  
  data <- matrix(unlist(data),ncol=ncol(extractResiduals.pcvar(model)))
  for (i in 1:ncol(data)){
    phi <- extractPhi(model)
    pred[1,] <- data[1,]
    for (row in 2:nrow(data)) {
      pred[row, ] = phi %*% data[row-1,]
    }
  }
  return(list(pred,se))
}


#SHINY MODULES FOR UI
##UI: Loading matrix----

loadingMatrixUI <- function(id, label="loading_matrix"){
  boxPlus(
    enable_sidebar=TRUE,
    solidheader=TRUE,
    collapsible=TRUE,
    status="success",
    title="Loading Matrix",
    rHandsontableOutput(label),
    # fileInput(
    #   'lmfile',
    #   'Upload Loading matrix',
    #   multiple = FALSE,
    #   accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
    # ),
    sidebar_width = 25,
    sidebar_start_open = FALSE,
    sidebar_content = tagList(
      downloadLink("downloadLMDataset", "Download Loading Matrix")
    )
  )
}

##Server: Loading matrix----
output$loading_matrix <- renderRHandsontable({
  if(!is.null(updated_lm())){
    rhandsontable(updated_lm())
  }
})

updated_lm <- reactive({
  if(!is.null(input_df$df) && (input$select_simulation_parameter_origin != 'Manual')){
    lm_output <- mod_params()$lm
    colnames(lm_output) <- colnames(filedata_updated())
    rownames(lm_output) <- colnames(filedata_updated())
    print(lm_output)
  }else if(input$select_simulation_parameter_origin == 'Manual'){
    lm_output <- matrix(0,input$nVar,input$nVar)
    diag(lm_output)<-1
    colnames(lm_output) <- c(paste("V",1:ncol(lm_output),sep=""))
    rownames(lm_output) <- c(paste("V",1:nrow(lm_output),sep=""))
  } else {
    lm_output <- NULL
  }
  lm_output
})  


current_lm_input <- reactive({
  if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
    dlm <- hot_to_r(input$loading_matrix)
  } else if (input$select_simulation_parameter_origin == 'Manual'){
    dlm <- hot_to_r(input$loading_matrix)
  }
})