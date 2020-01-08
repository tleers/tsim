#PC-VAR(p) implementation based on Bulteel et al. 2018, modified by Tim Leers
require(psych)

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

####Data-generating arguments (SHINY)----
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

####Name function-----
modelName.pcvar<-function(model){
  return('Principal Component Vector Autoregression')
}



####Data-generating function----
validate_lm <- function(lm){
  if(any(is.na(lm))){
    return(FALSE)
  } else {
    if(any(colMeans(lm)==TRUE) | any(rowMeans(lm)==TRUE) | any(abs(lm)>1)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

#SOURCE NOTE: Code is based on code distributed by the paper of Bulteel, Tuerlinckx, Brose, and Ceulemans
#TITLE:Improved Insight into and Prediction of Network Dynamics by Combining VAR and Dimension Reduction
#doi:10.1080/00273171.2018.1516540

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
  loading_matrix <- mod_vars$lm
  nComp<-ncol(loading_matrix)
  if(val){
    print("Validating transition and innovation matrix.")
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
    print("Matrices are valid.")
    
    if(!validate_lm(loading_matrix)){
      warning("Loading matrix invalid")
      return(NULL)
    }
    print("Matrices are valid.")
  }
  #Generate innovations
  innovations <- mvtnorm::rmvnorm(time+burn,rep(0,nComp),inno)
  
  #Create empty matrix
  U <- matrix(innovations, time + burn, nComp)
  simdata <- matrix(0, time + burn, nComp)
  simdata[1, ] <- U[1, ]
  
  for (row in 2:(time + burn)) {
    simdata[row, ] = simdata[(row - 1), ] %*% phi + U[row, ]
  }
  randomError <- matrix(rnorm(time * nVar, 0, 1), time, nVar)
  E <- sqrt(error) * randomError
  Y <- simdata[-(1:burn), ] 

  Y <- Y %*% t(loading_matrix) + E # Y is the toy data
  return(Y)
}

####Model fit arguments (SHINY)----
modelDataParams.pcvar<-function(model){
  if(!is.null(input$ncomp)){
    return(list(ncomp=input$ncomp))
  } else {
    return(list(ncomp=NULL))
  }
}

computePhi.pcvar <- function(model,sim_params,...) {
  ncomp<-sim_params$ncomp
  Phi <- matrix(0, ncomp, ncomp)
  diag(Phi) <- .5 # The diagonal elements
  Phi[diag(1, ncomp) == 0] <- .2 # The off-diagonal elements
  return(Phi)
}

computeSigma.pcvar <- function(model, sim_params, ...) {
  ncomp<-sim_params$ncomp
  Sigma <- matrix(0, ncomp, ncomp)
  diag(Sigma) <- .5
  Sigma[diag(1, ncomp) == 0] <- .3
  return(Sigma)
}

tpModParams.pcvar <- function(model,...){
  return(list(
    ncomp=input$ncomp
  ))
}

simParams.pcvar <- function(model){
  return(list(ncomp=input$side_ncomp))
}


####Model fit function----
modelData.pcvar <- function(model, dataset, lagNum,index_vars = NULL, mod_vars, ...) {
  if(is.null(mod_vars$ncomp)){
    ncomp <- ncol(dataset)
  } else {
    ncomp <- mod_vars$ncomp
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

#extract loading matrix for PCA-based methods
extractLM <- function(model) {
  UseMethod('extractLM', model)
}

extractLM.pcvar<-function(model){
  return(unclass(model$B_rot))
}

####Error computation function----

# computeError.pcvar <- function(model,pred,dat){
#   error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
#   sqr_resid <- (dat-pred[[1]])^2
#   #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
#   sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
#   error<-rlist::list.append(error,sd)
#   names(error)[[7]] <- 'sd'
#   return(error)
# }

computeError.pcvar <- function(model,pred,dat){
  PCA_varimax<-principal(dat,
                         nfactors=ncol(model$B_rot),
                         rotate="varimax")
  F_rot<-PCA_varimax$scores
  B_rot<-PCA_varimax$loadings
  dat <- F_rot
  
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
  sqr_resid <- (dat-pred[[1]])^2
  #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
  sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
  error<-rlist::list.append(error,sd)
  names(error)[[7]] <- 'sd'
  return(error)
}


###Model prediction function----
# altpredict.pcvar <- function(model,data){
#   pred <- matrix(0,nrow(data),ncol(data))
#   #se <- matrix(0,n.ahead,ncol(data))
#   se <- NULL
#   loading_matrix <- extractLM(model)
#   phi <- extractPhi(model)
#   data <- matrix(unlist(data),ncol=ncol(extractResiduals.pcvar(model)))
#   for (i in 1:ncol(data)){
#     pred[1,] <- data[1,]
#     for (row in 2:nrow(data)) {
#       pred[row, ] = phi %*% (data[row-1,]*t(loading_matrix))
#     }
#   }
#   pred <- pred
#   return(list(pred,se))
# }

altpredict.pcvar <- function(model,data){
  backup <- data
  PCA_varimax<-principal(data,
                         nfactors=ncol(model$B_rot),
                         rotate="varimax")
  F_rot<-PCA_varimax$scores
  B_rot<-PCA_varimax$loadings
  data <- F_rot

  pred <- matrix(0,nrow(data),ncol(data))
  #se <- matrix(0,n.ahead,ncol(data))
  se <- NULL
  loading_matrix <- extractLM(model)
  phi <- extractPhi(model)
  data <- matrix(unlist(data),ncol=ncol(extractResiduals.pcvar(model)))
  for (i in 1:ncol(data)){
    pred[1,] <- data[1,]
    for (row in 2:nrow(data)) {
      pred[row, ] = phi %*% data[row-1,]
    }
  }
  pred <- pred
  return(list(pred,se))
}

# altpredict.pcvar <- function(model,data){
#   ncomp<-nrow(B_rot)
#   backup <- data
#   
#   pred <- matrix(0,ncomp,ncomp)
#   #se <- matrix(0,n.ahead,ncol(data))
#   se <- NULL
#   loading_matrix <- extractLM(model)
#   phi <- extractPhi(model)
#   data <- matrix(unlist(data),ncol=ncol(extractResiduals.pcvar(model)))
#   for (i in 1:ncol(data)){
#     pred[1,] <- data[1,]
#     for (row in 2:nrow(data)) {
#       pred[row, ] = phi %*% data[row-1,]
#     }
#   }
#   pred <- pred  %*% t(1/loading_matrix)
#   return(list(pred,se))
# }


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
    sidebar_width = 25,
    sidebar_start_open = TRUE,
    sidebar_content = tagList(
      numericInput(
        'side_ncomp',
        "Number of components",
        if(!is.null(filedata_updated()) && (input$select_simulation_parameter_origin != 'Manual')){
          ncol(filedata_updated())
        } else {
          input$nVar
        },
        min = 2,
        max = if(!is.null(filedata_updated()) && (input$select_simulation_parameter_origin != 'Manual')){
          ncol(filedata_updated())
        } else {
          input$nVar
        }
      )
    ),
    downloadLink("downloadLMDataset", "Download Loading Matrix")
  )
}


#DYNAMIC UI: simulation----
output$pcvar_sim_output <- renderUI({
  tagList(
    transitionMatrixUI(ns(session)$ns,'phi'),
    innovationMatrixUI(ns(session)$ns,'inno'),
    loadingMatrixUI(ns(session)$ns,'loading_matrix')
  )
})

##Server: Loading matrix table----
output$loading_matrix <- renderRHandsontable({
  if(!is.null(updated_lm())){
    rhandsontable(updated_lm())
  }
})

#Server: LM updating function - used if we change parameter estimate source
updated_lm <- reactive({
  if(!is.null(input_df$df) && (input$select_simulation_parameter_origin != 'Manual')){
    lm_output <- mod_params()$lm
    rownames(lm_output) <- colnames(filedata_updated())
    print(lm_output)
  }else if(input$select_simulation_parameter_origin == 'Manual'){
    lm_output <- matrix(0,input$nVar,input$side_ncomp)
    diag(lm_output)<-1
    colnames(lm_output) <- c(paste("V",1:ncol(lm_output),sep=""))
    rownames(lm_output) <- c(paste("V",1:nrow(lm_output),sep=""))
  } else {
    lm_output <- NULL
  }
  lm_output
})  

#Current input in the table 
current_lm_input <- reactive({
  if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
    dlm <- hot_to_r(input$loading_matrix)
  } else if (input$select_simulation_parameter_origin == 'Manual'){
    dlm <- hot_to_r(input$loading_matrix)
  }
})

#Download LM
output$downloadLMDataset <- downloadHandler(
  filename = function() {
    paste("lm-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(mod_params()$lm, file)
  }
)

#timepoint search dynamic UI
output$pcvar_mod_output <- renderUI({
  tagList(
    numericInput(
      'ncomp',
      "Number of components:",
      if(!is.null(r$data)){
        ncol(r$data)
      } else {
        1
      },
      min = 1,
      max = if(!is.null(r$data)){
        ncol(r$data)
      } else {
        1
      }
    )
  )
})
