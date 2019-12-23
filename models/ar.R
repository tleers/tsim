#AR(p) implementation made by Tim Leers

####General model function----
#' Multivariate version of AR
#' 
#' \code{ar.multivariate} returns arest model
ar.multivariate <- function(x,aic=TRUE,order.max=1,na.action=na.fail,demean=TRUE,intercept=demean,type='const',...){
  method<-AR_METHOD
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
    arest$var.pred[i]<-as.vector(tmp$var.pred)
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


####Name function-----
modelName.ar<-function(model){
  return('Autoregression')
}
modelName.arest<-modelName.ar

####Data-generating function----
computeData.ar <-function(nVar,
                           time,
                           error,
                           model,
                           val=TRUE,
                           burn=1000,
                           phi,
                           inno,
                           ...){
  
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
  innovations <- rmvnorm(time+burn,rep(0,nVar),inno)
  
  #Create empty matrix
  U <- matrix(innovations, time + burn, nVar)
  simdata <- matrix(0, time + burn, nVar)
  simdata[1, ] <- U[1, ]
  
  #withProgress(message = paste0('Simulating ',model), value = 0, {
  for (row in 2:(time + burn)) {
    simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
  }
  randomError <- matrix(rnorm(time * nVar, 0, 1), time, nVar)
  E <- sqrt(error) * randomError
  Y <- simdata[-(1:burn), ]  + E
  #})
  
  return(Y)
}

computeData.varest<-computeData.var

####Model fit function----
modelData.ar <- function(model, dataset,lagNum,index_vars = NULL) {
  ar.multivariate(
    dataset, #%>% dplyr::select(-index_vars),
    type = 'const',
    method = AR_METHOD,
    aic=FALSE,
    order.max=lagNum
  )
}


####Parameter extraction functions-----
extractPhi.arest <- function(model){
  tmp <- matrix(0,length(model$ar),length(model$ar))
  diag(tmp) <- model$ar
  phi <- tmp
}

extractPhi.ar <- function(model){
  model$ar
}

extractAIC.ar <- function(model){
  log(det(extractInno(model)))+(2*model$order.max*ncol(model$resid)^2)/nrow(model$resid)
}

extractInno.ar <- function(model){
  return(cov(na.omit(model$resid)))
}

extractInno.arest <- extractInno.ar


####Residual extraction function----
extractResiduals.ar <- function(model){
  model$resid
}

####Error computation function----
computeError.arest <- function(model,pred,dat){
  error <- pracma::rmserr(dat %>% unlist() %>% as.numeric(), pred[[1]] %>% as.numeric())
  sqr_resid <- (dat-pred[[1]])^2
  #sd <- apply(sqr_resid,2,sd)/sqrt(nrow(dat))
  sd <- sd(as.numeric(unlist(sqr_resid)))/sqrt(nrow(dat))
  error<-rlist::list.append(error,sd)
  names(error)[[7]] <- 'sd'
  return(error)
}

####Model prediction function----
altpredict.arest <- function(model,data){
  pred <- matrix(0,nrow(data),ncol(data))
  #se <- matrix(0,n.ahead,ncol(data))
  data <- matrix(unlist(data),ncol=ncol(model$resid))
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
  se <- NULL
  return(list(pred,se))
}





#########SHINY-SPECIFIC----------
ar_sim_server_mod <- function(input, output, session, data, left, right){
output$inno_ar <- renderRHandsontable({
  if(!is.null(updated_inno())){
    rhandsontable(updated_inno())
  }
})

output$loading_matrix <- renderRHandsontable({
  if(!is.null(updated_lm())){
    rhandsontable(updated_lm())
  }
})

output$phi <- renderRHandsontable({
  if(!is.null(updated_phi())){
    rhandsontable(updated_phi())
  }
})
}
####Data simulation shiny module----


ar_sim_ui_mod<-function(id){
  box(
  condition = "input.select_simulation_parameter_origin == 'Manual'",
  numericInput(
    "nDiagPhi",
    "Diagonal PHI:",
    .1,
    min = 0.1,
    max = 1,
    step = 0.1
  ),
  numericInput(
    "nInnoVar",
    "Variance of innovations",
    .01,
    min = 0.01,
    max = 10,
    step = 0.1
  )
  ),
conditionalPanel(
  condition = "input.select_simulation_parameter_origin == 'Manual'",
  numericInput(
    "nOffdiagPhi",
    "Offdiagonal PHI",
    .1,
    min = 0.1,
    max = 1,
    step = 0.1
  ),
  numericInput(
    "nInnoCovar",
    "Covariance of innovations",
    .01,
    min = 0.01,
    max = 10,
    step = 0.1
  )
)
}
  