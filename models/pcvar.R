

#VAR(p) implementation based on Bulteel et al. 2018, modified by Tim Leers


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
                              phi,
                              inno,
                              loading_matrix,
                              ....){
  #Generate errors
  innovations <- rmvnorm(time+burn,rep(0,nVar),inno)
  
  #Create empty matrix
  U <- matrix(innovations, time + burn, nVar)
  simdata <- matrix(0, time + burn, nVar)
  simdata[1, ] <- U[1, ]
  
  for (row in 2:(time + burn)) {
    simdata[row, ] = phi %*% simdata[(row - 1), ] + U[row, ]
  }
  randomError <- matrix(rnorm(time * nVar, 0, 1), time, nVar)
  E <- sqrt(error) * randomError
  Y <- simdata[-(1:burn), ] 
  # 4. Combine component scores F, with loading matrix B and error values E to obtain latent structure
  Y <- Y %*% t(loading_matrix) + E # Y is the toy data
  return(Y)
}

####Model fit function----
modelData.pcvar <- function(model, dataset,lagNum,index_vars = NULL, ncomp=ncol(dataset)) {
  require(psych)
  PCA_varimax<-principal(dataset,
                         nfactors=nComp,
                         rotate="varimax")
  F_rot<-PCA_varimax$scores
  B_rot<-PCA_varimax$loadings
  PCVARfit <- lsfit(head(F_rot,-1),tail(F_rot,-1),intercept = FALSE) # VAR(1) analysis on rotated scores
  return(PCVARfit)
}

####Parameter extraction functions-----
extractPhi.pcvar <- function(model){
  return(model$coefficients)
}

extractInno.pcvar <- function(model){
  return(cov(na.omit(extractResiduals.pcvar)))
}

####Residual extraction function----
extractResiduals.pcvar <- function(model){
  return(model$residuals)
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
