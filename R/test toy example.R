# 
# ### Generate toy example according to a dynamic factor model 
# # y(t) = B*f(t) + e(t)
# # f(t) = Phi*f(t-1) + u(t)
# # T -> Toy example data
# # B -> loading matrix
# # F -> Component scores
# # E -> Error scores
# # Phi -> Lagged relations
# # U -> Innovations
# 
# # 1. Load packages + define input variables
# library('MASS')
# library('graphicalVAR')
# nVar <- 6
# nComp <- 3
# nTime <- 500
# error <- .05
# 
# # 2. Create stationary Phi matrix 
# Phi <- matrix(0,nComp,nComp)
# diag(Phi) <- .4 # The diagonal elements
# Phi[diag(1,3)==0] <- .2 # The off-diagonal elements
# # To check for stationarity: Modulus of the eigenvalues < 1
# spectDecomp <- eigen(Phi)
# eigenvalues <- spectDecomp$values
# eigenvalues
# 
# # 3. Generate component scores F, following a VAR(1) process 
# Sigma <- matrix(1,nComp,nComp)
# Sigma[diag(1,3)==0] <- .2
# nIntro <- 1000
# U <- mvrnorm(nTime + nIntro,matrix(0,nComp,1),Sigma)
# F <- matrix(0,nTime + nIntro,nComp)
# F[1,] <- U[1,]
# for (t in 2: (nTime + nIntro)){
#   F[t,] <- F[t-1,] %*% Phi + U[t,]
# }
# F <- F[-(1:nIntro),]
# 
# # 4. Combine component scores F, with loading matrix B and error values E to obtain latent structure
# eye <- diag(1,3) 
# B <- eye[c(1,1,1,2,3,3),] # Loading matrix
# randomErrorValues <- rnorm(nTime * nVar,0,1) 
# randomError <- matrix(randomErrorValues,nTime,nVar)
# E <- sqrt(error) * randomError
# data <- F %*% t(B) + E
# 
# ### Standardization
# # Function to standardize based on the population standard deviation
# pop.var <- function(x) var(x) * (length(x)-1) / length(x) 
# pop.sd <- function(x) sqrt(pop.var(x)) 
# standardize.popsd <- function(dataset){
#   means <- apply(dataset,2,mean)
#   stdevs <- apply(dataset,2,pop.sd)
#   dataCent <- sweep(dataset,2,means)
#   dataSt <- sweep(dataCent,2,stdevs,"/")
#   return(dataSt)
# }
# # Standardize data 
# dataSt <- standardize.popsd(data)
# # First split data into criterion and predictor (i.e., lagged) variables, and then standardize
# X <- head(data,-1)
# Y <- tail(data,-1)
# XSt <- standardize.popsd(X)
# YSt <- standardize.popsd(Y)
# 
# ### Analyses
# 
# library('graphicalVAR')
# library('qgraph')
# library('glmnet')
# library('psych')
# 
# # 1. VAR(1)
# VARfit <- lsfit(XSt,YSt,intercept = FALSE)
# Phi_VAR <- VARfit$coefficients
# 
# # 2. Univariate lasso VAR(1)
# 
# XSt <- as.matrix(XSt)
# YSt <- as.matrix(YSt)
# K <- 10 # The number of folds
# nTime <- nTime - 1
# BlockedCVInd <- as.numeric(factor(sort(rank(1:nTime)%%K)))
# Phi_lassoVAR <- matrix(0,nVar,nVar)
# for (v in 1:nVar){
#   cvfit <- cv.glmnet(XSt, YSt[,v],foldid = BlockedCVInd)
#   TempPhi <- as.matrix(coef(cvfit, s = "lambda.min"))
#   Phi_lassoVAR[,v] <- TempPhi[-1,] # Remove intercept
#   remove(cvfit,TempPhi)
# }
# 
# # 3. Graphical VAR
# res <- graphicalVAR(dataSt)
# PCC <- res$PCC
# PDC <- res$PDC
# 
# # 4. PC-VAR(1)
# PCA_varimax <- principal(dataSt, nfactors = nComp, rotate="varimax")
# F_rot <- PCA_varimax$scores
# B_rot <- PCA_varimax$loadings 
# PCVARfit <- lsfit(head(F_rot,-1),tail(F_rot,-1),intercept = FALSE) # VAR(1) analysis on rotated scores
# Phi_PCVAR <- PCVARfit$coefficients
# 
