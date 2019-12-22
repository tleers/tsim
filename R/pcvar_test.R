nVar <- 2
time <- 100
error <- 0
model <- 'pcvar'

phi <- matrix(0,2,2)
phi[1,1]<-.52
phi[2,2]<-.32
phi[1,2]<-.22
phi[2,1]<-.24

inno <- matrix(0,2,2)
inno[1,1]<-1
inno[2,1]<-.52
inno[2,2]<-1
inno[1,2]<-.52

loading_matrix<-matrix(0,2,2)
diag(loading_matrix)<-1
test_args<-c(nVar,
   time,
   error,
   model,
   phi,
   inno,
   val=TRUE,
   burn=1000,
   loading_matrix)

do.call(computeData.pcvar,as.list(test_args))
