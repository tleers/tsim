var_test_computedata <- function(){
  nVar <- 2
  time <- 10000
  error <- 0
  model <- 'ar'
  
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
  
  test_args<-list(nVar,
                  time,
                  error,
                  model,
                  phi,
                  inno,
                  val=TRUE,
                  burn=1000)
  
  dat<-do.call(computeData,test_args)
  m<-mean(dat)
  print(m)
  plot(1:nrow(dat),dat[,1],type='l')
  plot(1:nrow(dat),dat[,2],type='l')
  if(m<.5){
    return(dat)
  } else {
    return(FALSE)
  }
}

pcvar_test_computedata <- function(){
  nVar <- 2
  time <- 10000
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
  test_args<-list(nVar,
     time,
     error,
     model,
     phi,
     inno,
     val=TRUE,
     burn=1000,
     loading_matrix)
  
  dat<-do.call(computeData,test_args)
  m<-mean(dat)
  print(m)
  plot(1:nrow(dat),dat[,1],type='l')
  plot(1:nrow(dat),dat[,2],type='l')
  if(m<.5){
    return(dat)
  } else {
    return(FALSE)
  }
}

pcvar_test_modeldata<-function(){
  model<-'pcvar'
  dataset<-pcvar_test_computedata()
  lagNum<-1
  index_vars<-NULL
  nComp<-ncol(dataset)
  test_args<-list(
    model, 
    dataset,
    lagNum,
    index_vars = NULL,
    nComp
  )
  mod<-do.call(modelData,test_args)
  return(mod)
}

