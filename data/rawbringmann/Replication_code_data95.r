###############################################################
################# General information #########################
###############################################################
#R-CODE for the manuscript BRINGMANN ET AL. 2016, Assessment
#Title: Assessing temporal emotion dynamics using networks

#The files needed are:
#  replication_code_data95.R
#  replication_code_data95.m
#  data_to_text.m
#  Data95.csv

#%####################################################################%#
### Before you start you have to install and load the libraries below ##
#%####################################################################%#
library("qgraph")#1.3.1

###Please make sure all files are in the same working directory 
##and that you have set your working directory to source file location###
getwd()
ls() #Here you can see which files are in your current working directory

#%###########################################################%#
### Data and estimating a multilevel model in Matlab ##########
#%###########################################################%#
formula<-paste(getwd(),"/Data95.csv",sep="")
Data95<-read.csv(formula,sep=";", na.strings=c("9999","9998"))

#This data is analyzed in matlab. For the multilevel VAR code, see
#Replication_code_data95.m for further instructions.
#After the matlab code has run, you can run this R code.

#%###########################################################%#
### Population network fixed effects: Figure 2 ################
#%###########################################################%#
nv=6 # number of variables
fixedlist=list()
pvalueslist=list()
for (i in 1:nv){
  formula1=paste(getwd(),"/modelfixed",i,".txt",sep="")
  fixed <-as.vector(unlist(read.table(formula1, quote="\"")))
  
  formula2=paste(getwd(),"/modelpvalues",i,".txt",sep="")
  pvalues <-as.vector(unlist(read.table(formula2, quote="\"")))
  #save the innovations as a list
  fixedlist[[i]]<-(fixed)[-1]
  pvalueslist[[i]]<-(pvalues)[-1]
}

Labelz<-c("Angry","Dysphoric","Sad","Anxious","Relaxed","Happy")
FE<-cbind(unlist(fixedlist),unlist(pvalueslist))

E=cbind(from=rep(1:nv,nv),to=rep(1:nv,each=nv),weigth=unlist(FE[,1]))
edge.color <- qgraph:::addTrans(ifelse(FE[,1]>0, "green3", "red3"), ifelse(FE[,2]<0.05, 255, 0))
Network<-qgraph(E, layout="circle", curve = -1, cut = 0.05, esize = 10, 
                edge.color = edge.color,labels=Labelz,title="Dataset 1",lty=ifelse(E[,3]>0,1,5))

#%###########################################################%#
### The individual networks ###################################
#%###########################################################%#
#After you have done the analyses in Matlab, you can load in the edges of the individual networks (fixed effects + random effects).
#In this case you have 36 edges per individual. 

edges=list()
for (i in 1:nv){
  formula=paste(getwd(),"/modelrandom",i,".txt",sep="")
  random <- read.csv(formula, header=FALSE)
  #save the edges as a list
  edges[[i]]<-random[,-1]}
data95edges=matrix(unlist(edges),ncol=(nv*nv)) #The columns are the links in the network (36) and the rows are the individuals (95).

#These are the colnames of the edges of the network. 
#The first variable is the dependent and the second the independent
colnames(data95edges)<-c("angryLangry","angryLdysph","angryLsad","angryLanxious","angryLrelaxed","angryLhappy"," dysphLangry",
                     "dysphLdysph","dysphLsad","dysphLanxious","dysphLrelaxed","dysphLhappy"," sadLangry","sadLdysph",
                     "sadLsad","sadLanxious","sadLrelaxed","sadLhappy"," anxiousLangry","anxiousLdysph","anxiousLsad",
                     "anxiousLanxious","anxiousLrelaxed","anxiousLhappy"," relaxedLangry","relaxedLdysph","relaxedLsad","relaxedLanxious",
                     "relaxedLrelaxed","relaxedLhappy"," happyLangry"," happyLdysph"," happyLsad"," happyLanxious"," happyLrelaxed","happyLhappy")



#%###########################################################%#
### Density: Table 1 ########################################
#%###########################################################%#
overalldensity<-apply(abs(data95edges[,]),1,mean)
negdensityonly<-apply(abs(data95edges[,c(1:4,7:10,13:16,19:22)]),1,mean)
posdensityonly<-apply(abs(data95edges[,c(29,30,35,36)]),1,mean)

#Correlate the density results with neuroticism
cor.test(posdensityonly,Data95[1:95,15])
cor.test(negdensityonly,Data95[1:95,15])
cor.test(overalldensity,Data95[1:95,15])


#%###########################################################%#
### Centrality: Table 2 until 5 ########################################
#%###########################################################%#

Labelz<-c("Angry","Dysphoric","Sad","Anxious","Relaxed","Happy")
CENlist=list()
for (PP in 1:95){
  E=cbind(from=rep(1:nv,nv),to=rep(1:nv,each=nv),weigth=as.numeric(data95edges[PP,1:36]))
  E=as.matrix(E)
   CENlist[[PP]]<- cbind(Labelz,centrality_auto(E)$node.centrality)
  
}

even_indexes<-seq(2,8,2)
odd_indexes<-seq(1,8,2)
Centrality95<-matrix(NA,nv,8)
output=matrix(NA,nv,95)
rownames(output)<-Labelz
colnames(Centrality95)<-c("Betweenness","p","Closeness","p","InStrength","p","OutStrength","p")
rownames(Centrality95)<-Labelz
for(i in 1:4){
  for (PP in 1:95){
    output[,PP]=CENlist[[PP]][,1+i]
    
  }
  
  for (j in 1:nv){
    #Correlate the centrality results with neuroticism
  
    Centrality95[j,odd_indexes[i]]<-cor(output[j,],Data95[1:95,15])
    Centrality95[j,even_indexes[i]]<-round(cor.test(output[j,],Data95[1:95,15])$p.value,4)
  }
}

Centrality95<-round(Centrality95,3)
Centrality95


#%###########################################################%#
### Self loops: Table 6 ########################################
#%###########################################################%#
angdensityself<-abs(data95edges[,c(1)])
depdensityself<-abs(data95edges[,c(8)])
anxdensityself<-abs(data95edges[,c(22)])
saddensityself<-abs(data95edges[,c(15)])

reldensityself<-abs(data95edges[,c(29)])
hapdensityself<-abs(data95edges[,c(36)])

#Correlate the self loops with neuroticism
cor.test(angdensityself,Data95[1:95,15]) 
cor.test(depdensityself,Data95[1:95,15])
cor.test(anxdensityself,Data95[1:95,15])
cor.test(saddensityself,Data95[1:95,15])

cor.test(reldensityself,Data95[1:95,15])
cor.test(hapdensityself,Data95[1:95,15])


