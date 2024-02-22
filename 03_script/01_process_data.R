# Analysis script

# Author Caitlin Gaskell
#inport plant full

pfull<-read.table("01_data/plant_full.txt",header=T)
head(pfull,4);dim(pfull)


#import site data

sdata<-read.csv("01_data/site_data.csv",header=T)
head(sdata,4);dim(sdata)

sdata$quadratID<-paste(sdata$transect,sdata$quadrat,sep=".")

#extract from pfull unique quadrats

soilq<-unique(sdata$quadratID)
soilq


#check that soilqs exist in pfull

missingq<-sdata[which(!soilq %in% unique(pfull$quadratID)),]$quadratID

soilq<-soilq[which(!soilq==missingq)]


psoil<-pfull[which(pfull$quadratID %in% soilq),]
head(psoil);dim(psoil)
rownames(psoil)<-1:nrow(psoil)
