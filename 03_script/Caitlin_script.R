#Caitlin's script - script development - no longer need in main script


#import plant full (already imported and subsetted to soil core sites)
#pfull<-read.table("01_data/plant_full.txt",header=T)
#head(pfull,4);dim(pfull)

#extract from pfull unique quadrats

soilq<-unique(sdata$quadratID)
soilq


#check that soilqs exist in pfull

missingq<-sdata[which(!soilq %in% unique(pfull$quadratID)),]$quadratID

soilq<-soilq[which(!soilq==missingq)]


psoil<-pfull[which(pfull$quadratID %in% soilq),]
rownames(psoil)<-1:nrow(psoil)
#above-ground plant data for soil core sites
head(psoil);dim(psoil)

#if need to write table, just run once below:
#write.table(psoil,file="01_data/psoil.txt",sep="\t",quote = F,row.names = F)