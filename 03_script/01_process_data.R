# Analysis script

# Author Caitlin Gaskell


#above-ground data for soil core sites
soilabove<-read.table("01_data/psoil.txt",header=T)
head(soilabove,4);dim(soilabove)

#import site data for soil core sites

sdata<-read.csv("01_data/site_data.csv",header=T)
sdata$quadratID<-paste(sdata$transect,sdata$quadrat,sep=".")
head(sdata,4);dim(sdata)


#import test tray data

tdata<-tdata<-read.csv("01_data/tray_data_test.csv",header=T)
head(tdata);dim(tdata)

#add quadratID collumn to tray data
tdata$quadratID<-paste(tdata$transect,tdata$quadrat,sep=".")

#T45.Q5 
tdata[which(!tdata$quadratID %in% sdata$quadratID),]
unique(soilabove$quadratID)
