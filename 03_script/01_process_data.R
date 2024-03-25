# Analysis script

# Author Caitlin Gaskell


#above-ground data for soil core sites
soilabove<-read.table("01_data/psoil.txt",header=T)
head(soilabove,4);dim(soilabove)

#import site data for soil core sites

sdata<-read.csv("01_data/site_data.csv",header=T)
sdata$quadratID<-paste(sdata$transect,sdata$quadrat,sep=".")
#below should be 30
length(unique(sdata$quadratID))
head(sdata,4);dim(sdata)


#import test tray data

tdata<-tdata<-read.csv("01_data/tray_data.csv",header=T)
head(tdata);dim(tdata)

#add quadratID collumn to tray data
tdata$quadratID<-paste(tdata$transect,tdata$quadrat,sep=".")

#import below-ground data for soil core sites (emergence trial data)
specieslist<-read.table("01_data/plant_data.txt",header=T)
which(!is.na(specieslist$species))
head(specieslist,4);dim(specieslist)
#species list for species identified so far 21/03/04
specieslistID<-specieslist[which(!is.na(specieslist$species)),]
head(specieslistID,4);dim(specieslistID)

#plant data for all sites above ground
psabove<-read.table("01_data/plantspeciesabove.txt",header=T)

#check that quadratID's match across 4 datasets
head(soilabove,4);dim(soilabove)
head(sdata,4);dim(sdata)
head(tdata);dim(tdata)
head(specieslist,4);dim(specieslist)
head(specieslistID,4);dim(specieslistID)
head(psabove,4);dim(psabove)

#check soilabove exists in site data
table(soilabove$quadratID %in% sdata$quadratID)
#all tray data exists in site data
table(tdata$quadratID %in% sdata$quadratID)

table(soilabove$sp %in% specieslistID$code)
#all species codes in plant data exist in tray data
table(specieslist$code %in% tdata$code)

