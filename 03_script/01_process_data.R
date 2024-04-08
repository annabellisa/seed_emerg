# Analysis script

# Author Caitlin Gaskell


#t37-46 above-ground data 
AGdata<-read.table("01_data/psoil.txt",header=T)
head(AGdata,4);dim(AGdata)
length(unique(AGdata$sp))
#import site data for soil core sites

#t37-46 site data, add quadratID column
sdata<-read.csv("01_data/site_data.csv",header=T)
sdata$quadratID<-paste(sdata$transect,sdata$quadrat,sep=".")
#below should be 30
length(unique(sdata$quadratID))
head(sdata,4);dim(sdata)

#t37-46 emergence trial tray data, add quadratID column
tdata<-tdata<-read.csv("01_data/tray_data.csv",header=T)
tdata$quadratID<-paste(tdata$transect,tdata$quadrat,sep=".")
head(tdata);dim(tdata)

#t37-46 below-ground species list including morphospecies
BGspec <- combospec[combospec$location == "0" | combospec$location == "2",]
head(BGspec,4);dim(BGspec)
#alt
#BGspec<-read.table("01_data/plant_data.txt",header=T)
#which(!is.na(BGspec$species))


#t37-46 below-ground species list excluding morphospecies (species identified as of 21/03/04)
BGspecid <- combospec[(combospec$location == "0" | combospec$location == "2") & combospec$speciesID == "1",]
head(BGspecid,4);dim(BGspecid)

#alt - smth wrong with combospec$speciesID
#BGspecid<-BGspec[which(!is.na(BGspec$species)),]
#BGspecid <- BGspec[BGspec$speciesID == "1",]


#t01-46 AG species list incl. morphospecies
AGspecall<-read.table("01_data/alltransectspeciesag.txt",header=T)

#t37-46 AG species list incl. morphospecies
AGspec <-AGspecall[AGspecall$location == "1" | AGspecall$location == "2",]
head(AGspec,4);dim(AGspec)

#t37-46 AG species list excl. morphospecies
AGspecid <- AGspecall[(AGspecall$location == "1" | AGspecall$location == "2") & AGspecall$speciesID == "1",]
head(AGspecid,4);dim(AGspecid)

#combined species list of above and below ground, including non identified
combospec<-read.table("01_data/combinedspecies.txt",header=T)
head(combospec,4);dim(combospec)

#check that quadratID's match across 4 datasets
head(AGdata,4);dim(AGdata) #t37-46 AG data
head(sdata,4);dim(sdata) #t37-46 site data
head(tdata);dim(tdata) #t37-46 emergence(BG) data
head(BGspec,4);dim(BGspec) #t37-46 BG species list incl. morphospecies
head(BGspecid,4);dim(BGspecid) #t37-46 BG species list excl. morphospecies
head(AGspecall,4);dim(AGspecall) #t01-46 AG species list incl. morphospecies
head(AGspec,4);dim(AGspec) #t37-46 AG species list incl. morphospecies
head(AGspecid,4);dim(AGspecid) #t37-46 AG species list excl. morphospecies
head(combospec,4);dim(combospec) #t37-46 combined species list of above and below ground, incl morphospecies

#check AGdata exists in site data
table(AGdata$quadratID %in% sdata$quadratID)
#all tray data exists in site data
table(tdata$quadratID %in% sdata$quadratID)

table(AGdata$sp %in% BGspecid$code) #includes duplicates in AGdata
which(!AGdata$sp %in% unique(BGspecid$code))
intersection <- intersect(AGdata$sp, BGspecid$code) # doesn't include duplicates
print(intersection)
#all species codes in plant data exist in tray data
table(BGspec$code %in% tdata$code)
which(!BGspec$code %in% unique(tdata$code))

#which species codes in plant data exist in above-ground veg data
table(BGspecid$code %in% AGdata$sp)
which(BGspecid$code %in% unique(AGdata$sp))
#which species codes in below-ground species list exist in above-ground veg data
BGspecid[which(!BGspecid$code %in% AGdata$sp)[11:21],]
AGdata[which(AGdata$species=="Sonchus oleraceus"),]

#no. species not in below ground data (false)
table(AGspec$sp %in% BGspecid$code)

# VENN diagram:
library(VennDiagram)

# Summarise overlap:
#belowonly<-21+47
#aboveonly<-49+47
#overlap<-47
belowonly <- sum((combospec$location == "0" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
aboveonly <- sum((combospec$location == "1" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
overlap <- sum(combospec$location == "2" & combospec$speciesID == "1")

dev.new(width=6,height=4,dpi=160,pointsize=12, noRStudioGD = T)

par(mar=c(4,4,4,4))

venn.plot<-draw.pairwise.venn(belowonly,aboveonly,overlap, category=c("Below","Above"),scaled=F,fill=rgb(0,0,0,0.5),fontfamily="sans",cat.fontfamily="sans",cex=1, cat.pos=c(2,10),lwd=1)

pdf(file="venn.pdf",width=4,height=4,pointsize=12)

par(mar=c(4,4,1,1))

grid.draw(venn.plot)

dev.off()

#site by species matrix? attempted to use tdata and add species names from combospec, however matrix used codes as collumns - reformatting using pivot_longer returned confusing result
library(reshape)
site.species_matrix <-cast(tdata, quadratID ~ code, value='count', fun.aggregate=sum)
site.species_matrix
named_ssm <- merge(site.species_matrix, combospec, by = "code", all.x = TRUE)
#aforementioned reformatting attempt:
long_ssm <- pivot_longer(site.species_matrix,
                         + cols = quadratID,
                         + names_to = "code",
                         + values_to = "count")
long_ssm
named_ssm <- merge(long_ssm,combospec, by = "code", all.x = TRUE)
print(named_ssm)

#formatting?
#
AG.code <- which(colnames(site.species_matrix) %in% AG)
AG.sm <- site.species_matrix[,c(1,AG.code)]
head(AG.sm[,1:10]);dim(AG.sm)
head(site.species_matrix[,1:10]);dim(site.species_matrix)
#
AG_id.code <- which(colnames(site.species_matrix) %in% AG_id)
AG_id.sm <- site.species_matrix[,c(1,AG_id.code)]
head(AG_id.sm[,1:10]);dim(AG_id.sm)
head(site.species_matrix[,1:10]);dim(site.species_matrix)

#AG
head(psoil); dim(psoil)
length(unique(psoil$quadratID))
AGmat <- as.data.frame.matrix(xtabs(cover~quadratID + sp, data=psoil))
head(AGmat[, 1:10]);dim(AGmat)


#BG ssm
head(combospec); dim(combospec)
head(tdata); dim(tdata)

BGmat <- as.data.frame.matrix(xtabs(count~quadratID + code, data=tdata))
head(BGmat[, 1:10]);dim(BGmat)

#these should all be true:
table(rownames(AGmat) %in% rownames(BGmat))

#constructing 
head(sdata,4);dim(sdata)

div1 <- sdata
head(div1);dim(div1)

#AG species richness
div1$agsr <- apply(AGmat, MARGIN = 1, FUN = function(x) length(which(x > 0)))
xx <- AGmat[, 3]
length(which(xx > 0))

boxplot(div1$agsr ~ div1$burn_trt)

#bgsr, agsimps, bgsimps
#BG species richness
div1$bgsr <- apply(BGmat, MARGIN = 1, FUN = function(x) length(which(x>0)))
xxx <- BGmat[, 1]
length(which(xxx > 0))

bwplot(div1$bgsr ~ div1$burn_trt)

#AG shannon
div1$agshan <- diversity(AGmat, index = "shannon")
div1$agsimp <- diversity(AGmat, index = "invsimpson")

bwplot(div1$agsimp ~ div1$burn_trt)

#BG shannon
div1$bgshan <- diversity(BGmat, index = "shannon")
div1$bgsimp <- diversity(BGmat, index = "invsimpson")

bwplot(div1$bgsimp ~ div1$burn_trt)
#----------------------


#species richness? wait this sums above and below data (1 and 2 or 0 and 2) wait but its based on tray data counts not plant data - so its only BG
AG.id_sr <- aggregate(count ~ species, data = AGmergedID_data, FUN = sum)
BG.id_sr <- aggregate(count ~ species, data = BGmergedID_data, FUN = sum)

#transect richness
transectAG.id_sr <-aggregate(count ~ transect.x + species, data = AGmergedID_data, FUN = sum)
transectBG.id_sr <- aggregate(count ~ transect.x + species, data = BGmergedID_data, FUN = sum)

#cntrl vs burn richness transect
BG.idburn_data <- BGmergedID_data[BGmergedID_data$burn_trt == 'Burn', ]
transburn_BG.id_sr <- aggregate(count ~ transect.x + species, data = BG.idburn_data, FUN = sum)
BG.idcntrl_data <- BGmergedID_data[BGmergedID_data$burn_trt == 'Control', ]
transcntrl_BG.id_sr <- aggregate(count ~ transect.x + species, data = BG.idcntrl_data, FUN = sum)

#quadrat richness
quadratBG.id_sr <- aggregate(count ~ quadratID +species, data = BGmergedID_data, FUN = sum)

quadcntrl_BG.id_sr <- aggregate(count ~ quadratID + species, data = BG.idcntrl_data, FUN = sum)

quadburn_BG.id_sr <- aggregate(count ~ quadratID + species, data = BG.idburn_data, FUN = sum)

#shannon?
library(vegan)
#transect shannon
calc_shannons <- function(data) {
  shannons <-diversity(data, index = "shannon") 
  return (shannons)
  }
shannon_transect <- tapply(transectBG.id_sr$count, transectBG.id_sr$transect.x, calc_shannons)

shannon_df <- data.frame(transect = names(shannon_transect), shannons_diversity = unname(shannon_transect))

print(shannon_df)

#transect burn/trt shannon
burn.shannon_transect <- tapply(transburn_BG.id_sr$count, transburn_BG.id_sr$transect.x, calc_shannons)
transburn.shannon_df <- data.frame(
  transect = names(burn.shannon_transect),
  shannons_diversity = unname(burn.shannon_transect)
)

cntrl.shannon_transect <- tapply(transcntrl_BG.id_sr$count, transcntrl_BG.id_sr$transect.x, calc_shannons)
transcntrl.shannon_df <- data.frame(
  transect = names(cntrl.shannon_transect),
  shannons_diversity = unname(cntrl.shannon_transect)
)

#quadrat shannon id
shannon_quadrat <- tapply(quadratBG.id_sr$count, quadratBG.id_sr$quadratID, calc_shannons)
shannonq_df <- data.frame(quadrat = names(shannon_quadrat), shannons_diversity = unname(shannon_quadrat))
print(shannonq_df)

#quadrat burn shannon id
quadburn_BG.id_sr
burn.shannon_quadrat <- tapply(quadburn_BG.id_sr$count, quadburn_BG.id_sr$quadratID, calc_shannons)
quadburn.shannon_df <- data.frame(
  quadratID = names(burn.shannon_quadrat),
  shannons_diversity = unname(burn.shannon_quadrat)
)

#quadrat cntrl shannon id 
cntrl.shannon_quadrat <- tapply(quadcntrl_BG.id_sr$count, quadcntrl_BG.id_sr$quadratID, calc_shannons)
quadcntrl.shannon_df <- data.frame(
  quadratID = names(cntrl.shannon_quadrat),
  shannons_diversity = unname(cntrl.shannon_quadrat)
)



#sorensen
library(divo)

calc_sorensen <- function(data1, data2) {
  similarity <- li(data1, data2) 
  return(similarity)
  }
#adding burn_trt columns to quadrat species info
quadratBG.id_sr$burn_trt <- ifelse(quadratBG.id_sr$quadratID %in% BG.idburn_data$quadratID, "burn", "control")

quadcntrl_BG.id_sr <- subset(quadratBG.id_sr, burn_trt == "control")
quadburn_BG.id_sr <- subset(quadratBG.id_sr, burn_trt == "burn")

quad_sim <- apply(quadcntrl_BG.id_sr$count, 1, function(cntrl_count) {
  sapply(quadburn_BG.id_sr$count, function(burn_count) {
    calc_sorensen(cntrl_count, burn_count)
  })
})
