# Analysis script: post-fire soil seedbank dynamics 

# Author: Caitlin Gaskell & Annabel Smith

# ---- libraries
library(VennDiagram)
library(lme4)
library(vegan)
library(AICcmodavg)
library(lmerTest)
library(ecodist)
library(ape)
library(scales)
library(adespatial)
library(ade4)
library(glmmADMB)

# ---- load workspace:
load("04_workspaces/seedbank_analysis.RData")

# Load functions:
invisible(lapply(paste("02_functions/",dir("02_functions"),sep=""), function(x) source(x)))


# ---- Import data ----

#combined species list of above and below ground, including non identified
combospec<-read.table("01_data/combinedspecies.txt",header=T)
head(combospec,4);dim(combospec)

#t37-46 above-ground data 
AGdata<-read.table("01_data/psoil.txt",header=T)
AGdata$code <- AGdata$sp
AGdata <- subset(AGdata, select = -sp)
AGdata <- AGdata[!(AGdata$code %in% c("Rock", "Litter", "Bare")), ]
head(AGdata,4);dim(AGdata)
length(unique(AGdata$code))

#import site data for soil core sites

#t37-46 site data, add quadratID column
sdata<-read.csv("01_data/site_data.csv",header=T)
sdata$quadratID<-paste(sdata$transect,sdata$quadrat,sep=".")
head(sdata,4);dim(sdata)
#below should be 30
length(unique(sdata$quadratID))

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
#write.csv(BGspecid, file = "01_data/BGspecid.csv", row.names = FALSE)
#alt
#BGspecid<-BGspec[which(!is.na(BGspec$species)),]
#BGspecid <- BGspec[BGspec$speciesID == "1",]

#t01-46 AG species list incl. morphospecies
AGspecall<-read.table("01_data/alltransectspeciesag.txt",header=T)
head(AGspecall);dim(AGspecall)
#AGspecall <- combospec[(combospec$location == "1" | combospec4location == "2")]

#t37-46 AG species list incl. morphospecies
#AGspec <-AGspecall[AGspecall$location == "1" | AGspecall$location == "2",]
AGspec <-combospec[combospec$location == "1" | combospec$location == "2",]
head(AGspec,4);dim(AGspec)

#t37-46 AG species list excl. morphospecies
#AGspecid <- AGspecall[(AGspecall$location == "1" | AGspecall$location == "2") & AGspecall$speciesID == "1",]
#AGspecid <- combospec[(combospec$location == "1") | combospec$location == "2" & combospec$speciesID == "1", ]
AGspecid <- combospec[(combospec$location %in% c("1", "2")) & combospec$speciesID == "1", ]
head(AGspecid,4);dim(AGspecid) 

table(AGspecid$code %in% AGdata$code)
which(!AGspecid$code %in% unique(AGdata$code))

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

#---- checking data
#discrepancy between AGdata (psoil) and species list (combospec) - 15 less species in psoil
print(AGspecid$code[which(!AGspecid$code %in% unique(AGdata$code))])
print(AGdata$code)
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

# ----

# ---- VENN diagram ----

# above/below species occurrence VENN diagram:

# Summarise overlap:
belowonly <- sum((combospec$location == "0" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
aboveonly <- sum((combospec$location == "1" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
overlap <- sum(combospec$location == "2" & combospec$speciesID == "1")

# plot
dev.new(width=4,height=4,dpi=120,pointsize=16, noRStudioGD = T)
par(mar=c(0,4,4,4))
venn.plot<-draw.pairwise.venn(belowonly,aboveonly,overlap, inverted = F, category=c("Below ground\nseedbank","Above ground\nvegetation"),scaled=F,fill=rgb(0,0,0,0.2),fontfamily="sans",cat.fontfamily="sans",cex=1.5, lwd=1, cat.pos=c(342,18),cat.dist=c(0.07 ,0.07), cat.cex=rep(1.2,2), cat.just=list(c(0, 0.4), c(1,0.4)))

pdf(file="Fig2_Venn.pdf",width=4,height=4,pointsize=16)
par(mar=c(4,4,1,1))
grid.draw(venn.plot)
dev.off()

# ----

# ---- Define functional groups ----
head(BGspecid,4);dim(BGspecid)
head(AGspecid,4);dim(AGspecid)

# analyse 13 groups:

# all
# native
# exotic
# annual
# perennial
# forb
# grass
# native grasses
# exotic grasses
# native forb
# exotic forb
# non-leguminous forb
# leguminous forb

# Below ground

BG.native <- BGspecid$code[which(BGspecid$origin == "Native")]
BG.exotic <- BGspecid$code[which(BGspecid$origin == "Exotic")]
BG.annual <- BGspecid$code[which(BGspecid$life_span == "Annual" | BGspecid$life_span == "Biennial" | BGspecid$life_span == "Annual/Biennial")]
BG.perr <- BGspecid$code[which(BGspecid$life_span == "Perennial")]
BG.forb <- BGspecid$code[which(BGspecid$form == "Herb")]
BG.grass <- BGspecid$code[which(BGspecid$form == "Grass")]
BG.native_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Native")]
BG.exotic_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Exotic")]
BG.native_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$origin == "Native")]
BG.exotic_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$origin == "Exotic")]
BG.leg_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$legume == "1")]
BG.nonleg_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$legume == "0")]

# Not analysed
# BG.tree <- BGspecid$code[which(BGspecid$form == "Tree")]
# BG.shrub <- BGspecid$code[which(BGspecid$form == "Shrub")]
# BG.sedge <- BGspecid$code[which(BGspecid$form == "Sedge/Rush")]
# BG.leg <- BGspecid$code[which(BGspecid$legume == 1)]


# Above ground
AG.native <- AGspecid$code[which(AGspecid$origin == "Native")]
AG.exotic <- AGspecid$code[which(AGspecid$origin == "Exotic")]
AG.annual <- AGspecid$code[which(AGspecid$life_span == "Annual" | AGspecid$life_span == "Biennial")]
AG.perr <- AGspecid$code[which(AGspecid$life_span == "Perennial")]
AG.forb <- AGspecid$code[which(AGspecid$form == "Herb" | AGspecid$form == "Vine")]
AG.grass <- AGspecid$code[which(AGspecid$form == "Grass")]
AG.native_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Native")]
AG.exotic_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Exotic")]
AG.native_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$origin == "Native")]
AG.exotic_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$origin == "Exotic")]
AG.leg_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$legume == "1")]
AG.nonleg_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$legume == "0")]

# Not analysed
# AG.leg <- AGspecid$code[which(AGspecid$legume == 1)]
# AG.tree <- AGspecid$code[which(AGspecid$form == "Tree")]
# AG.shrub <- AGspecid$code[which(AGspecid$form == "Shrub")]
# AG.sedge <- AGspecid$code[which(AGspecid$form == "Sedge/Rush")]

# put AGshrub back in group when 5x5 data added
# "AG.shrub", "AG.leg", "BG.leg"
# adding AG.shrub back into group.df returns dim(X) must have a positive length when running for loop?
# make a df of functional groups:
group.df <- data.frame(group = c("BG.native","BG.exotic","BG.annual","BG.perr", "BG.leg", "BG.tree","BG.shrub","BG.forb","BG.grass","BG.sedge","BG.native_grass", "BG.exotic_grass", "BG.native_forb", "BG.exotic_forb", "BG.leg_forb", "BG.nonleg_forb", "AG.native","AG.exotic","AG.annual","AG.perr","AG.leg", "AG.tree","AG.shrub","AG.forb","AG.grass","AG.sedge", "AG.native_grass","AG.exotic_grass","AG.native_forb", "AG.exotic_forb", "AG.leg_forb", "AG.nonleg_forb"))

# ----

# ---- Site x species matrix ----

#AG ssm
head(AGdata); dim(AGdata)
length(unique(AGdata$quadratID))
AGmat <- as.data.frame.matrix(xtabs(cover~quadratID + code, data=AGdata))
head(AGmat[, 1:10]);dim(AGmat)

#BG ssm
head(combospec); dim(combospec)
head(tdata); dim(tdata)
#tdataiD
head(BGspecid,4);dim(BGspecid)

tdataID <- tdata[which(tdata$code %in% BGspecid$code), ]
head(tdataID,4);dim(tdataID)

length(unique(tdataID$code))

BGmat <- as.data.frame.matrix(xtabs(count~quadratID + code, data=tdataID))
head(BGmat[, 1:10]);dim(BGmat)

which(duplicated(colnames(BGmat)))
colnames(BGmat)

#these should all be true:
table(rownames(AGmat) %in% rownames(BGmat))

# ----

# ---- Species richness: functional groups ----

rich.data<-list()
simp.data<-list()

head(AGmat); dim(AGmat)
head(BGmat); dim(BGmat)

for (i in 1:nrow(group.df)){
  
  name.thisrun<-as.character(group.df$group[i])
  vec.thisrun<-get(name.thisrun)
  
  if(substr(name.thisrun,1,3)=="BG.") data.thisrun<-BGmat[,colnames(BGmat) %in% vec.thisrun]
  if(substr(name.thisrun,1,3)=="AG.") data.thisrun<-AGmat[,colnames(AGmat) %in% vec.thisrun]
  head(data.thisrun); dim(data.thisrun)
  
  if(is.null(dim(data.thisrun))) {
    if(substr(name.thisrun,1,3)=="AG.") rownames.thisrun<-rownames(AGmat) else rownames.thisrun <- rownames(BGmat)
    data.thisrun <- data.frame(data.thisrun) 
  colnames(data.thisrun) <- vec.thisrun
  rownames(data.thisrun) <- rownames.thisrun
  } # close if only one species
  
  rich.data[[i]]<-apply(data.thisrun,1,function(x)length(which(x>0)))
  
  #simp.data[[i]]<-diversity(data.thisrun,index="invsimpson")
  
} # close i for

rich.res<-data.frame(do.call(cbind,rich.data))
colnames(rich.res)<-group.df$group

# All species richness
div1 <- sdata

#AG species richness
div1$agsr <- apply(AGmat, MARGIN = 1, FUN = function(x) length(which(x > 0)))

#BG species richness
div1$bgsr <- apply(BGmat, MARGIN = 1, FUN = function(x) length(which(x>0)))

head(div1);dim(div1)
head(rich.res); dim(rich.res)
quadratID_order <- div1$quadratID[match(rownames(rich.res), div1$quadratID)]
div1$quadratID <- quadratID_order
rownames(rich.res) == div1$quadratID

rich.res <- data.frame(quadratID = rownames(rich.res),rich.res)

cnt_rich<-merge(div1,rich.res, by="quadratID", all.x = T, all.y = F)

head(cnt_rich,3); dim(cnt_rich)
head(group.df)

colnames(cnt_rich)[which(colnames(cnt_rich) %in% c("agsr","bgsr"))] <- c("AG.all", "BG.all")

AG.all <- colnames(AGmat)
BG.all <- colnames(BGmat)

group.df <- data.frame(group=c("AG.all", "BG.all", group.df$group))

# Summarise the number of species in each group:
group.df$no_species<-NA
for (i in 1:nrow(group.df)){
  var.thisrun<-as.character(group.df$group[i])
  group.df$no_species[i]<-length(get(var.thisrun))
}

# Add labels to group data
gdf<-group.df
gdf

gdf$ylab<-c("AG all","BG all","BG Native","BG Exotic","BG Annual","BG Perennial","BG Legume","BG Tree","BG Shrub","BG Forb","BG Grass","BG Sedge","BG Native Grass","BG Exotic Grass", "BG Native Forb", "BG Exotic Forb", "BG Leguminous Forb", "BG Non-leguminous Forb", "AG Native","AG Exotic","AG Annual","AG Perennial","AG Legume","AG Tree","AG Shrub","AG Forb","AG Grass","AG Sedge","AG Native Grass","AG Exotic Grass", "AG Native Forb", "AG Exotic Forb", "AG Leguminous Forb", "AG Non-leguminous Forb")

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Species richness: data set-up  ----

# div4 & div5
div2 <- cnt_rich[, 1:5]
head(div2);dim(div2)

div3<-rbind(div2,div2)
div3$ab<-c(rep("above",30),rep("below",30))
head(div3);dim(div3)

head(cnt_rich,3); dim(cnt_rich)

AG.sr<-cnt_rich[,grep("AG.",colnames(cnt_rich))]
AG.sr<-data.frame(AG.sr[,which(colnames(AG.sr)=="AG.all"):which(colnames(AG.sr)=="AG.tree")],AG.shrub=rep(0,30),AG.sr[,which(colnames(AG.sr)=="AG.forb"):which(colnames(AG.sr)=="AG.nonleg_forb")])

colnames(AG.sr)<-substr(colnames(AG.sr),4,nchar(colnames(AG.sr)))

BG.sr<-cnt_rich[,grep("BG.",colnames(cnt_rich))]
colnames(BG.sr)<-substr(colnames(BG.sr),4,nchar(colnames(BG.sr)))

head(AG.sr,3); dim(AG.sr)
head(BG.sr,3); dim(BG.sr)

length(colnames(AG.sr))
length(colnames(BG.sr))

# should all be true
colnames(AG.sr)==colnames(BG.sr)

all.sr<-rbind(AG.sr,BG.sr)
head(all.sr,3); dim(all.sr)

# factorise explanatory variables
div4<-cbind(div3,all.sr)
head(div4,3); dim(div4)
div4$burn_trt <- factor(div4$burn_trt, levels = c("Control","Burn"))
div4$ab <- factor(div4$ab, levels = c("above","below"))

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Species richness: modelling  ----

#---- SR GLMERs: all interactions insignificant and removed; additive models used as final models

# Species richness (alpha diversity)

# All species
srmod_all.int<-glmer(all~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_all.int)

srmod_all<-glmer(all~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_all)

# native
srmod_nat.int<-glmer(native~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_nat.int)

srmod_nat<-glmer(native~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_nat)

# exotic
srmod_exo.int<-glmer(exotic~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exo.int)

srmod_exo<-glmer(exotic~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exo)

# annual
srmod_ann.int<-glmer(annual~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_ann.int)

srmod_ann<-glmer(annual~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_ann)

# perennial
srmod_per.int<-glmer(perr~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_per.int)

srmod_per<-glmer(perr~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_per)

# forb

srmod_for.int<-glmer(forb~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_for.int)

srmod_for<-glmer(forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_for)

# grass

srmod_gra.int<-glmer(grass~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_gra.int)

srmod_gra<-glmer(grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_gra)

# native_grass

srmod_natgra.int<-glmer(native_grass~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_natgra.int)

srmod_natgra<-glmer(native_grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_natgra)

# exotic_grass

srmod_exogra.int<-glmer(exotic_grass~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exogra.int)

srmod_exogra<-glmer(exotic_grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exogra)

# native_forb
srmod_natfor.int<-glmer(native_forb~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_natfor.int)

srmod_natfor<-glmer(native_forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_natfor)

# exotic_forb
srmod_exofor.int<-glmer(exotic_forb~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exofor.int)

srmod_exofor<-glmer(exotic_forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exofor)

# leg_forb
srmod_legfor.int<-glmer(leg_forb~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_legfor.int)

srmod_legfor<-glmer(leg_forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_legfor)

# nonleg_forb
srmod_nonlegfor.int<-glmer(nonleg_forb~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_nonlegfor.int)

srmod_nonlegfor<-glmer(nonleg_forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_nonlegfor)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Species richness: model estimates ----

# ND for all models

nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))

# all species 
srmod_all1 <- predictSE(mod=srmod_all,newdata=nd1,type="response",se.fit = T)
srmod_all1 <- data.frame(nd1, fit = srmod_all1$fit, se = srmod_all1$se.fit)
srmod_all1$lci <- srmod_all1$fit-(srmod_all1$se*1.96)
srmod_all1$uci <- srmod_all1$fit+(srmod_all1$se*1.96)
head(srmod_all1)

# native 
srmod_nat1 <- predictSE(mod=srmod_nat,newdata=nd1,type="response",se.fit = T)
srmod_nat1 <- data.frame(nd1, fit = srmod_nat1$fit, se = srmod_nat1$se.fit)
srmod_nat1$lci <- srmod_nat1$fit-(srmod_nat1$se*1.96)
srmod_nat1$uci <- srmod_nat1$fit+(srmod_nat1$se*1.96)
head(srmod_nat1)

# exotic 
srmod_exo1 <- predictSE(mod=srmod_exo,newdata=nd1,type="response",se.fit = T)
srmod_exo1 <- data.frame(nd1, fit = srmod_exo1$fit, se = srmod_exo1$se.fit)
srmod_exo1$lci <- srmod_exo1$fit-(srmod_exo1$se*1.96)
srmod_exo1$uci <- srmod_exo1$fit+(srmod_exo1$se*1.96)
head(srmod_exo1)

# annual
srmod_ann1 <- predictSE(mod=srmod_ann,newdata=nd1,type="response",se.fit = T)
srmod_ann1 <- data.frame(nd1, fit = srmod_ann1$fit, se = srmod_ann1$se.fit)
srmod_ann1$lci <- srmod_ann1$fit-(srmod_ann1$se*1.96)
srmod_ann1$uci <- srmod_ann1$fit+(srmod_ann1$se*1.96)
head(srmod_ann1)

# perennial
srmod_per1 <- predictSE(mod=srmod_per,newdata=nd1,type="response",se.fit = T)
srmod_per1 <- data.frame(nd1, fit = srmod_per1$fit, se = srmod_per1$se.fit)
srmod_per1$lci <- srmod_per1$fit-(srmod_per1$se*1.96)
srmod_per1$uci <- srmod_per1$fit+(srmod_per1$se*1.96)
head(srmod_per1)

# forb
srmod_for1 <- predictSE(mod=srmod_for,newdata=nd1,type="response",se.fit = T)
srmod_for1 <- data.frame(nd1, fit = srmod_for1$fit, se = srmod_for1$se.fit)
srmod_for1$lci <- srmod_for1$fit-(srmod_for1$se*1.96)
srmod_for1$uci <- srmod_for1$fit+(srmod_for1$se*1.96)
head(srmod_for1)

# grass
srmod_gra1 <- predictSE(mod=srmod_gra,newdata=nd1,type="response",se.fit = T)
srmod_gra1 <- data.frame(nd1, fit = srmod_gra1$fit, se = srmod_gra1$se.fit)
srmod_gra1$lci <- srmod_gra1$fit-(srmod_gra1$se*1.96)
srmod_gra1$uci <- srmod_gra1$fit+(srmod_gra1$se*1.96)
head(srmod_gra1)

# native grass
srmod_natgra1 <- predictSE(mod=srmod_natgra,newdata=nd1,type="response",se.fit = T)
srmod_natgra1 <- data.frame(nd1, fit = srmod_natgra1$fit, se = srmod_natgra1$se.fit)
srmod_natgra1$lci <- srmod_natgra1$fit-(srmod_natgra1$se*1.96)
srmod_natgra1$uci <- srmod_natgra1$fit+(srmod_natgra1$se*1.96)
head(srmod_natgra1)

# exotic grass
srmod_exogra1 <- predictSE(mod=srmod_exogra,newdata=nd1,type="response",se.fit = T)
srmod_exogra1 <- data.frame(nd1, fit = srmod_exogra1$fit, se = srmod_exogra1$se.fit)
srmod_exogra1$lci <- srmod_exogra1$fit-(srmod_exogra1$se*1.96)
srmod_exogra1$uci <- srmod_exogra1$fit+(srmod_exogra1$se*1.96)
head(srmod_exogra1)

# native forb
srmod_natfor1 <- predictSE(mod=srmod_natfor,newdata=nd1,type="response",se.fit = T)
srmod_natfor1 <- data.frame(nd1, fit = srmod_natfor1$fit, se = srmod_natfor1$se.fit)
srmod_natfor1$lci <- srmod_natfor1$fit-(srmod_natfor1$se*1.96)
srmod_natfor1$uci <- srmod_natfor1$fit+(srmod_natfor1$se*1.96)
head(srmod_natfor1)

# exotic forb
srmod_exofor1 <- predictSE(mod=srmod_exofor,newdata=nd1,type="response",se.fit = T)
srmod_exofor1 <- data.frame(nd1, fit = srmod_exofor1$fit, se = srmod_exofor1$se.fit)
srmod_exofor1$lci <- srmod_exofor1$fit-(srmod_exofor1$se*1.96)
srmod_exofor1$uci <- srmod_exofor1$fit+(srmod_exofor1$se*1.96)
head(srmod_exofor1)

# leguminous forb
srmod_legfor1 <- predictSE(mod=srmod_legfor,newdata=nd1,type="response",se.fit = T)
srmod_legfor1 <- data.frame(nd1, fit = srmod_legfor1$fit, se = srmod_legfor1$se.fit)
srmod_legfor1$lci <- srmod_legfor1$fit-(srmod_legfor1$se*1.96)
srmod_legfor1$uci <- srmod_legfor1$fit+(srmod_legfor1$se*1.96)
head(srmod_legfor1)

# non-leguminous forb
srmod_nonlegfor1 <- predictSE(mod=srmod_nonlegfor,newdata=nd1,type="response",se.fit = T)
srmod_nonlegfor1 <- data.frame(nd1, fit = srmod_nonlegfor1$fit, se = srmod_nonlegfor1$se.fit)
srmod_nonlegfor1$lci <- srmod_nonlegfor1$fit-(srmod_nonlegfor1$se*1.96)
srmod_nonlegfor1$uci <- srmod_nonlegfor1$fit+(srmod_nonlegfor1$se*1.96)
head(srmod_nonlegfor1)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Species richness: plot estimates ----

# April 2025 update
# Make new plot with total species richness only for the main document (because the responses are all the same). Remove total from the multi-group figure and put the multi-group in the SI. 

# Use x_labels2 and x_lab_at for Main Doc; x_labels for SI
x_labels2 <- c("above", "below", "above", "below")
x_labels <- c("AG", "BG", "AG", "BG")
x_lab_at<-c(0.8,1.9,3.1,4.2)

# define vectors to jitter raw data:
head(div4,2); dim(div4)

all.c.ab<-div4$all[div4$burn_trt=="Control" & div4$ab=="above"]
all.c.bl<-div4$all[div4$burn_trt=="Control" & div4$ab=="below"]
all.b.ab<-div4$all[div4$burn_trt=="Burn" & div4$ab=="above"]
all.b.bl<-div4$all[div4$burn_trt=="Burn" & div4$ab=="below"]

raw.lim<-c(all.c.ab,all.c.bl,all.b.ab,all.b.bl)


dev.new(width=5,height=3.5,dpi=120,pointsize=16, noRStudioGD = T)
par(mar=c(3.5,4,0.5,1), mgp=c(2.4,1,0), oma=c(0,0,0,4))

# all sr
plot(c(1:4), srmod_all1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(c(srmod_all1$lci,raw.lim))), max(c(srmod_all1$uci,raw.lim))), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")

axis(side=1, at=x_lab_at, labels=x_labels2, tick=F, cex.axis=0.8, mgp=c(2,0.5,0))
axis(side=1, at=1:4, labels=NA, tick=T, cex.axis=0.8, mgp=c(2,0.5,0))
title(xlab="Position",mgp=c(2,1,0))
title(main = "", line = 0.5,adj=0, cex.main=0.95)

points(jitter(rep(1,length(all.c.ab)),factor=4),all.c.ab,col=alpha("chartreuse4",0.5), pch=20, cex=0.5)
points(jitter(rep(2,length(all.c.bl)),factor=4),all.c.bl,col=alpha("chartreuse4",0.5), pch=20, cex=0.5)
points(jitter(rep(3,length(all.b.ab)),factor=4),all.b.ab,col=alpha("orange",0.5), pch=20, cex=0.5)
points(jitter(rep(4,length(all.b.bl)),factor=4),all.b.bl,col=alpha("orange",0.5), pch=20, cex=0.5)

arrows(c(1:4), srmod_all1$lci, c(1:4), srmod_all1$uci, length=0.05, code=3, angle=90)
points(c(1:4), srmod_all1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

par(xpd=NA)
legend(4.8,33, legend=c("Control", "Burn"), col = c("chartreuse4", "orange"),pch=c(20, 20), cex = (1),pt.cex=2, title = NULL,bty="n")
par(xpd=F)

# Functional groups species richness for SI:

head(srmod_all1)
head(srmod_nat1)
head(srmod_exo1)
head(srmod_ann1)
head(srmod_per1)
head(srmod_tree1)
head(srmod_shr1)
head(srmod_for1)
head(srmod_gra1)
head(srmod_sed1)
head(srmod_natgra1)
head(srmod_exogra1)

gdf

dev.new(width=9,height=10,dpi=60,pointsize=18, noRStudioGD = T)
par(mfrow=c(4,3),mar=c(3.5,4,1.5,1), mgp=c(2.4,1,0), oma=c(0,0,0,6))

# native
plot(c(1:4), srmod_nat1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nat1$lci)), max(srmod_nat1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_nat1$lci, c(1:4), srmod_nat1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(a) Native", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_nat1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# exotic
plot(c(1:4), srmod_exo1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exo1$lci)), max(srmod_exo1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exo1$lci, c(1:4), srmod_exo1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(b) Exotic", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_exo1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# annual
plot(c(1:4), srmod_ann1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_ann1$lci)), max(srmod_ann1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_ann1$lci, c(1:4), srmod_ann1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(c) Annual", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_ann1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

par(xpd=NA)
legend(5.2,8.2, legend=c("Control", "Burn"), col = c("chartreuse4", "orange"),pch=c(20, 20), cex = (1),pt.cex=2, title = NULL,bty="o")
par(xpd=F)

# perennial
plot(c(1:4), srmod_per1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_per1$lci)), max(srmod_per1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_per1$lci, c(1:4), srmod_per1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(d) Perennial", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_per1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# forb
plot(c(1:4), srmod_for1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_for1$lci)), max(srmod_for1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_for1$lci, c(1:4), srmod_for1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(e) Forb", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_for1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# grass
plot(c(1:4), srmod_gra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_gra1$lci)), max(srmod_gra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_gra1$lci, c(1:4), srmod_gra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(f) Grass", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_gra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# native grass
plot(c(1:4), srmod_natgra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natgra1$lci)), max(srmod_natgra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_natgra1$lci, c(1:4), srmod_natgra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(g) Native Grass", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_natgra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# exotic grass
plot(c(1:4), srmod_exogra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exogra1$lci)), max(srmod_exogra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exogra1$lci, c(1:4), srmod_exogra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(h) Exotic Grass", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_exogra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# native forbs
plot(c(1:4), srmod_natfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natfor1$lci)), max(srmod_natfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_natfor1$lci, c(1:4), srmod_natfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(i) Native Forb", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_natfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# exotic forbs
plot(c(1:4), srmod_exofor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exofor1$lci)), max(srmod_exofor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_exofor1$lci, c(1:4), srmod_exofor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(j) Exotic Forb", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_exofor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# non-leguminous forbs
plot(c(1:4), srmod_legfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nonlegfor1$lci)), max(srmod_nonlegfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_nonlegfor1$lci, c(1:4), srmod_nonlegfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(k) Non-leguminous Forb", line = 0.5,adj=0, cex.main=0.9, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_nonlegfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# leguminous forbs
plot(c(1:4), srmod_legfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_legfor1$lci)), max(srmod_legfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_legfor1$lci, c(1:4), srmod_legfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
title(main = "(l) Leguminous Forb", line = 0.5,adj=0, cex.main=0.95, font.main=1)
title(xlab="Position",mgp=c(1.8,1,0))
points(c(1:4), srmod_legfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Site x species matrix for all species, above and below (n=117) ----

# ssm construction - creating an above and below matrix with 117 columns and 30 rows to merge into one.
AGmat2 <- AGmat
rownames(AGmat2) <- paste0(rownames(AGmat), ".above")

BGmat2 <- BGmat
BGmat2 <- as.matrix(BGmat2)
rownames(BGmat2) <- paste0(rownames(BGmat), ".below")

uniqueAG <- setdiff(colnames(AGmat), colnames(BGmat))
uniqueBG <- setdiff(colnames(BGmat), colnames(AGmat))
commonall <- intersect(colnames(AGmat), colnames(BGmat))

cols_all1 <- c(uniqueAG, commonall, uniqueBG)
rownames_ag <- c(rownames(AGmat2))
rownames_bg <- c(rownames(BGmat2))

# ALLmat ag and bg
ALLmat.ag <- matrix(0, nrow = length(rownames_ag), ncol = length(cols_all1))
ALLmat.ag<-as.data.frame(ALLmat.ag)
colnames(ALLmat.ag) <- cols_all1
rownames(ALLmat.ag) <- rownames_ag
ALLmat.ag[1:nrow(AGmat2), colnames(AGmat2)] <- AGmat2
head(ALLmat.ag,3); dim(ALLmat.ag)

ALLmat.bg <- matrix(0, nrow = length(rownames_bg), ncol = length(cols_all1))
colnames(ALLmat.bg) <- cols_all1
rownames(ALLmat.bg) <- rownames_bg
ALLmat.bg[1:nrow(BGmat2), colnames(BGmat2)] <- BGmat2
head(ALLmat.bg,3); dim(ALLmat.bg)

# these should all be true
table(colnames(ALLmat.ag)==colnames(ALLmat.bg))

# ALLmat - combined above/below ssm
ALLmat <- rbind(ALLmat.ag, ALLmat.bg)

# change counts (bg) and covers (ag) to presence/absence 
div6 <- ALLmat
div6[div6 > 0] <- 1
head(div6[,1:10],4);dim(div6)

head(AGmat2[,1:10],3); dim(AGmat2)
head(BGmat2[,1:10],3); dim(BGmat2)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# ---- PCA ----

# Update 15 Jan 2026
# Note the defaults in prcomp: (center = TRUE, scale. = FALSE), so we have not scaled the variables. 

# Review Comment from R1: "Principal Components Analysis assumes that species responses to the underlying ecological gradients are linear, which is not always true for ecological data. An alternative that does not require an assumption of linearity is non-metric multidimensional scaling (NMDS). There are ways to check for homogeneity (and thus suitability for PCA; here is one source: https://www.davidzeleny.net/anadat-r/doku.php/en:ordination). Please justify your choice of PCA as a suitable approach for this project."

# The Zeleny source recommends: "calculate DCA (detrended by segments) on your data, and check the length of the first DCA axis (which is scaled in units of standard deviation, S.D.). The length of first DCA axis > 4 S.D. indicates a heterogeneous dataset on which unimodal methods should be used, while the length < 3 S.D. indicates a homogeneous dataset for which linear methods are suitable."

# One explanation for homogeneity from this source is that their: "dataset contains relatively homogenous species composition, since the sampled grassland patches all belong to the same or very closely related vegetation types, which makes this dataset suitable for PCA."

# DCA can be done in vegan with decorana()

dca1<-decorana(div6)
dca1

# The length of the first DCA is < 4 (2.8558), so we apparently have homogeneous data, meaning PCA is suitable for this data set. 

# This is the original PCA script from the first submission:
pca1 <- prcomp(div6)
pca1
summary(pca1)
pov1<-summary(pca1)$importance[2,]
head(div6);dim(div6)

#40% of variance explained by 3 components
cum_var <- cumsum(pca1$sdev^2)/sum(pca1$sdev^2)
sum(cum_var <= 0.4)
cum_var[1:3]

pcadata <- data.frame(quadratID = div6[,1])
pcadata <- data.frame(quadratID = rownames(div6), row.names = NULL)
head(pcadata,3); dim(pcadata)

pcadata$pca.comp1<-pca1$x[,1]
pcadata$pca.comp2<-pca1$x[,2]
pcadata$pca.comp3<-pca1$x[,3]
head(pca1$x[1:3])
head(pca1$rotation[1:3])
head(pca1);dim(pca1)

pca1$rotation[,1]
pca1$rotation[,2]
pca1$rotation[,3]
summary(pca1)

nd2 <- div4[,1:6]
head(nd2);dim(nd2)
head(div4);dim(div4)
nd2$quadratID2 <- paste(nd2$quadratID,nd2$ab, sep=".")

pcadata2 <- merge(pcadata,nd2, by.x = "quadratID", by.y = "quadratID2", all.x = T, all.y = F)
head(pcadata2);dim(pcadata2)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Model Principal Components components 1-3 ----
 
# comp1
comp1_lmer.int<-lmer(pca.comp1~ab*burn_trt+(1|transect), data=pcadata2)
summary(comp1_lmer.int)

comp1_lmer<-lmer(pca.comp1~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp1_lmer)

# comp2
comp2_lmer.int<-lmer(pca.comp2~ab*burn_trt+(1|transect), data=pcadata2)
summary(comp2_lmer.int)

comp2_lmer<-lmer(pca.comp2~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp2_lmer)

# comp3
comp3_lmer.int<-lmer(pca.comp3~ab*burn_trt+(1|transect), data=pcadata2)
summary(comp3_lmer.int)

comp3_lmer<-lmer(pca.comp3~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp3_lmer)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# plot PCA ----
shapes<-c(15,17)
shapes<-shapes[as.factor(pcadata2$ab)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcadata2$burn_trt)]

dev.new(height=6,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.2,0.8,0), oma=c(0,0,0,7))

# scree plot
plot(x=1:length(pov1),y=pov1,ylab="",xlab="Components",type="p", las=1)
lines(x=1:length(pov1),y=pov1)
title(ylab="Propotion Variance Explained", mgp=c(3,1,0))
mtext("(a)",3,0.7,F,0)

#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, choices=c(3, 4))
#mtext("(b)",3,0.7,F,adj = 0)

# 1 vs 2
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=shapes, xlab="PC1",ylab="PC2",cex=2,col=alpha(col.1,1), las=1)
mtext("(b)",3,0.4,F,adj=0)
# text(x=c(-0.61888015), y=c(-7.32836938), labels= c("T26_20"), pos=4)
# text(x=c(14.84107197), y=c(-0.54987242), labels= c("T06_57"), pos=2)

# Add legend
par(xpd=NA)
legend(3.5,2, legend=c("Control Above", "Control Below", "Burnt Above", "Burnt Below"), col = c("grey60", "grey60", "grey20", "grey20"),pch=c(15, 17, 15, 17))
par(xpd=F)

# 1 vs 3
plot(pcadata2$pca.comp1,pcadata2$pca.comp3,pch=shapes, xlab="PC1",ylab="PC3",cex=2,col=alpha(col.1,1), las=1)
mtext("(c)",3,0.4,F,adj=0)
# text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
# text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

# 2 vs 3
plot(pcadata2$pca.comp2,pcadata2$pca.comp3,pch=shapes, xlab="PC2",ylab="PC3",cex=2,col=alpha(col.1,1), las=1)
mtext("(d)",3,0.4,F,adj=0)
# text(x=c(0.69979374), y=c(-6.582940182), labels= c("T10_06"), pos=2)
# text(x=c(-7.32836938), y=c(1.416516483), labels= c("T26_20"), pos=4)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Explore distance-based ordination ----

# Is nMDS better for presence / absence data? According to Bolker on this page, yes. 
# https://stats.stackexchange.com/questions/623005/can-i-conduct-a-pca-on-binary-presence-absence-data

# However, the nMDS doesn't have 'axes' like a PCA and can not be used in downstream analyses:
# https://www.researchgate.net/post/Can_you_use_NMDS_site_scores_in_a_regression

# So let's have a look and see how the results compare to PCA
# The figure below shows the nMDS produces a very similar outcome to PCA: almost all of the dissimilarity is captured by the above/below differences, and very little by the burn treatment. As with the PCA, the first nMDS axis is capturing almost all of the dissimilarity between above and below ground, with the other two axes contributing little. 

head(div6);dim(div6)

mds1<-metaMDS(div6, distance = "bray", k=4, trymax = 50)
str(mds1)
plot(mds1, xlim=c(-2,2))

# Extract relevant data:
mds.dat<-data.frame(mds1$points)
mds.dat$quadratID2<-rownames(mds.dat)
table(rownames(mds.dat) %in% nd2$quadratID2)

rownames(mds.dat)<-1:nrow(mds.dat)
head(nd2);dim(nd2)
head(mds.dat,3); dim(mds.dat)

mds.dat1<-merge(nd2, mds.dat, by="quadratID2")

# MDS sites:
head(mds.dat1,3); dim(mds.dat1)

mds.dat.sp<-data.frame(mds1$species)
head(mds.dat.sp,3); dim(mds.dat.sp)

shapes2<-c(15,17)
shapes2<-shapes2[as.factor(mds.dat1$ab)]
col.2<-c("grey60","grey20")
col.2<-col.1[as.factor(mds.dat1$burn_trt)]

dev.new(height=6,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.2,0.8,0), oma=c(0,0,0,7))

# 1 vs 2
plot(mds.dat.sp$MDS1, mds.dat.sp$MDS2, pch=3,col="red", xlab="NMDS1",ylab="NMDS2",las=1)
points(mds.dat1$MDS1,mds.dat1$MDS2,pch=shapes,col=alpha(col.1,1),cex=1.5)
mtext("(a)",3,0.4,F,adj=0)

# 1 vs 3
plot(mds.dat.sp$MDS1, mds.dat.sp$MDS3, pch=3,col="red", xlab="NMDS1",ylab="NMDS3",las=1)
points(mds.dat1$MDS1,mds.dat1$MDS3,pch=shapes,col=alpha(col.1,1),cex=1.5)
mtext("(b)",3,0.4,F,adj=0)

# Add legend
par(xpd=NA)
legend(1,2, legend=c("Control Above", "Control Below", "Burnt Above", "Burnt Below", "Species"), col = c("grey60", "grey60", "grey20", "grey20","red"),pch=c(c(15, 17, 15, 17,3)))
par(xpd=F)

# 2 vs 3
plot(mds.dat.sp$MDS2, mds.dat.sp$MDS3, pch=3,col="red", xlab="NMDS2",ylab="NMDS3",las=1)
points(mds.dat1$MDS2,mds.dat1$MDS3,pch=shapes,col=alpha(col.1,1),cex=1.5)
mtext("(c)",3,0.4,F,adj=0)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Beta diversity: data set-up ----

# 2026 revision

# R1: "For evaluating beta diversity, I encourage you to consider some more complex beta diversity indices (see Pondani and Schmera 2011 Oikos, and subsequent papers). You could also partition beta diversity into nestedness and turnover components (nestedness = low diversity plots being subsets of high diversity plots; turnover = changes in species identity; see Baselga 2010 Global Ecology and Biogeography). That partitioning can provide additional insight into the biological mechanisms driving changes in species composition. These metrics can be calculated with the adespatial R package."

# Whittaker's beta diversity is problematic anyway. 

# Ricotta & Burrascano (2009) explain "...since Whittaker's βW summarizes the turnover in species composition of a given set of plots with a single scalar, it cannot be used for testing for differences in beta diversity among different sets of plots."

# Legendre et al. (2005) talk about 'levels of abstraction', and that when the interest is in beta diversity between 'groups of sites' distance based approaches are necessary. Raw data based approaches are suitable for "studying variation of community composition among sites".

# wrt partitioning, Podani and Schmera (2011) and Baselga (2010) methods are implementable inadespatial

head(div6);dim(div6)
head(div4);dim(div4)
head(AGmat[,1:10]);dim(AGmat)
head(BGmat[,1:10]);dim(BGmat)

# above and below combined:
head(div6);dim(div6)
comp.all<-beta.div.comp(div6,coef = "J")
comp.all$part

# above:
comp.ag<-beta.div.comp(AGmat,coef = "J")
head(AGmat[,1:10]);dim(AGmat)
comp.ag$part

# below:
comp.bg<-beta.div.comp(BGmat,coef = "J")
head(BGmat[,1:10]);dim(BGmat)
comp.bg$part

# triangle.plot order is left, bottom, right

# put them in the same order as Podani and Schmera, and other implementations of the method (i.e. left = rich diff; bottom=similarity; right = replacement). 

# Extract components - above ground
repl.ag <- comp.ag$repl
rich.ag <- comp.ag$rich
# Podani & Schmera use similarity (S=1-D)
sim.ag <- 1 - comp.ag$D
tri.ag <- data.frame(cbind(rich.ag, sim.ag, repl.ag))
table(rowSums(tri.ag))
head(tri.ag,3); dim(tri.ag)

# Extract components - below ground
repl.bg <- comp.bg$repl
rich.bg <- comp.bg$rich
# Podani & Schmera use similarity (S=1-D)
sim.bg <- 1 - comp.bg$D
tri.bg <- data.frame(cbind(rich.bg, sim.bg, repl.bg))
table(rowSums(tri.bg))
head(tri.bg,3); dim(tri.bg)

# Extract components - all plots
repl.all <- comp.all$repl
rich.all <- comp.all$rich
# Podani & Schmera use similarity (S=1-D)
sim.all <- 1 - comp.all$D
tri.all <- data.frame(cbind(rich.all, sim.all, repl.all))
table(rowSums(tri.all))
head(tri.all,3); dim(tri.all)

# Adding labels manually to control parameters, but they can be checked by adding the default: labeltriangle = T

dev.new(width=9,height=3,dpi=60, pointsize=18, noRStudioGD = T)
par(mfrow=c(1,3),mgp=c(2.2,1,0), mar=c(4,6,4,6),oma=c(0,0,0,0))

triangle.plot(tri.ag,scale=F, show.position = F, labeltriangle = F)

text(-0.45,0.25,labels="richness difference", col="black", srt=60)
text(0,-0.52,labels="similarity", col="black", srt=0)
text(0.45,0.25,labels="replacement", col="black", srt=300)

mtext("(a) above ground", side=3, line=2.5, adj=0.5, cex=0.8)

triangle.plot(tri.bg,scale=F, show.position = F, labeltriangle = F)

text(-0.45,0.25,labels="richness difference", col="black", srt=60)
text(0,-0.52,labels="similarity", col="black", srt=0)
text(0.45,0.25,labels="replacement", col="black", srt=300)

mtext("(b) below ground", side=3, line=2.5, adj=0.5, cex=0.8)

triangle.plot(tri.all,scale=F, show.position = F, labeltriangle = F)

text(-0.45,0.25,labels="richness difference", col="black", srt=60)
text(0,-0.52,labels="similarity", col="black", srt=0)
text(0.45,0.25,labels="replacement", col="black", srt=300)

mtext("(c) above and below", side=3, line=2.5, adj=0.5, cex=0.8)

# Re-calculate beta diversity using a distance-based method (beta.div adespatial)

# Originally, we calculated beta diversity separately for each position (i.e. gamma was total species richness under ground and above ground, separately). Burn treatments were combined. But actually this is not appropriate. Cornell and Harrison (2014, AREE) provide a definition: "The species pool can be defined as all the species present in a region that can disperse to a focal locality regardless of their ability to tolerate the prevailing environmental conditions"

# The MS has been updated to reflect the biogeographic context of our study: "The regional species pool (γ) was defined as total species richness across all 60 quadrats, including both levels of the fire treatment (control, burn) and position (above, below). This definition of γ is suitable for identifying biotic and abiotic filters that shape patterns of community assembly (such as species losses and gains under different management regimes) at spatial scales where dispersal is possible (Cornell & Harrison 2014). This allowed γ to reflect that seed from the above ground community becomes available to the seedbank below ground, while species in the seedbank are available to recruit above ground."

# Redefine functional groups:

# gdf, group.df and gdf2 all have groups that we are not analysing
head(gdf)

gdf2<-gdf
gdf2$gr<-substr(gdf2$group,start = unlist(gregexpr("[.]",gdf2$group))+1,stop=nchar(gdf2$group))
head(gdf2)

# Make group level df for functional groups, using only the 13 functional groups included in the paper:

gr.df<-data.frame(group=c("all","native","exotic","annual" ,"perr" ,"forb" ,"grass","native_grass","exotic_grass","native_forb","exotic_forb","leg_forb","nonleg_forb"))

ag.groups<-paste("AG.",gr.df$group[2:length(gr.df$group)], sep="")
bg.groups<-paste("BG.",gr.df$group[2:length(gr.df$group)], sep="")

gr.df$AG.n<-c(NA,unlist(lapply(ag.groups, FUN=function(x) length(get(x)))))
gr.df$BG.n<-c(NA,unlist(lapply(bg.groups, FUN=function(x) length(get(x)))))
gr.df$AG.n[1]<-ncol(AGmat)
gr.df$BG.n[1]<-ncol(BGmat)

# Add total species richness:
gr.df$all<-NA
for(i in 1:length(ag.groups)){
  group.thisurn<-c(ag.groups[i],bg.groups[i])
  gr.df$all[i+1]<-length(unique(unlist(lapply(group.thisurn,function(x)get(x)))))
  }
# write.table(gr.df, file="out.txt", sep="\t", row.names = F, quote=F)

# This is the output from the original Whittaker definition:
head(div4,3);dim(div4)

# These are the quadrat x species matrices:
head(div6[,1:10]);dim(div6)
head(AGmat[,1:10]);dim(AGmat)
head(BGmat[,1:10]);dim(BGmat)

# Make output files to store new values

ag.out<-data.frame(quadratID=div2$quadratID)
above.out<-ag.out
below.out<-ag.out
above.out$ab<-factor("above", levels=c("above","below"))
below.out$ab<-factor("below", levels=c("above","below"))

both.out<-bd.dat[,c("quadratID2", "ab")]
both.out$ab<-as.factor(both.out$ab)

head(above.out); dim(above.out)
head(below.out); dim(below.out)
head(both.out); dim(both.out)

# Make data file for new analysis:
head(div2);dim(div2)
head(div3);dim(div3)
bd.dat<-div3

# Put final data in bd.dat
bd.dat$quadratID2<-paste(bd.dat$quadratID,bd.dat$ab,sep=".")
head(bd.dat,3);dim(bd.dat)

# Calculate beta diversity for all functional groups using the 117 quadrat by species matrix

head(div6[,1:10]);dim(div6)
head(AGmat[,1:10], 3);dim(AGmat)
head(BGmat[,1:10],3);dim(BGmat)

head(gdf2); dim(gdf2)

gr.toanalyse<-unique(gr.df$group)

bd.outlist<-list()

# some were not running because old groups that were not included were still in the list. This is now running with the updated the list of groups. 

# However, some are returning NaN, even though the parameters are the same, because these include quadrats with no species:
# 4 annual; 9 exotic_grass; 11 exotic_forb; 12 leg_forb
# I added an if to deal with this in the loop and merge back to the main df. 

for (i in 1:length(gr.toanalyse)){
  
  gr.thisrun<-gr.toanalyse[i]
  
  lines.thisrun<-which(gdf2$gr %in% gr.thisrun)
  groups.thisrun<-gdf2$group[lines.thisrun]
  
  ag.thisrun<-get(groups.thisrun[grep("AG",groups.thisrun)])
  bg.thisrun<-get(groups.thisrun[grep("BG",groups.thisrun)])
  
  both.thisrun<-unique(c(ag.thisrun, bg.thisrun))
  
  # ag.mat<-AGmat[,ag.thisrun]
  # bg.mat<-BGmat[,bg.thisrun]
  
  both.mat<-div6[,both.thisrun]

  head(both.mat,3);dim(both.mat)
  
  if (length(which(rowSums(both.mat)==0))>0) both.mat<-both.mat[-which(rowSums(both.mat)==0),]
  
  # table(rowSums(both.mat)) # sites
  # table(colSums(both.mat)) # species
  
  # ag.obj<-beta.div(ag.mat, method = "jaccard")
  # bg.obj<-beta.div(bg.mat, method = "jaccard")
  both.obj<-beta.div(both.mat, method="jaccard")
  
  # bd.ag<-data.frame(quadratID=names(ag.obj$LCBD),bd=ag.obj$LCBD)
  # bd.bg<-data.frame(quadratID=names(bg.obj$LCBD),bd=bg.obj$LCBD)
  bd.both<-data.frame(quadratID2=names(both.obj$LCBD),bd=both.obj$LCBD)
  
  # rownames(bd.ag)<-1:nrow(bd.ag)
  # rownames(bd.bg)<-1:nrow(bd.bg)
  rownames(bd.both)<-1:nrow(bd.both)
  
  head(bd.both); dim(bd.both)
  head(both.out); dim(both.out)
  
  # ag.dat<-merge(above.out, bd.ag, by="quadratID")
  # bg.dat<-merge(below.out, bd.bg, by="quadratID")
  both.dat<-merge(both.out, bd.both, all.x=T, all.y=F, by="quadratID2")
  both.dat<-both.dat[order(both.dat$ab, both.dat$quadratID2),]
  rownames(both.dat)<-1:nrow(both.dat)
  
  # dat.sep<-rbind(ag.dat, bg.dat)
  # colnames(dat.sep)[grep("bd", colnames(dat.sep))]<-gr.thisrun
  colnames(both.dat)[grep("bd", colnames(both.dat))]<-gr.thisrun
  
  head(both.dat); dim(both.dat)
  
  bd.outlist[[i]]<-both.dat
  
} # close bd 

head(bd.outlist[[13]])

bd.data<-do.call(cbind,bd.outlist)
head(bd.data, 3); dim(bd.data)

# If all of these are TRUE, then the quadrat and position (ab) names are all in the correct order after cbind, and the extra ones can simply be removed:
table(apply(bd.data[,grep("quadratID2", colnames(bd.data))],2,function(x) x==x))
table(apply(bd.data[,grep("ab", colnames(bd.data))],2,function(x) x==x))

quad.remove<-grep("quadratID2", colnames(bd.data))
bd.data<-bd.data[,-quad.remove[2:length(quad.remove)]]

ab.remove<-grep("ab", colnames(bd.data))
bd.data<-bd.data[,-ab.remove[2:length(quad.remove)]]

# the new data needs be structured like div4 for modelling
head(bd.data, 3); dim(bd.data)
head(div4, 3); dim(div4) # nrow==60

# div3 is the base
div7<-div3
div7$quadratID2<-paste(div7$quadratID, div7$ab, sep=".")
div7$ab<-NULL

div7<-merge(div7, bd.data, by="quadratID2", all.x=T, all.y=F)
div7<-div7[order(div7$ab, div7$quadratID2),]
rownames(div7)<-1:nrow(div7)
head(div7);dim(div7)

div7$burn_trt<-as.factor(div7$burn_trt)
div7$transect<-as.factor(div7$transect)

# Save all species output and explore the difference between calculating beta diversity separately above and below, vs the whole matrix together (these have been saved in the workspace and blanked in the loop): 

all.gamma.separate<-dat.sep # calculated separately
all.gamma.together<-both.dat # calculated together
head(all.gamma.separate); dim(all.gamma.separate) 
head(all.gamma.together); dim(all.gamma.together)

dev.new(width=9,height=4,dpi=60, pointsize=18, noRStudioGD = T)
par(mfrow=c(1,2),mgp=c(2.2,1,0), mar=c(4,5,1,1),oma=c(0,0,0,0), font.main=1)

plot(all.gamma.separate$ab, all.gamma.separate$all, las=1, xlab="position", ylab="", main="All species (y separate)", font.main=1, cex.main=0.9)
title(ylab="beta diversity", mgp=c(3.5,1,0))
plot(all.gamma.together$ab, all.gamma.together$all, las=1, xlab="position", ylab="", main="All species (y together)", font.main=1, cex.main=0.9)
title(ylab="beta diversity", mgp=c(3.5,1,0))

# save.image("04_workspaces/seedbank_analysis.RData")

# Plot new raw beta values:

dev.new(width=7,height=10,dpi=60,pointsize=18, noRStudioGD = T)
par(mfrow=c(5,3),mar=c(3.5,4,1,1), mgp=c(2,0.5,0), oma=c(0,0,0,0))

head(bd.data, 3); dim(bd.data)
head(gr.df); dim(gr.df)

gr.toplot<-colnames(bd.data)[3:ncol(bd.data)]

for (i in 1:length(gr.toplot)){
  gr.thisrun<-gr.toplot[i]
  plot(bd.data$ab, bd.data[,gr.thisrun], las=1, xlab="", ylab="", main=gr.thisrun, font.main=1, cex.main=0.9, cex.axis=0.8)
}

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Beta diversity: fit models & predict ----

head(div7,3);dim(div7)
head(gr.df); dim(gr.df)

# Manually add y labels

gr.df$ylab<-c("All species","Native", "Exotic", "Annual", "Perennial", "Forb", "Grass", "Native Grass", "Exotic Grass", "Native Forb", "Exotic Forb", "Leguminous Forb", "Non-leguminous Forb")

# The data are positive numeric. 
# Positive, continuous, numeric, non-integer, bounded by zero. 
range(div7[,which(colnames(div7)=="all"):ncol(div7)], na.rm=T)
# hist(unlist(div7[,which(colnames(div7)=="all"):ncol(div7)]))

# So this should be a gamma distribution, which can be stably implemented in glmmadmb with random effects

beta.mod<-list()
beta.sum<-list()
beta.coef<-list()
beta.pred<-list()

gr.df$beta.int.p<-NA
gr.df$beta.add.fire.p<-NA
gr.df$beta.add.position.p<-NA

beta.groups<-gr.df$group
head(div7,3);dim(div7)

for (i in 1:length(beta.groups)){
  
  group.thisrun<-beta.groups[i]
  dat.thisrun<-div7[,c("burn_trt","transect", "ab",group.thisrun)]
  head(dat.thisrun); dim(dat.thisrun)
  
  if(length(which(is.na(dat.thisrun[,group.thisrun])))>0){
    dat.thisrun<-dat.thisrun[-which(is.na(dat.thisrun[,group.thisrun])),]
    dat.thisrun<-tidy.df(dat.thisrun)
    }
  
  form.int.thisrun<-paste(group.thisrun,"~ab*burn_trt+(1|transect)",sep="")
  mod.int.thisrun<-glmmadmb(formula=as.formula(form.int.thisrun), family="gamma", data=dat.thisrun)
  
  form.add.thisrun<-paste(group.thisrun,"~ab+burn_trt+(1|transect)",sep="")
  mod.add.thisrun<-glmmadmb(formula=as.formula(form.add.thisrun), family="gamma", data=dat.thisrun)
  
  # Interaction P value: if the P value is < 0.05 the interaction model is better: 
  int.coef<-data.frame(summary(mod.int.thisrun)$coefficients)
  int.p<-int.coef[grep(":", rownames(int.coef)),grep("Pr", colnames(int.coef))]
  gr.df$beta.int.p[i]<-int.p
  
  if(int.p>0.05){
  add.coef<-data.frame(summary(mod.add.thisrun)$coefficients)
  gr.df$beta.add.fire.p[i]<-add.coef[grep("burn", rownames(add.coef)),grep("Pr", colnames(add.coef))]
  gr.df$beta.add.position.p[i]<-add.coef[grep("abbelow", rownames(add.coef)),grep("Pr", colnames(add.coef))]
}
 
   if(int.p<0.05) mod.thisrun<-mod.int.thisrun else mod.thisrun<-mod.add.thisrun
  
  beta.mod[[i]]<-mod.thisrun 
  beta.sum[[i]]<-summary(mod.thisrun)
  beta.coef[[i]]<-summary(mod.thisrun)$coefficients
  
  pred.thisrun<-pred(model=mod.thisrun, new.data = nd1, se.fit = T)
  
  beta.pred[[i]]<-pred.thisrun
  
} # beta analysis

head(gr.beta)

# save.image("04_Workspaces/processed_data.RData")

# ----

# Beta diversity: plot estimates ----

# June 2025: make plot for beta diversity, showing the variation for total, native, exotic, annual and perennial for the main document (to show the variation in responses). Plot all other functional groups with additive effects in the SI. 

# Jan 2026: update with estimates from distance based beta diversity:

dev.new(width=8,height=10,dpi=60,pointsize=18, noRStudioGD = T)
par(mfrow=c(5,3),mar=c(3.5,4,1.5,1), mgp=c(2.2,1,0), oma=c(0,0,0,0))

for (i in 1:nrow(gr.df)){
  
  gr.thisrun<-gr.df$group[i]
  beta.pred.thisrun<-beta.pred[[i]]
  plot(c(1:4), beta.pred.thisrun$fit.resp, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(c(beta.pred.thisrun$lci.resp))), max(c(beta.pred.thisrun$uci.resp))), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
  title(xlab="Position", mgp=c(1.8,1,0))
  
  # Raw data jitters:
  
  raw.thisrun<-div7[,c(1:which(colnames(div7)=="ab"),which(colnames(div7)==gr.thisrun))]
  head(raw.thisrun,3); dim(raw.thisrun)
  
  b.c.ab<-raw.thisrun[raw.thisrun$burn_trt=="Control" & raw.thisrun$ab=="above",which(colnames(raw.thisrun)==gr.thisrun)]
  b.c.bl<-raw.thisrun[raw.thisrun$burn_trt=="Control" & raw.thisrun$ab=="below",which(colnames(raw.thisrun)==gr.thisrun)]
  b.b.ab<-raw.thisrun[raw.thisrun$burn_trt=="Burn" & raw.thisrun$ab=="above",which(colnames(raw.thisrun)==gr.thisrun)]
  b.b.bl<-raw.thisrun[raw.thisrun$burn_trt=="Burn" & raw.thisrun$ab=="below",which(colnames(raw.thisrun)==gr.thisrun)]
  b.raw.lim<-c(b.c.ab,b.c.bl,b.b.ab,b.b.bl)
  
  points(jitter(rep(1,length(b.c.ab)),factor=4),b.c.ab,col=alpha("chartreuse4",0.5), pch=20, cex=0.5)
  points(jitter(rep(2,length(b.c.bl)),factor=4),b.c.bl,col=alpha("chartreuse4",0.5), pch=20, cex=0.5)
  points(jitter(rep(3,length(b.b.ab)),factor=4),b.b.ab,col=alpha("orange",0.5), pch=20, cex=0.5)
  points(jitter(rep(4,length(b.b.bl)),factor=4),b.b.bl,col=alpha("orange",0.5), pch=20, cex=0.5)
  
  # Model estimates
  arrows(c(1:4), beta.pred.thisrun$lci.resp, c(1:4), beta.pred.thisrun$uci.resp, length=0.05, code=3, angle=90)
  axis(side=1, at=1:4, labels=x_labels2, tick=T, cex.axis=0.8, mgp=c(3,0.5,0))
  title(main = paste("(a) ",gr.df$ylab[i],sep=""), line = 0.5,adj=0, cex.main=0.95, font.main=1)
  points(c(1:4), beta.pred.thisrun$fit.resp,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)
  
}

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

