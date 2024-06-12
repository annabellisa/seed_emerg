# Analysis script

# Author Caitlin Gaskell

# ---- libraries
library(VennDiagram)
library(lme4)
library(vegan)
library(abdiv)
library(divo)
library(AICcmodavg)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lmerTest)
library(ecodist)
library(ape)

# ---- load workspace:
load("04_workspaces/seedbank_analysis.RData")

# ---- import data ----

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
dev.new(width=4,height=4,dpi=160,pointsize=12, noRStudioGD = T)
par(mar=c(4,4,4,4))
venn.plot<-draw.pairwise.venn(belowonly,aboveonly,overlap, category=c("Below ground\nseedbank","Above ground\nvegetation"),scaled=F,fill=rgb(0,0,0,0.2),fontfamily="sans",cat.fontfamily="sans",cex=1, cat.pos=c(10,-10), cat.dist=c(0.05,0.05),lwd=1)

pdf(file="venn.pdf",width=4,height=4,pointsize=12)
par(mar=c(4,4,1,1))
grid.draw(venn.plot)
dev.off()
# ----

# ---- define functional groups ----
head(BGspecid,4);dim(BGspecid)
head(AGspecid,4);dim(AGspecid)

#groups to analyse:
#all
#native
#exotic
#annual
#perennial
#tree
#shrub
#forb
#grass
#sedges
#exotic grasses
#native grasses

# Below ground

BG.native <- BGspecid$code[which(BGspecid$origin == "Native")]
BG.exotic <- BGspecid$code[which(BGspecid$origin == "Exotic")]
BG.annual <- BGspecid$code[which(BGspecid$life_span == "Annual" | BGspecid$life_span == "Biennial" | BGspecid$life_span == "Annual/Biennial")]
BG.perr <- BGspecid$code[which(BGspecid$life_span == "Perennial")]
BG.leg <- BGspecid$code[which(BGspecid$form == "Legume")]
BG.tree <- BGspecid$code[which(BGspecid$form == "Tree")]
BG.shrub <- BGspecid$code[which(BGspecid$form == "Shrub")]
BG.forb <- BGspecid$code[which(BGspecid$form == "Herb")]
BG.grass <- BGspecid$code[which(BGspecid$form == "Grass")]
BG.sedge <- BGspecid$code[which(BGspecid$form == "Sedge/Rush")]
BG.native_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Native")]
BG.exotic_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Exotic")]
BG.native_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$origin == "Native")]
BG.exotic_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$origin == "Exotic")]
BG.leg_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$legume == "1")]
BG.nonleg_forb <- BGspecid$code[which(BGspecid$form == "Herb" & BGspecid$legume == "0")]

# Above ground
AG.native <- AGspecid$code[which(AGspecid$origin == "Native")]
AG.exotic <- AGspecid$code[which(AGspecid$origin == "Exotic")]
AG.annual <- AGspecid$code[which(AGspecid$life_span == "Annual" | AGspecid$life_span == "Biennial")]
AG.perr <- AGspecid$code[which(AGspecid$life_span == "Perennial")]
AG.leg <- AGspecid$code[which(AGspecid$form == "Legume")]
AG.tree <- AGspecid$code[which(AGspecid$form == "Tree")]
AG.shrub <- AGspecid$code[which(AGspecid$form == "Shrub")]
AG.forb <- AGspecid$code[which(AGspecid$form == "Herb" | AGspecid$form == "Vine")]
AG.grass <- AGspecid$code[which(AGspecid$form == "Grass")]
AG.sedge <- AGspecid$code[which(AGspecid$form == "Sedge/Rush")]
AG.native_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Native")]
AG.exotic_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Exotic")]
AG.native_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$origin == "Native")]
AG.exotic_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$origin == "Exotic")]
AG.leg_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$legume == "1")]
AG.nonleg_forb <- AGspecid$code[which(AGspecid$form == "Herb" & AGspecid$legume == "0")]

#put AGshrub back in group when 5x5 data added
#"AG.shrub", "AG.leg", "BG.leg"
#adding AG.shrub back into group.df returns dim(X) must have a positive length when running for loop?
#make a df of functional groups:
group.df <- data.frame(group = c("BG.native","BG.exotic","BG.annual","BG.perr", "BG.leg", "BG.tree","BG.shrub","BG.forb","BG.grass","BG.sedge","BG.native_grass", "BG.exotic_grass", "BG.native_forb", "BG.exotic_forb", "BG.leg_forb", "BG.nonleg_forb", "AG.native","AG.exotic","AG.annual","AG.perr","AG.leg", "AG.tree","AG.shrub","AG.forb","AG.grass","AG.sedge", "AG.native_grass","AG.exotic_grass","AG.native_forb", "AG.exotic_forb", "AG.leg_forb", "AG.nonleg_forb"))

# ----

# ---- site x species matrix ----

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

# ---- species richness of functional groups ----

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

# prepare data for analysis ----

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

# model species richness ----

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

# legume (response is constant)
srmod_leg1.int<-glmer(leg~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_leg1.int)

srmod_leg<-glmer(leg~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_leg)

# tree #no isSingular error?
srmod_tree.int<-glmer(tree~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_tree.int)

srmod_tree<-glmer(tree~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_tree)

# shrub #negative lci?
srmod_shr.int<-glmer(shrub~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_shr.int)

srmod_shr<-glmer(shrub~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_shr)

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

# sedge

srmod_sed.int<-glmer(sedge~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_sed.int)

srmod_sed<-glmer(sedge~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_sed)

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

# model estimates: species richness ----

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

#legume
#srmod_leg1 <- predictSE(mod=srmod_leg,newdata=nd1,type="response",se.fit = T)
#srmod_leg1 <- data.frame(nd1, fit = srmod_leg1$fit, se = srmod_leg1$se.fit)
#srmod_leg1$lci <- srmod_leg1$fit-(srmod_leg1$se*1.96)
#srmod_leg1$uci <- srmod_leg1$fit+(srmod_leg1$se*1.96)
#head(srmod_leg1)

# tree
srmod_tree1 <- predictSE(mod=srmod_tree,newdata=nd1,type="response",se.fit = T)
srmod_tree1 <- data.frame(nd1, fit = srmod_tree1$fit, se = srmod_tree1$se.fit)
srmod_tree1$lci <- srmod_tree1$fit-(srmod_tree1$se*1.96)
srmod_tree1$uci <- srmod_tree1$fit+(srmod_tree1$se*1.96)
head(srmod_tree1)

# shrub
srmod_shr1 <- predictSE(mod=srmod_shr,newdata=nd1,type="response",se.fit = T)
srmod_shr1 <- data.frame(nd1, fit = srmod_shr1$fit, se = srmod_shr1$se.fit)
srmod_shr1$lci <- srmod_shr1$fit-(srmod_shr1$se*1.96)
srmod_shr1$uci <- srmod_shr1$fit+(srmod_shr1$se*1.96)
head(srmod_shr1)

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

# sedge
srmod_sed1 <- predictSE(mod=srmod_sed,newdata=nd1,type="response",se.fit = T)
srmod_sed1 <- data.frame(nd1, fit = srmod_sed1$fit, se = srmod_sed1$se.fit)
srmod_sed1$lci <- srmod_sed1$fit-(srmod_sed1$se*1.96)
srmod_sed1$uci <- srmod_sed1$fit+(srmod_sed1$se*1.96)
head(srmod_sed1)

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

# plot species richness ----

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

x_labels <- c("AG", "BG", "AG", "BG")
gdf

dev.new(width=8.27,height=11.69,dpi=70,pointsize=18, noRStudioGD = T)
par(mfrow=c(5,3),mar=c(4,4,1.5,1), mgp=c(2.4,1,0))

# all sr
plot(c(1:4), srmod_all1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_all1$lci)), max(srmod_all1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_all1$lci, c(1:4), srmod_all1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(a) Total", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_all1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# native
plot(c(1:4), srmod_nat1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nat1$lci)), max(srmod_nat1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_nat1$lci, c(1:4), srmod_nat1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(b) Native", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_nat1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)
     
# exotic
plot(c(1:4), srmod_exo1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exo1$lci)), max(srmod_exo1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exo1$lci, c(1:4), srmod_exo1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(c) Exotic", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_exo1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# annual
plot(c(1:4), srmod_ann1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_ann1$lci)), max(srmod_ann1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_ann1$lci, c(1:4), srmod_ann1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(d) Annual", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_ann1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# perennial
plot(c(1:4), srmod_per1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_per1$lci)), max(srmod_per1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_per1$lci, c(1:4), srmod_per1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(e) Perennial", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_per1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# forb
plot(c(1:4), srmod_for1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_for1$lci)), max(srmod_for1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_for1$lci, c(1:4), srmod_for1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(f) Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_for1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# grass
plot(c(1:4), srmod_gra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_gra1$lci)), max(srmod_gra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_gra1$lci, c(1:4), srmod_gra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(g) Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_gra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# native grass
plot(c(1:4), srmod_natgra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natgra1$lci)), max(srmod_natgra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_natgra1$lci, c(1:4), srmod_natgra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(h) Native Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_natgra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# exotic grass
plot(c(1:4), srmod_exogra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exogra1$lci)), max(srmod_exogra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exogra1$lci, c(1:4), srmod_exogra1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(i) Exotic Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_exogra1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# native forbs
plot(c(1:4), srmod_natfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natfor1$lci)), max(srmod_natfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_natfor1$lci, c(1:4), srmod_natfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(j) Native Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_natfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# exotic forbs
plot(c(1:4), srmod_exofor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exofor1$lci)), max(srmod_exofor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_exofor1$lci, c(1:4), srmod_exofor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(k) Exotic Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_exofor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# non-leguminous forbs
plot(c(1:4), srmod_legfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nonlegfor1$lci)), max(srmod_nonlegfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_nonlegfor1$lci, c(1:4), srmod_nonlegfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(l) Non-leguminous Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_nonlegfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

# leguminous forbs
plot(c(1:4), srmod_legfor1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_legfor1$lci)), max(srmod_legfor1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_legfor1$lci, c(1:4), srmod_legfor1$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=T, cex.axis=0.8)
title(main = "(m) Leguminous Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_legfor1$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

par(xpd=NA)
legend(7,2.9, legend=c("Control", "Burn"), col = c("chartreuse4", "orange"),pch=c(20, 20), cex = (1),pt.cex=2, title = NULL,bty="o")
par(xpd=F)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# Site x species matrix for all species, above and below (n=117) ----

head(AGmat2[,1:10],3); dim(AGmat2)
head(BGmat2[,1:10],3); dim(BGmat2)

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
# up to here

ALLmat.bg <- matrix(0, nrow = length(rownames_bg), ncol = length(cols_all1))
colnames(ALLmat.bg) <- cols_all1
rownames(ALLmat.bg) <- rownames_bg
ALLmat.bg[1:nrow(BGmat2), colnames(BGmat2)] <- BGmat2

head(ALLmat.ag,3); dim(ALL.mat.ag)

#ALLmat - combined above/below ssm
ALLmat <- rbind(ALLmat.ag, ALLmat.bg)

#div 6 - presence/absence ssm
div6 <- ALLmat
div6[div6 > 0] <- 1
head(div6);dim(div6)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----

# ---- PCA ----

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

#modelling pca components 1-3
#comp1
#comp1_lmer.int<-lmer(pca.comp1~ab*burn_trt+(1|transect), data=pcadata2)
#summary(comp1_lmer.int)

comp1_lmer<-lmer(pca.comp1~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp1_lmer)

#comp2
#comp2_lmer.int<-lmer(pca.comp2~ab*burn_trt+(1|transect), data=pcadata2)
#summary(comp2_lmer.int)

comp2_lmer<-lmer(pca.comp2~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp2_lmer)

#comp3
#comp3_lmer.int<-lmer(pca.comp3~ab*burn_trt+(1|transect), data=pcadata2)
#summary(comp3_lmer.int)

comp3_lmer<-lmer(pca.comp3~ab+burn_trt+(1|transect), data=pcadata2)
summary(comp3_lmer)

#plotting pca
shapes<-c(15,17)
shapes<-shapes[as.factor(pcadata2$ab)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcadata2$burn_trt)]
View(shapes)
View(col.1)


dev.new(height=8,width=10,dpi=80,pointsize=14,noRStudioGD = T)

#scree plot
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(3,1,0), oma=c(0,0,0,8))
plot(x=1:length(pov1),y=pov1,ylab="Propotion Variance Explained",xlab="Components",type="p", las=1)
lines(x=1:length(pov1),y=pov1)
mtext("(a)",3,0.7,F,0)

#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, choices=c(3, 4))
#mtext("(b)",3,0.7,F,adj = 0)

#1 vs 2
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1), las=1)
#legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(b)",3,0.4,F,adj=0)
title(ylab="PC2",cex=1.2, mgp=c(2.5,1,0))
title(xlab="PC1",cex=1.2, mgp=c(2.5,1,0))
text(x=c(-0.61888015), y=c(-7.32836938), labels= c("T26_20"), pos=4)
text(x=c(14.84107197), y=c(-0.54987242), labels= c("T06_57"), pos=2)

par(xpd=NA)
legend(3.5,2, legend=c("Control Above", "Control Below", "Burnt Above", "Burnt Below"), col = c("grey60", "grey60", "grey20", "grey20"),pch=c(15, 17, 15, 17))
par(xpd=F)

#1 vs 3
plot(pcadata2$pca.comp1,pcadata2$pca.comp3,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1), las=1)
#legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2, mgp=c(2.5,1,0))
title(xlab="PC1",cex=1.2, mgp=c(2.5,1,0))
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#2 vs 3
plot(pcadata2$pca.comp2,pcadata2$pca.comp3,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1), las=1)
#legend("bottomleft",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(d)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2, mgp=c(2.5,1,0))
title(xlab="PC2",cex=1.2, mgp=c(2.5,1,0))
text(x=c(0.69979374), y=c(-6.582940182), labels= c("T10_06"), pos=2)
text(x=c(-7.32836938), y=c(1.416516483), labels= c("T26_20"), pos=4)

# ----

# Beta diversity ----

#all
head(div4);dim(div4)
head(AGmat[,1:10]);dim(AGmat)
head(BGmat[,1:10]);dim(BGmat)

range(colSums(AGmat))
range(colSums(BGmat))

AGgam <- ncol(AGmat)
BGgam <- ncol(BGmat)

AGbeta <- AGgam/(div4$all[which(div4$ab == "above")])
BGbeta <- BGgam/(div4$all[which(div4$ab == "below")])

div4$beta.all <- c(AGbeta, BGbeta)

#beta native
AGbeta.nat <- group.df$no_species[group.df$group == "AG.native"]/(div4$native[which(div4$ab == "above")])
BGbeta.nat <- group.df$no_species[group.df$group == "BG.native"]/(div4$native[which(div4$ab == "below")])
  
div4$beta.nat <- c(AGbeta.nat, BGbeta.nat)

#beta exotic
AGbeta.exo <- group.df$no_species[group.df$group == "AG.exotic"]/(div4$exotic[which(div4$ab == "above")])
BGbeta.exo <- group.df$no_species[group.df$group == "BG.exotic"]/(div4$exotic[which(div4$ab == "below")])

div4$beta.exo <- c(AGbeta.exo, BGbeta.exo)

#beta exotic
AGbeta.exo <- group.df$no_species[group.df$group == "AG.exotic"]/(div4$exotic[which(div4$ab == "above")])
BGbeta.exo <- group.df$no_species[group.df$group == "BG.exotic"]/(div4$exotic[which(div4$ab == "below")])

div4$beta.exo <- c(AGbeta.exo, BGbeta.exo)

#beta annual
AGbeta.ann <- ifelse(div4$annual[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.annual"] /div4$annual[which(div4$ab == "above")])
BGbeta.ann <- ifelse(div4$annual[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.annual"] /div4$annual[which(div4$ab == "below")])

div4$beta.ann <- c(AGbeta.ann, BGbeta.ann)


#beta perennial
AGbeta.per <- group.df$no_species[group.df$group == "AG.perr"]/(div4$perr[which(div4$ab == "above")])
BGbeta.per <- group.df$no_species[group.df$group == "BG.perr"]/(div4$perr[which(div4$ab == "below")])

div4$beta.per <- c(AGbeta.per, BGbeta.per)

#beta forb
AGbeta.for <- group.df$no_species[group.df$group == "AG.forb"]/(div4$forb[which(div4$ab == "above")])
BGbeta.for <- group.df$no_species[group.df$group == "BG.forb"]/(div4$forb[which(div4$ab == "below")])

div4$beta.for <- c(AGbeta.for, BGbeta.for)

#beta grass
AGbeta.gra <- group.df$no_species[group.df$group == "AG.grass"]/(div4$grass[which(div4$ab == "above")])
BGbeta.gra <- group.df$no_species[group.df$group == "BG.grass"]/(div4$grass[which(div4$ab == "below")])

div4$beta.gra <- c(AGbeta.gra, BGbeta.gra)

#beta native grass
AGbeta.natgra <- group.df$no_species[group.df$group == "AG.native_grass"]/(div4$native_grass[which(div4$ab == "above")])
BGbeta.natgra <- group.df$no_species[group.df$group == "BG.native_grass"]/(div4$native_grass[which(div4$ab == "below")])

div4$beta.natgra <- c(AGbeta.natgra, BGbeta.natgra)

#beta exotic grass
AGbeta.exogra <- ifelse(div4$exotic_grass[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.exotic_grass"] /div4$exotic_grass[which(div4$ab == "above")])
BGbeta.exogra <- ifelse(div4$exotic_grass[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.exotic_grass"] /div4$exotic_grass[which(div4$ab == "below")])

div4$beta.exogra <- c(AGbeta.exogra, BGbeta.exogra)

#beta native forb
AGbeta.natfor <- group.df$no_species[group.df$group == "AG.native_forb"]/(div4$native_forb[which(div4$ab == "above")])
BGbeta.natfor <- group.df$no_species[group.df$group == "BG.native_forb"]/(div4$native_forb[which(div4$ab == "below")])

div4$beta.natfor <- c(AGbeta.natfor, BGbeta.natfor)

#beta exotic forb
AGbeta.exofor <- ifelse(div4$exotic_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.exotic_forb"] /div4$exotic_forb[which(div4$ab == "above")])
BGbeta.exofor <- ifelse(div4$exotic_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.exotic_forb"] /div4$exotic_forb[which(div4$ab == "below")])

div4$beta.exofor <- c(AGbeta.exofor, BGbeta.exofor)


#beta non leguminous forb
AGbeta.nlegfor <- ifelse(div4$nonleg_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.nonleg_forb"] /div4$nonleg_forb[which(div4$ab == "above")])
BGbeta.nlegfor <- ifelse(div4$nonleg_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.nonleg_forb"] /div4$nonleg_forb[which(div4$ab == "below")])

div4$beta.nlegfor <- c(AGbeta.nlegfor, BGbeta.nlegfor)

#beta leguminous forb
AGbeta.legfor <- ifelse(div4$leg_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.leg_forb"] /div4$leg_forb[which(div4$ab == "above")])
BGbeta.legfor <- ifelse(div4$leg_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.leg_forb"] /div4$leg_forb[which(div4$ab == "below")])

div4$beta.legfor <- c(AGbeta.legfor, BGbeta.legfor)

#beta diversity modelling - lmer. all interactions insignificant except total, annual, perennial. all else uses additive models in further analysis.
#all -> interaction significant
allbeta<-lmer(beta.all~ab*burn_trt+(1|transect), data=div4)
summary(allbeta)

#allbeta1<-lmer(beta.all~ab+burn_trt+(1|transect), data=div4)
#summary(allbeta1)

#native
#natbeta<-lmer(beta.nat~ab*burn_trt+(1|transect), data=div4)
#summary(natbeta)

natbeta2<-lmer(beta.nat~ab+burn_trt+(1|transect), data=div4)
summary(natbeta2)

#exotic
#exobeta<-lmer(beta.exo~ab*burn_trt+(1|transect), data=div4)
#summary(exobeta)

exobeta2<-lmer(beta.exo~ab+burn_trt+(1|transect), data=div4)
summary(exobeta2)

#annual - interaction significant
annbeta<-lmer(beta.ann~ab*burn_trt+(1|transect), data=div4)
summary(annbeta)

#annbeta2<-lmer(beta.ann~ab+burn_trt+(1|transect), data=div4)
#summary(annbeta2)

#perennial - interaction significant
perbeta<-lmer(beta.per~ab*burn_trt+(1|transect), data=div4)
summary(perbeta)

#perbeta2<-lmer(beta.per~ab+burn_trt+(1|transect), data=div4)
#summary(perbeta)

#forb
#forbeta<-lmer(beta.for~ab*burn_trt+(1|transect), data=div4)
#summary(forbeta)

forbeta2<-lmer(beta.for~ab+burn_trt+(1|transect), data=div4)
summary(forbeta2)

#grass
#grabeta<-lmer(beta.gra~ab*burn_trt+(1|transect), data=div4)
#summary(grabeta)

grabeta2<-lmer(beta.gra~ab+burn_trt+(1|transect), data=div4)
summary(grabeta2)

#native grass
#natgrabeta<-lmer(beta.natgra~ab*burn_trt+(1|transect), data=div4)
#summary(natgrabeta)

natgrabeta2<-lmer(beta.natgra~ab+burn_trt+(1|transect), data=div4)
summary(natgrabeta2)

#exotic grass
#exograbeta<-lmer(beta.exogra~ab*burn_trt+(1|transect), data=div4)
#summary(exograbeta)

exograbeta2<-lmer(beta.exogra~ab+burn_trt+(1|transect), data=div4)
summary(exograbeta2)

#native forb
#natforbeta<-lmer(beta.natfor~ab*burn_trt+(1|transect), data=div4)
#summary(natforbeta)

natforbeta2<-lmer(beta.natfor~ab+burn_trt+(1|transect), data=div4)
summary(natforbeta2)

#exotic forb
#exoforbeta<-lmer(beta.exofor~ab*burn_trt+(1|transect), data=div4)
#summary(exoforbeta)

exoforbeta2<-lmer(beta.exofor~ab+burn_trt+(1|transect), data=div4)
summary(exoforbeta2)

#non leg forb
#nlegbeta<-lmer(beta.nlegfor~ab*burn_trt+(1|transect), data=div4)
#summary(nlegbeta)

nlegbeta2<-lmer(beta.nlegfor~ab+burn_trt+(1|transect), data=div4)
summary(nlegbeta2)
round(summary(nlegbeta2)$coefficient,3)

#leg forb
#legbeta<-lmer(beta.legfor~ab*burn_trt+(1|transect), data=div4)
#summary(legbeta)

legbeta2<-lmer(beta.legfor~ab+burn_trt+(1|transect), data=div4)
summary(legbeta2)
round(summary(legbeta2)$coefficient,3)

#predictse beta diversity
#all
srmod_beta.all <- predictSE(mod=allbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.all <- data.frame(nd1, fit = srmod_beta.all$fit, se = srmod_beta.all$se.fit)
srmod_beta.all$lci <- srmod_beta.all$fit-(srmod_beta.all$se*1.96)
srmod_beta.all$uci <- srmod_beta.all$fit+(srmod_beta.all$se*1.96)
head(srmod_beta.all)

#native
srmod_beta.nat <- predictSE(mod=natbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.nat <- data.frame(nd1, fit = srmod_beta.nat$fit, se = srmod_beta.nat$se.fit)
srmod_beta.nat$lci <- srmod_beta.nat$fit-(srmod_beta.nat$se*1.96)
srmod_beta.nat$uci <- srmod_beta.nat$fit+(srmod_beta.nat$se*1.96)
head(srmod_beta.nat)

#exotic
srmod_beta.exo <- predictSE(mod=exobeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exo <- data.frame(nd1, fit = srmod_beta.exo$fit, se = srmod_beta.exo$se.fit)
srmod_beta.exo$lci <- srmod_beta.exo$fit-(srmod_beta.exo$se*1.96)
srmod_beta.exo$uci <- srmod_beta.exo$fit+(srmod_beta.exo$se*1.96)
head(srmod_beta.exo)

#annual
srmod_beta.ann <- predictSE(mod=annbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.ann <- data.frame(nd1, fit = srmod_beta.ann$fit, se = srmod_beta.ann$se.fit)
srmod_beta.ann$lci <- srmod_beta.ann$fit-(srmod_beta.ann$se*1.96)
srmod_beta.ann$uci <- srmod_beta.ann$fit+(srmod_beta.ann$se*1.96)
head(srmod_beta.ann)

#perennial
srmod_beta.per <- predictSE(mod=perbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.per <- data.frame(nd1, fit = srmod_beta.per$fit, se = srmod_beta.per$se.fit)
srmod_beta.per$lci <- srmod_beta.per$fit-(srmod_beta.per$se*1.96)
srmod_beta.per$uci <- srmod_beta.per$fit+(srmod_beta.per$se*1.96)
head(srmod_beta.per)

#forb
srmod_beta.for <- predictSE(mod=forbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.for <- data.frame(nd1, fit = srmod_beta.for$fit, se = srmod_beta.for$se.fit)
srmod_beta.for$lci <- srmod_beta.for$fit-(srmod_beta.for$se*1.96)
srmod_beta.for$uci <- srmod_beta.for$fit+(srmod_beta.for$se*1.96)
head(srmod_beta.for)

#grass
srmod_beta.gra <- predictSE(mod=grabeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.gra <- data.frame(nd1, fit = srmod_beta.gra$fit, se = srmod_beta.gra$se.fit)
srmod_beta.gra$lci <- srmod_beta.gra$fit-(srmod_beta.gra$se*1.96)
srmod_beta.gra$uci <- srmod_beta.gra$fit+(srmod_beta.gra$se*1.96)
head(srmod_beta.gra)

#native grass
srmod_beta.natgra <- predictSE(mod=natgrabeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.natgra <- data.frame(nd1, fit = srmod_beta.natgra$fit, se = srmod_beta.natgra$se.fit)
srmod_beta.natgra$lci <- srmod_beta.natgra$fit-(srmod_beta.natgra$se*1.96)
srmod_beta.natgra$uci <- srmod_beta.natgra$fit+(srmod_beta.natgra$se*1.96)
head(srmod_beta.natgra)

#exotic grass
srmod_beta.exogra <- predictSE(mod=exograbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exogra <- data.frame(nd1, fit = srmod_beta.exogra$fit, se = srmod_beta.exogra$se.fit)
srmod_beta.exogra$lci <- srmod_beta.exogra$fit-(srmod_beta.exogra$se*1.96)
srmod_beta.exogra$uci <- srmod_beta.exogra$fit+(srmod_beta.exogra$se*1.96)
head(srmod_beta.exogra)

#native forb
srmod_beta.natfor <- predictSE(mod=natforbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.natfor <- data.frame(nd1, fit = srmod_beta.natfor$fit, se = srmod_beta.natfor$se.fit)
srmod_beta.natfor$lci <- srmod_beta.natfor$fit-(srmod_beta.natfor$se*1.96)
srmod_beta.natfor$uci <- srmod_beta.natfor$fit+(srmod_beta.natfor$se*1.96)
head(srmod_beta.natfor)

#exotic forb
srmod_beta.exofor <- predictSE(mod=exoforbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exofor <- data.frame(nd1, fit = srmod_beta.exofor$fit, se = srmod_beta.exofor$se.fit)
srmod_beta.exofor$lci <- srmod_beta.exofor$fit-(srmod_beta.exofor$se*1.96)
srmod_beta.exofor$uci <- srmod_beta.exofor$fit+(srmod_beta.exofor$se*1.96)
head(srmod_beta.exofor)

#non-leguminous forb
srmod_beta.nlegfor <- predictSE(mod=nlegbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.nlegfor <- data.frame(nd1, fit = srmod_beta.nlegfor$fit, se = srmod_beta.nlegfor$se.fit)
srmod_beta.nlegfor$lci <- srmod_beta.nlegfor$fit-(srmod_beta.nlegfor$se*1.96)
srmod_beta.nlegfor$uci <- srmod_beta.nlegfor$fit+(srmod_beta.nlegfor$se*1.96)
head(srmod_beta.nlegfor)

#leguminous forb
srmod_beta.legfor <- predictSE(mod=legbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.legfor <- data.frame(nd1, fit = srmod_beta.legfor$fit, se = srmod_beta.legfor$se.fit)
srmod_beta.legfor$lci <- srmod_beta.legfor$fit-(srmod_beta.legfor$se*1.96)
srmod_beta.legfor$uci <- srmod_beta.legfor$fit+(srmod_beta.legfor$se*1.96)
head(srmod_beta.legfor)

# ----

# Plot beta diversity ----

dev.new(width=9,height=15,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(5,3),mar=c(4,4,1.5,1), mgp=c(2.5,1,0))

#all
plot(c(1:4), srmod_beta.all$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.all$lci)), max(srmod_beta.all$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.all$lci, c(1:4), srmod_beta.all$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(a) Total*", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.all$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native
plot(c(1:4), srmod_beta.nat$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.nat$lci)), max(srmod_beta.nat$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.nat$lci, c(1:4), srmod_beta.nat$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(b) Native", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.nat$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic
plot(c(1:4), srmod_beta.exo$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.exo$lci)), max(srmod_beta.exo$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.exo$lci, c(1:4), srmod_beta.exo$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(c) Exotic", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.exo$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#annual
plot(c(1:4), srmod_beta.ann$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.ann$lci)), max(srmod_beta.ann$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.ann$lci, c(1:4), srmod_beta.ann$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(d) Annual*", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.ann$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#perennial
plot(c(1:4), srmod_beta.per$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.per$lci)), max(srmod_beta.per$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.per$lci, c(1:4), srmod_beta.per$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(e) Perennial*", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.per$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#forb
plot(c(1:4), srmod_beta.for$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.for$lci)), max(srmod_beta.for$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.for$lci, c(1:4), srmod_beta.for$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(f) Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.for$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#grass
plot(c(1:4), srmod_beta.gra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.gra$lci)), max(srmod_beta.gra$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.gra$lci, c(1:4), srmod_beta.gra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(g) Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.gra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native grass
plot(c(1:4), srmod_beta.natgra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.natgra$lci)), max(srmod_beta.natgra$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.natgra$lci, c(1:4), srmod_beta.natgra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(h) Native Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.natgra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic grass
plot(c(1:4), srmod_beta.exogra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.exogra$lci)), max(srmod_beta.exogra$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.exogra$lci, c(1:4), srmod_beta.exogra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(i) Exotic Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.exogra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native forb
plot(c(1:4), srmod_beta.natfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.natfor$lci)), max(srmod_beta.natfor$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.natfor$lci, c(1:4), srmod_beta.natfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(j) Native Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.natfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic forb
plot(c(1:4), srmod_beta.exofor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.exofor$lci)), max(srmod_beta.exofor$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.exofor$lci, c(1:4), srmod_beta.exofor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(k) Exotic Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.exofor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#non-leg forb
plot(c(1:4), srmod_beta.nlegfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.nlegfor$lci)), max(srmod_beta.nlegfor$uci)), ylab="Beta Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.nlegfor$lci, c(1:4), srmod_beta.nlegfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(l) Non-leg Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.nlegfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)


#leguminous forbs 
plot(c(1:4), srmod_beta.legfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_beta.legfor$lci)), max(srmod_beta.legfor$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_beta.legfor$lci, c(1:4), srmod_beta.legfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(m) Leg Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_beta.legfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

par(xpd=NA)
legend(8,5.9, legend=c("Control", "Burn"), col = c("chartreuse4", "orange"),pch=c(20, 20), cex = (1.45), title = "Legend")
par(xpd=F)

# ----

