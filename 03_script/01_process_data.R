# Analysis script

# Author Caitlin Gaskell

#---- libraries
library(VennDiagram)
library(lme4)
library(vegan)
library(abdiv)
library(divo)
library(AICcmodavg)

#---- loading data

#combined species list of above and below ground, including non identified
combospec<-read.table("01_data/combinedspecies.txt",header=T)
head(combospec,4);dim(combospec)

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
#alt
#BGspecid<-BGspec[which(!is.na(BGspec$species)),]
#BGspecid <- BGspec[BGspec$speciesID == "1",]

#t01-46 AG species list incl. morphospecies
AGspecall<-read.table("01_data/alltransectspeciesag.txt",header=T)
head(AGspecall);dim(AGspecall)

#t37-46 AG species list incl. morphospecies
#AGspec <-AGspecall[AGspecall$location == "1" | AGspecall$location == "2",]
head(AGspec,4);dim(AGspec)
AGspec <-combospec[combospec$location == "1" | combospec$location == "2",]

#t37-46 AG species list excl. morphospecies
#AGspecid <- AGspecall[(AGspecall$location == "1" | AGspecall$location == "2") & AGspecall$speciesID == "1",]
#AGspecid <- combospec[(combospec$location == "1") | combospec$location == "2" & combospec$speciesID == "1", ]
head(AGspecid,4);dim(AGspecid)
AGspecid <- combospec[(combospec$location %in% c("1", "2")) & combospec$speciesID == "1", ]

table(AGspecid$code %in% AGdata$sp)
which(!AGspecid$code %in% unique(AGdata$sp))

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
print(AGspecid$code[which(!AGspecid$code %in% unique(AGdata$sp))])

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


# sr VENN diagram:


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

belowonly <- sum((combospec$location == "0" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
aboveonly <- sum((combospec$location == "1" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1"))
overlap <- sum(combospec$location == "2" & combospec$speciesID == "1")

#merging datasets
library(tidyr)
library(dplyr)
#venn2 <- merge(combospec, AGdata, by.x = "code", by.y = "sp", all.x = TRUE)
#venn2 <- merge(combospec, tdata, by = "code", all.x = TRUE)
head(venn2);dim(venn2)

combospec$burn <- ifelse(is.na(combospec$burn_trt.x), 0, ifelse(is.na(combospec$burn_trt.y), 1, 2))

AGdata$code <- AGdata$sp
AGdata <- subset(AGdata, select = -sp)

merged_data <- merge(tdata, combospec, by = "code", all.x = TRUE)
merged_data <- merge(AGdata, combospec, by = "code", all.x = T)

merged_data$burn <- ifelse(merged_data$burn_trt == "Control", 0, 
                           ifelse(merged_data$burn_trt == "Burn", 1, 2))

merged_data_summary <- merged_data %>%
  group_by(code) %>%
  summarize(burn = case_when(
    all(burn == 0) ~ 0,   # If all burns are 0, assign 0
    all(burn == 1) ~ 1,   # If all burns are 1, assign 1
    TRUE ~ 2              # Otherwise, assign 2
  ))

merged_data <- merged_data %>%
  distinct(code, species.x, life_span, form, family, speciesID, location.y, burn)

#define functional groups
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

BG.native <- BGspecid$code[which(BGspecid$origin == "Native")]
BG.exotic <- BGspecid$code[which(BGspecid$origin == "Exotic")]
BG.annual <- BGspecid$code[which(BGspecid$life_span == "Annual")]
BG.perr <- BGspecid$code[which(BGspecid$life_span == "Perennial")]
BG.tree <- BGspecid$code[which(BGspecid$form == "Tree")]
BG.shrub <- BGspecid$code[which(BGspecid$form == "Shrub")]
BG.forb <- BGspecid$code[which(BGspecid$form == "Herb")]
BG.grass <- BGspecid$code[which(BGspecid$form == "Grass")]
BG.sedge <- BGspecid$code[which(BGspecid$form == "Sedge/Rush")]
BG.native_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Native")]
BG.exotic_grass <- BGspecid$code[which(BGspecid$form == "Grass" & BGspecid$origin == "Exotic")]

AG.native <- AGspecid$code[which(AGspecid$origin == "Native")]
AG.exotic <- AGspecid$code[which(AGspecid$origin == "Exotic")]
AG.annual <- AGspecid$code[which(AGspecid$life_span == "Annual")]
AG.perr <- AGspecid$code[which(AGspecid$life_span == "Perennial")]
AG.tree <- AGspecid$code[which(AGspecid$form == "Tree")]
AG.shrub <- AGspecid$code[which(AGspecid$form == "Shrub")]
AG.forb <- AGspecid$code[which(AGspecid$form == "Herb")]
AG.grass <- AGspecid$code[which(AGspecid$form == "Grass")]
AG.sedge <- AGspecid$code[which(AGspecid$form == "Sedge/Rush")]
AG.native_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Native")]
AG.exotic_grass <- AGspecid$code[which(AGspecid$form == "Grass" & AGspecid$origin == "Exotic")]

#put AGshrub back in group when 5x5 data added
#"AG.shrub",
#make a df of functional groups:
group.df <- data.frame(group = c("BG.native","BG.exotic","BG.annual","BG.perr","BG.tree","BG.shrub","BG.forb","BG.grass","BG.sedge","BG.native_grass","BG.exotic_grass","AG.native","AG.exotic","AG.annual","AG.perr","AG.tree","AG.forb","AG.grass","AG.sedge","AG.native_grass","AG.exotic_grass"))

rich.data<-list()
#shan.data<-list()

head(AGmat); dim(AGmat)
head(BGmat); dim(BGmat)

for (i in 1:nrow(group.df)){
  
  name.thisrun<-as.character(group.df$group[i])
  vec.thisrun<-get(name.thisrun)
  
  if(substr(name.thisrun,1,3)=="BG.") data.thisrun<-BGmat[,colnames(BGmat) %in% vec.thisrun]
  if(substr(name.thisrun,1,3)=="AG.") data.thisrun<-AGmat[,colnames(AGmat) %in% vec.thisrun]
  head(data.thisrun); dim(data.thisrun)
  
  rich.data[[i]]<-apply(data.thisrun,1,function(x)length(which(x>0)))
  
  #shan.data[[i]]<-diversity(data.thisrun,index="shannon")
  
} # close i for

rich.res<-data.frame(do.call(cbind,rich.data))
colnames(rich.res)<-group.df$group



#------ ssm contstruction

#AG ssm
head(AGdata); dim(AGdata)
length(unique(AGdata$quadratID))
AGmat <- as.data.frame.matrix(xtabs(cover~quadratID + sp, data=AGdata))
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

#constructing ssm
head(sdata,4);dim(sdata)

div1 <- sdata


#AG species richness
div1$agsr <- apply(AGmat, MARGIN = 1, FUN = function(x) length(which(x > 0)))
#xx <- AGmat[, 3]
#length(which(xx > 0))

#boxplot(div1$agsr ~ div1$burn_trt)

#bgsr, agsimps, bgsimps
#BG species richness
div1$bgsr <- apply(BGmat, MARGIN = 1, FUN = function(x) length(which(x>0)))
#xxx <- BGmat[, 1]
#length(which(xxx > 0))

head(div1);dim(div1)
head(rich.res); dim(rich.res)
rownames(rich.res) == div1$quadratID

rich.res <- data.frame(quadratID = rownames(rich.res),rich.res)

cnt_rich<-merge(div1,rich.res, by="quadratID", all.x = T, all.y = F)

#shan.res<-data.frame(do.call(cbind,shan.data))
#colnames(shan.res)<-group_df$group
#cnt_shan<-cbind(cnt,shan.res)

head(cnt_rich,3); dim(cnt_rich)
#head(cnt_shan,3)
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

#
gdf<-group.df
gdf

gdf$ylab<-c("AG all","BG all","BG Native","Important","Indicator","Important + Indicator","Increaser","Native forb","Exotic forb","Exotic annual forb","Exotic perennial forb","Native annual forb","Native perennial forb","Native non-leg. forb","Exotic non-leg. forb","Native leg. forb","Exotic leg. forb","Native grass","Exotic grass","Exotic annual grass","Exotic perennial grass","C3 grass","C4 grass","Native C3 grass","Native C4 grass","Exotic C3 grass","Sedge/Rush")


#bwplot(div1$bgsr ~ div1$burn_trt)

#bwplot sr 
div2 <- cnt_rich[, 1:5]
head(div2);dim(div2)

div3<-rbind(div2,div2)
div3$ab<-c(rep("above",30),rep("below",30))
head(div3);dim(div3)


head(cnt_rich,3); dim(cnt_rich)

AG.sr<-cnt_rich[,grep("AG.",colnames(cnt_rich))]
AG.sr<-data.frame(AG.sr[,which(colnames(AG.sr)=="AG.all"):which(colnames(AG.sr)=="AG.tree")],AG.shrub=rep(0,30),AG.sr[,which(colnames(AG.sr)=="AG.forb"):which(colnames(AG.sr)=="AG.exotic_grass")])

colnames(AG.sr)<-substr(colnames(AG.sr),4,nchar(colnames(AG.sr)))

BG.sr<-cnt_rich[,grep("BG.",colnames(cnt_rich))]
colnames(BG.sr)<-substr(colnames(BG.sr),4,nchar(colnames(BG.sr)))

head(AG.sr,3); dim(AG.sr)
head(BG.sr,3); dim(BG.sr)

length(colnames(AG.sr))
length(colnames(BG.sr))

#should all be true
colnames(AG.sr)==colnames(BG.sr)

all.sr<-rbind(AG.sr,BG.sr)
head(all.sr,3); dim(all.sr)

div4<-cbind(div3,all.sr)
head(div4,3); dim(div4)

div5$burn_trt <- factor(div5$burn_trt, levels = c("Control","Burn"))
div5$ab <- factor(div5$ab, levels = c("above","below"))

head(div5);dim(div5)
str(div5)

#bwplot
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)

par(mfrow = c(1, 2),mar=c(4,4,1,1))
boxplot(sr ~ ab, data = div5, las = 1, ylab = "species richness", xlab = "")

boxplot(sr ~ ab + burn_trt, data = div5, las = 1, ylab = "species richness", xlab = "", cex.axis = 0.7)

#sr glmer
#with interaction
sr_mod1<-glmer(sr~ab*burn_trt+(1|transect), family="poisson", data=div5) 
summary(sr_mod1)
#without interaction
sr_mod2<-glmer(sr~ab+burn_trt+(1|transect), family="poisson", data=div5)
summary(sr_mod2)

#hist(div5$sr)
#plot(div5$sr, div5$ab)


#
#nd1 <- data.frame(treatment = factor(c("control", "burn"), levels = c("control", "burn")))
#nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below"), burn_trt = factor("Control", "Burn"), levels = c("Control", "Burn")), ab = factor(c("above", "below"), levels = c("above", "below")))
nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
sr_mod3 <- predictSE(mod=sr_mod2,newdata=nd1,type="response",se.fit = T)
sr_mod3 <- data.frame(nd1, fit = sr_mod3$fit, se = sr_mod3$se.fit)
sr_mod3$lci <- sr_mod3$fit-(sr_mod3$se*1.96)
sr_mod3$uci <- sr_mod3$fit+(sr_mod3$se*1.96)
#view sr_mod3
head(sr_mod3)
summary(sr_mod2)$coefficients

#plot sr_mod3 - old, only above/below
#dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
#jpeg(sr1.jpeg)
#par(mfrow=c(2,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,6))
#plot(c(1:4),sr_mod3$fit, xlim=c(0.5,2.5), pch=20, xaxt="n",ylim=c((min(sr_mod3$lci)),max(sr_mod3$uci)),ylab="Species Richness",xlab="", las = 1, cex = 2.5)
#arrows(c(1:2),sr_mod3$lci,c(1:2),sr_mod3$uci,length=0.03,code=3,angle=90)
#axis(1,at=c(1:2),labels=F)
#axis(side = 1,at=c(1:4),labels=sr_mod3$ab,tick=F, cex.axis=1)
#mtext(paste("p = ",round(summary(sr_mod3)$coefficients["burn_trt",2],3)), side=3,line=0.1,adj=1, cex=1)

x_labels <- with(sr_mod3, interaction(ab, burn_trt))

dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), sr_mod3$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(sr_mod3$lci)), max(sr_mod3$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), sr_mod3$lci, c(1:4), sr_mod3$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=1)
     
#AG shannon and simpson

div1$agshan <- diversity(AGmat, index = "shannon")
div1$agsimp <- diversity(AGmat, index = "invsimpson")


bwplot(div1$agsimp ~ div1$burn_trt)

#BG shannon and simpson
div1$bgshan <- diversity(BGmat, index = "shannon")
div1$bgsimp <- diversity(BGmat, index = "invsimpson")


bwplot(div1$bgsimp ~ div1$burn_trt)

#sorensen

#div1$bgsoren <- li(BGmat)
#div1 <- subset(div1, select = -bgsoren)

#margalef?
#div1$agmarg <- margalef(AGmat) doesn't work as for community level comparison only
AGmarg <- margalef(AGmat)
Bgmarg <- margalef(BGmat)


#?? singular error either due to collinearity (variables being linear combinations of others), so likely not enough variability?
BGmodel_data <- data.frame(
  burn_trt = div1$burn_trt,
  transect = factor(div1$transect),
  bgsr = div1$bgsr
)
BGmodel_glmm <- glmer(bgsr ~ burn_trt + (1 | transect), data = BGmodel_data, family = poisson)

var(div1)
head(div1);dim(div1)

