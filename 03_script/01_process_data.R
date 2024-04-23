# Analysis script

# Author Caitlin Gaskell

#---- libraries
library(VennDiagram)
library(lme4)
library(vegan)
library(abdiv)
library(divo)
library(AICcmodavg)
library(stringr)
library(tidyr)
library(dplyr)

#---- loading data

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
write.csv(BGspecid, file = "BGspecid.csv", row.names = FALSE)
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


#---- sr VENN diagram:


# Summarise overlap:
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

#---- four-way venn? 
#merging datasets
merged_tdata <- merge(combospec, tdata, by = "code", all.x = T)
merged_AGdata <- merge(combospec, AGdata, by = "code", all.x = T)
merged_data <- merge(merged_tdata, merged_AGdata, by = "code", all = TRUE)

merged_data$burn <- ifelse(merged_data$burn_trt == "Control", 0, 
                           ifelse(merged_data$burn_trt == "Burn", 1, 2))

merged_data_summary <- merged_data %>%
  group_by(code) %>%
  summarize(burn = case_when(
    all(burn == 0) ~ 0,   # If all burns are 0, assign 0
    all(burn == 1) ~ 1,   # If all burns are 1, assign 1
    TRUE ~ 2              # Otherwise, assign 2
  ),
  species.x = first(species.x),
  origin.x = first(origin.x),
  life_span.x = first(life_span.x),
  form.x = first(form.x),
  family.x = first(family.x),
  speciesID.x = case_when(
    all(speciesID.x == 1) ~ 1,
    all(speciesID.x == 0) ~ 0,),
  location.x.y = case_when(
    all(location.x.y == 1) ~ 0,
    all(location.x.y == 1) ~ 1,
    all(location.x.y == 2) ~ 1
  )  )
  
merged_data_sum <- as.data.frame(merged_data_summary)
merged_data_sum <- merge(merged_data_sum, combospec, by = "code", all.x = T)
remove_columns <- c("species.x", "origin.x", "life_span.x", "form.x", "family.x", "speciesID.x", "location.x.y")
merged_data_sum <- merged_data_sum[, !(names(merged_data_sum) %in% remove_columns)]

#quad.venn
draw.quad.venn(
  area1 = sum((dimv4$location=="" combospec$location == "0" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1")) , area2, area3, area4, n12, n13, n14, n23, n24,
               n34, n123, n124, n134, n234, n1234, category = rep("",
                                                                  4), lwd = rep(2, 4), lty = rep("solid", 4), col =
                 rep("black", 4), fill = NULL, alpha = rep(0.5, 4),
               label.col = rep("black", 15), cex = rep(1, 15),
               fontface = rep("plain", 15), fontfamily = rep("serif",
                                                             15), cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22,
                                                                                                           0.22, 0.11, 0.11), cat.col = rep("black", 4), cat.cex
               = rep(1, 4), cat.fontface = rep("plain", 4),
               cat.fontfamily = rep("serif", 4), cat.just =
                 rep(list(c(0.5, 0.5)), 4), rotation.degree = 0,
               rotation.centre = c(0.5, 0.5), ind = TRUE, cex.prop =
                 NULL, print.mode = "raw", sigdigs = 3, direct.area =
                 FALSE, area.vector = 0, ...)
#-----
library(venneuler)
area_0_0 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
area_0_1 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
area_1_0 <- sum(merged_data_sum$burn == 1 & merged_data_sum$location == 0)
area_1_1 <- sum(merged_data_sum$burn == 1 & merged_data_sum$location == 1)

# Define the intersection sizes
n_00_00 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_01 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_10 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_11 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_12 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 2)
n_00_20 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_21 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_22 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 2)

# Draw the Venn diagram
venn_plot <- draw.quad.venn(
  area1 = area_0_0,
  area2 = area_0_1,
  area3 = area_1_0,
  area4 = area_1_1,
  n12 = n_00_00,
  n13 = n_00_01,
  n14 = n_00_10,
  n23 = n_00_02,
  n24 = n_00_11,
  n34 = n_00_12,
  n123 = n_00_20,
  n124 = n_00_21,
  n134 = n_00_22,
  n234 = 0,  # No data for this intersection
  n1234 = 0, # No data for this intersection
  category = c("Control", "Control above", "Treatment", "Treatment above"),
  fill = c("blue", "red", "green", "yellow"),
  label.col = rep("black", 15),
  cex = rep(1, 15),
  fontface = rep("plain", 15),
  cat.dist = c(0.15, 0.15, 0.15, 0.15), # Adjust label distance
  cat.cex = 1.5, # Adjust label size
  cat.fontface = 1, # Adjust label fontface
  cat.just = list(c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5)) # Center label text
)

# Plot the Venn diagram
grid.draw(venn_plot)

#----define functional groups
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

#put AGshrub back in group when 5x5 data added
#"AG.shrub",
#adding AG.shrub back into group.df returns dim(X) must have a positive length when running for loop?
#make a df of functional groups:
group.df <- data.frame(group = c("BG.native","BG.exotic","BG.annual","BG.perr", "BG.leg", "BG.tree","BG.shrub","BG.forb","BG.grass","BG.sedge","BG.native_grass", "BG.exotic_grass","AG.native","AG.exotic","AG.annual","AG.perr","AG.leg", "AG.tree","AG.shrub","AG.forb","AG.grass","AG.sedge", "AG.native_grass","AG.exotic_grass"))

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
  
  print(dim(data.thisrun))
  print(name.thisrun)
  
  head(data.thisrun)
  dim(data.thisrun)
  
  print(data.thisrun)
  
  rich.data[[i]]<-apply(data.thisrun,1,function(x)length(which(x>0)))
  
  #shan.data[[i]]<-diversity(data.thisrun,index="shannon")
  
} # close i for

rich.res<-data.frame(do.call(cbind,rich.data))
colnames(rich.res)<-group.df$group


#------ ssm contstruction

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

#constructing ssm
head(sdata,4);dim(sdata)

div1 <- sdata


#AG species richness
div1$agsr <- apply(AGmat, MARGIN = 1, FUN = function(x) length(which(x > 0)))

#BG species richness
div1$bgsr <- apply(BGmat, MARGIN = 1, FUN = function(x) length(which(x>0)))


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

gdf$ylab<-c("AG all","BG all","BG Native","BG Exotic","BG Annual","BG Perennial","BG Legume","BG Tree","BG Shrub","BG Forb","BG Grass","BG Sedge","BG Native Grass","BG Exotic Grass","AG Native","AG Exotic","AG Annual","AG Perennial","AG Legume","AG Tree","AG Shrub","AG Forb","AG Grass","AG Sedge","AG Native Grass","AG Exotic Grass")


#bwplot(div1$bgsr ~ div1$burn_trt)

#div4 & div5
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
div4$burn_trt <- factor(div4$burn_trt, levels = c("Control","Burn"))
div4$ab <- factor(div4$ab, levels = c("above","below"))

div5$burn_trt <- factor(div5$burn_trt, levels = c("Control","Burn"))
div5$ab <- factor(div5$ab, levels = c("above","below"))

head(div5);dim(div5)
str(div5)

#bwplot
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)

par(mfrow = c(1, 2),mar=c(4,4,1,1))
boxplot(sr ~ ab, data = div5, las = 1, ylab = "species richness", xlab = "")

boxplot(sr ~ ab + burn_trt, data = div5, las = 1, ylab = "species richness", xlab = "", cex.axis = 0.7)

#----SR GLMERS
#div4
#with interaction
sr_mod1<-glmer(sr~ab*burn_trt+(1|transect), family="poisson", data=div5) 
summary(sr_mod1)
#without interaction
sr_mod2<-glmer(sr~ab+burn_trt+(1|transect), family="poisson", data=div5)
summary(sr_mod2)

nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
sr_mod3 <- predictSE(mod=sr_mod2,newdata=nd1,type="response",se.fit = T)
sr_mod3 <- data.frame(nd1, fit = sr_mod3$fit, se = sr_mod3$se.fit)
sr_mod3$lci <- sr_mod3$fit-(sr_mod3$se*1.96)
sr_mod3$uci <- sr_mod3$fit+(sr_mod3$se*1.96)
#view sr_mod3
head(sr_mod3)
summary(sr_mod2)$coefficients

x_labels <- with(sr_mod3, interaction(ab, burn_trt))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), sr_mod3$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(sr_mod3$lci)), max(sr_mod3$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), sr_mod3$lci, c(1:4), sr_mod3$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=1)
title(main = "Species Richness by Treatment")

#div5
#gamma glmer
#without interaction
srmod_all<-glmer(all~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_all)
ndall <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_all1 <- predictSE(mod=srmod_all,newdata=ndall,type="response",se.fit = T)
srmod_all1 <- data.frame(ndall, fit = srmod_all1$fit, se = srmod_all1$se.fit)
srmod_all1$lci <- srmod_all1$fit-(srmod_all1$se*1.96)
srmod_all1$uci <- srmod_all1$fit+(srmod_all1$se*1.96)
#view srmod_nat1
head(srmod_all1)
summary(srmod_all)$coefficients
xall_labels <- with(srmod_all1, interaction(ab, burn_trt))
xall_labels <- str_replace_all(xall_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_all1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_all1$lci)), max(srmod_all1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_all1$lci, c(1:4), srmod_all1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xall_labels, tick=F, cex.axis=1)
title(main = "Gamma Species Richness")


#native
srmod_nat<-glmer(native~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_nat)
ndnat <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_nat1 <- predictSE(mod=srmod_nat,newdata=ndnat,type="response",se.fit = T)
srmod_nat1 <- data.frame(ndnat, fit = srmod_nat1$fit, se = srmod_nat1$se.fit)
srmod_nat1$lci <- srmod_nat1$fit-(srmod_nat1$se*1.96)
srmod_nat1$uci <- srmod_nat1$fit+(srmod_nat1$se*1.96)
#view srmod_nat1
head(srmod_nat1)
summary(srmod_nat)$coefficients
xnat_labels <- with(srmod_nat1, interaction(ab, burn_trt))
xnat_labels <- str_replace_all(xnat_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_nat1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nat1$lci)), max(srmod_nat1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_nat1$lci, c(1:4), srmod_nat1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xnat_labels, tick=F, cex.axis=1)
title(main = "Native Species Richness")


#exotic
srmod_exo<-glmer(exotic~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exo)
ndexo <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_exo1 <- predictSE(mod=srmod_nat,newdata=ndexo,type="response",se.fit = T)
srmod_exo1 <- data.frame(ndnat, fit = srmod_exo1$fit, se = srmod_exo1$se.fit)
srmod_exo1$lci <- srmod_exo1$fit-(srmod_exo1$se*1.96)
srmod_exo1$uci <- srmod_exo1$fit+(srmod_exo1$se*1.96)
#view srmod_exo1
head(srmod_exo1)
summary(srmod_exo)$coefficients
xexo_labels <- with(srmod_exo1, interaction(ab, burn_trt))
xexo_labels <- str_replace_all(xexo_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_exo1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exo1$lci)), max(srmod_exo1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exo1$lci, c(1:4), srmod_exo1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xexo_labels, tick=F, cex.axis=1)
title(main = "Exotic Species Richness")


#annual
srmod_ann<-glmer(annual~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_ann)
ndann <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_ann1 <- predictSE(mod=srmod_ann,newdata=ndann,type="response",se.fit = T)
srmod_ann1 <- data.frame(ndnat, fit = srmod_ann1$fit, se = srmod_ann1$se.fit)
srmod_ann1$lci <- srmod_ann1$fit-(srmod_ann1$se*1.96)
srmod_ann1$uci <- srmod_ann1$fit+(srmod_ann1$se*1.96)
#view srmod_ann1
head(srmod_ann1)
summary(srmod_ann)$coefficients
head(srmod_ann1)
summary(srmod_ann)$coefficients
xann_labels <- with(srmod_ann1, interaction(ab, burn_trt))
xann_labels <- str_replace_all(xann_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_ann1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_ann1$lci)), max(srmod_ann1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_ann1$lci, c(1:4), srmod_ann1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xann_labels, tick=F, cex.axis=1)
title(main = "Annual Species Richness")

#perennial
srmod_per<-glmer(perr~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_per)
ndper <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_per1 <- predictSE(mod=srmod_per,newdata=ndper,type="response",se.fit = T)
srmod_per1 <- data.frame(ndper, fit = srmod_per1$fit, se = srmod_per1$se.fit)
srmod_per1$lci <- srmod_per1$fit-(srmod_per1$se*1.96)
srmod_per1$uci <- srmod_per1$fit+(srmod_per1$se*1.96)
#view srmod_per1
head(srmod_per1)
summary(srmod_per)$coefficients
xper_labels <- with(srmod_per1, interaction(ab, burn_trt))
xper_labels <- str_replace_all(xper_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_per1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_per1$lci)), max(srmod_per1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_per1$lci, c(1:4), srmod_per1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xper_labels, tick=F, cex.axis=1)
title(main = "Perennial Species Richness")


#legume
srmod_leg<-glmer(leg~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_leg)
ndleg <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_leg1 <- predictSE(mod=srmod_leg,newdata=ndleg,type="response",se.fit = T)
srmod_leg1 <- data.frame(ndleg, fit = srmod_leg1$fit, se = srmod_leg1$se.fit)
srmod_leg1$lci <- srmod_leg1$fit-(srmod_leg1$se*1.96)
srmod_leg1$uci <- srmod_leg1$fit+(srmod_leg1$se*1.96)
#view srmod_leg1
head(srmod_leg1)
summary(srmod_leg)$coefficients
xleg_labels <- with(srmod_leg1, interaction(ab, burn_trt))
xleg_labels <- str_replace_all(xleg_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_leg1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_leg1$lci)), max(srmod_leg1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_leg1$lci, c(1:4), srmod_leg1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xleg_labels, tick=F, cex.axis=1)
title(main = "Legume Species Richness")

#tree #no isSingular error?
srmod_tree<-glmer(tree~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_tree)
ndtree <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_tree1 <- predictSE(mod=srmod_tree,newdata=ndtree,type="response",se.fit = T)
srmod_tree1 <- data.frame(ndtree, fit = srmod_tree1$fit, se = srmod_tree1$se.fit)
srmod_tree1$lci <- srmod_tree1$fit-(srmod_tree1$se*1.96)
srmod_tree1$uci <- srmod_tree1$fit+(srmod_tree1$se*1.96)
#view srmod_tree1
head(srmod_tree1)
summary(srmod_tree)$coefficients
xtree_labels <- with(srmod_tree1, interaction(ab, burn_trt))
xtree_labels <- str_replace_all(xtree_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_tree1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_tree1$lci)), max(srmod_tree1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_tree1$lci, c(1:4), srmod_tree1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xtree_labels, tick=F, cex.axis=1)
title(main = "Tree Species Richness")

#shrub #negative lci?
srmod_shr<-glmer(shrub~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_shr)
ndshr <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                      burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_shr1 <- predictSE(mod=srmod_shr,newdata=ndshr,type="response",se.fit = T)
srmod_shr1 <- data.frame(ndshr, fit = srmod_shr1$fit, se = srmod_shr1$se.fit)
srmod_shr1$lci <- srmod_shr1$fit-(srmod_shr1$se*1.96)
srmod_shr1$uci <- srmod_shr1$fit+(srmod_shr1$se*1.96)
#view srmod_shr1 #only ci for burn below
head(srmod_shr1)
summary(srmod_shr)$coefficients
xshr_labels <- with(srmod_shr1, interaction(ab, burn_trt))
xshr_labels <- str_replace_all(xshr_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_shr1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_shr1$lci)), max(srmod_shr1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_shr1$lci, c(1:4), srmod_shr1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xshr_labels, tick=F, cex.axis=1)
title(main = "Shrub Species Richness")


#forb
srmod_for<-glmer(forb~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_for)
ndfor <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_for1 <- predictSE(mod=srmod_for,newdata=ndfor,type="response",se.fit = T)
srmod_for1 <- data.frame(ndfor, fit = srmod_for1$fit, se = srmod_for1$se.fit)
srmod_for1$lci <- srmod_for1$fit-(srmod_for1$se*1.96)
srmod_for1$uci <- srmod_for1$fit+(srmod_for1$se*1.96)
#view srmod_for1
head(srmod_for1)
summary(srmod_for)$coefficients
xfor_labels <- with(srmod_for1, interaction(ab, burn_trt))
xfor_labels <- str_replace_all(xfor_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_for1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_for1$lci)), max(srmod_for1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_for1$lci, c(1:4), srmod_for1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xfor_labels, tick=F, cex.axis=1)
title(main = "Forb Species Richness")


#grass
srmod_gra<-glmer(grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_gra)
ndgra <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_gra1 <- predictSE(mod=srmod_gra,newdata=ndgra,type="response",se.fit = T)
srmod_gra1 <- data.frame(ndgra, fit = srmod_gra1$fit, se = srmod_gra1$se.fit)
srmod_gra1$lci <- srmod_gra1$fit-(srmod_gra1$se*1.96)
srmod_gra1$uci <- srmod_gra1$fit+(srmod_gra1$se*1.96)
#view srmod_gra1
head(srmod_gra1)
summary(srmod_gra)$coefficients
xgra_labels <- with(srmod_gra1, interaction(ab, burn_trt))
xgra_labels <- str_replace_all(xgra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_gra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_gra1$lci)), max(srmod_gra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_gra1$lci, c(1:4), srmod_gra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xgra_labels, tick=F, cex.axis=1)
title(main = "Grass Species Richness")

#sedge
srmod_sed<-glmer(sedge~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_sed)
ndsed <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_sed1 <- predictSE(mod=srmod_sed,newdata=ndsed,type="response",se.fit = T)
srmod_sed1 <- data.frame(ndsed, fit = srmod_sed1$fit, se = srmod_sed1$se.fit)
srmod_sed1$lci <- srmod_sed1$fit-(srmod_sed1$se*1.96)
srmod_sed1$uci <- srmod_sed1$fit+(srmod_sed1$se*1.96)
#view srmod_sed1
head(srmod_sed1)
summary(srmod_sed)$coefficients
xsed_labels <- with(srmod_sed1, interaction(ab, burn_trt))
xsed_labels <- str_replace_all(xsed_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_sed1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_sed1$lci)), max(srmod_sed1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_sed1$lci, c(1:4), srmod_sed1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xsed_labels, tick=F, cex.axis=1)
title(main = "Sedge Species Richness")


#native_grass
srmod_natgra<-glmer(native_grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_natgra)
ndnatgra <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                     burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_natgra1 <- predictSE(mod=srmod_natgra,newdata=ndnatgra,type="response",se.fit = T)
srmod_natgra1 <- data.frame(ndnatgra, fit = srmod_natgra1$fit, se = srmod_natgra1$se.fit)
srmod_natgra1$lci <- srmod_natgra1$fit-(srmod_natgra1$se*1.96)
srmod_natgra1$uci <- srmod_natgra1$fit+(srmod_natgra1$se*1.96)
#view srmod_natgra1
head(srmod_natgra1)
summary(srmod_natgra)$coefficients
xnatgra_labels <- with(srmod_natgra1, interaction(ab, burn_trt))
xnatgra_labels <- str_replace_all(xnatgra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_natgra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natgra1$lci)), max(srmod_natgra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_natgra1$lci, c(1:4), srmod_natgra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xnatgra_labels, tick=F, cex.axis=1)
title(main = "Native Grass Species Richness")


#exotic_grass
srmod_exogra<-glmer(exotic_grass~ab+burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_exogra)
ndexogra <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                        burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_exogra1 <- predictSE(mod=srmod_exogra,newdata=ndexogra,type="response",se.fit = T)
srmod_exogra1 <- data.frame(ndexogra, fit = srmod_exogra1$fit, se = srmod_exogra1$se.fit)
srmod_exogra1$lci <- srmod_exogra1$fit-(srmod_exogra1$se*1.96)
srmod_exogra1$uci <- srmod_exogra1$fit+(srmod_exogra1$se*1.96)
#view srmod_exogra1
head(srmod_exogra1)
summary(srmod_exogra)$coefficients
xexogra_labels <- with(srmod_exogra1, interaction(ab, burn_trt))
xexogra_labels <- str_replace_all(xexogra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_exogra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exogra1$lci)), max(srmod_exogra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exogra1$lci, c(1:4), srmod_exogra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xexogra_labels, tick=F, cex.axis=1)
title(main = "Exotic Grass Species Richness")

     
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


