#Caitlin's script - script development - no longer need in main script


#import plant full (already imported and subsetted to soil core sites)
#pfull<-read.table("01_data/plant_full.txt",header=T)
#head(pfull,4);dim(pfull)

#extract from pfull unique quadrats

soilq<-unique(sdata$quadratID)
soilq


#check that soilqs exist in pfull - should be 0, none missing

missingq<-sdata[which(!soilq %in% unique(pfull$quadratID)),]$quadratID

pfull <-read.table("01_data/plant_full.txt",header=T)

psoil<-pfull[which(pfull$quadratID %in% soilq),] #AG data for t37-46 quadratID's, including 1m and 5m quadrats
psoil1 <- subset(pfull, quadratID %in% soilq & cover == 1) #AG data for t37-46 quadratID's, excluding 5m quadrats
rownames(psoil)<-1:nrow(psoil)
#above-ground plant data for soil core sites
head(psoil);dim(psoil)
length(unique(psoil$sp))


#if need to write table, just run once below:
#write.table(psoil,file="01_data/psoil.txt",sep="\t",quote = F,row.names = F)


##
#merging datasets
library(tidyr)
merged_data <- merge(tdata, combospec, by = "code", all.x = TRUE)
merged_data <- merge(AGdata, combospec, by.x = "sp", by.y = "code", all.x = T)
merged_data <- merge(merged_data, sdata, by = "quadratID", all.x = TRUE)
#merged sitexspec matrix - code and species in different forms?
merged_ssm <- pivot_wider(merged_data,
                          id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                          names_from = "species",
                          values_from = "count",
                          values_fn = list(count = sum), #using quadratID results in duplicates due to separate entries for trays A and B, have summed
                          values_fill = list(count = 0))


#merged_id
mergedID_data <-merged_data[merged_data$speciesID == 1, ]
mergedID_ssm <- pivot_wider(mergedID_data,
                            id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                            names_from = "species",
                            values_from = "count",
                            values_fn = list(count = sum),
                            values_fill = list(count = 0))
#AG mergedID 
AGmergedID_data <- mergedID_data[mergedID_data$location.y == 1 | mergedID_data$location.y ==2, ]
AGmergedID_ssm <- pivot_wider(AGmergedID_data,
                              id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                              names_from = "species",
                              values_from = "count",
                              values_fn = list(count = sum),
                              values_fill = list(count = 0))

#BG mergedID
BGmergedID_data <- mergedID_data[mergedID_data$location.y == 0 | mergedID_data$location.y == 2, ]
BGmergedID_ssm <- pivot_wider(AGmergedID_data,
                              id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                              names_from = "species",
                              values_from = "count",
                              values_fn = list(count = sum),
                              values_fill = list(count = 0))

#remove columns
#div1 <- subset(div1, select = -agmarg)


#---- initial ssm, shannon, sr, simps attempt
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
#-------

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



div3 <- div2[,1:(ncol(div2)-1)]
div3$ab <- "above"
colnames(div3) [which(colnames(div3) == "agsr")] <- "sr"

head(div3);dim(div3)

div4 <- div2[,c(1:5,ncol(div2))]
div4$ab <- "below"
colnames(div4) [which(colnames(div4) == "bgsr")] <- "sr"
head(div4);dim(div4)



#sr_mod2
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

#alpha glmer
nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_all1 <- predictSE(mod=srmod_all,newdata=nd1,type="response",se.fit = T)
srmod_all1 <- data.frame(nd1, fit = srmod_all1$fit, se = srmod_all1$se.fit)
srmod_all1$lci <- srmod_all1$fit-(srmod_all1$se*1.96)
srmod_all1$uci <- srmod_all1$fit+(srmod_all1$se*1.96)
#view srmod_nat1
head(srmod_all1)
summary(srmod_all)$coefficients
xall_labels <- with(srmod_all1, interaction(ab, burn_trt))
xall_labels <- str_replace_all(xall_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(1,2))
plot(c(1:4), srmod_all1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_all1$lci)), max(srmod_all1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_all1$lci, c(1:4), srmod_all1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xall_labels, tick=F, cex.axis=1)
title(main = "Alpha Species Richness")

#native
srmod_nat1 <- predictSE(mod=srmod_nat,newdata=nd1,type="response",se.fit = T)
srmod_nat1 <- data.frame(nd1, fit = srmod_nat1$fit, se = srmod_nat1$se.fit)
srmod_nat1$lci <- srmod_nat1$fit-(srmod_nat1$se*1.96)
srmod_nat1$uci <- srmod_nat1$fit+(srmod_nat1$se*1.96)
#view srmod_nat1
head(srmod_nat1)
summary(srmod_nat)$coefficients
xnat_labels <- with(srmod_nat1, interaction(ab, burn_trt))
xnat_labels <- str_replace_all(xnat_labels, c("above\\.Control" = "AG Control", "below\\.Control" = "BG Control", "above\\.Burn" = "AG Burn", "below\\.Burn" = "BG Burn"))
#dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_nat1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nat1$lci)), max(srmod_nat1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_nat1$lci, c(1:4), srmod_nat1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xnat_labels, tick=F, cex.axis=0.8)
title(main = "Native Species Richness")

#exotic
srmod_exo1 <- predictSE(mod=srmod_exo,newdata=nd1,type="response",se.fit = T)
srmod_exo1 <- data.frame(nd1, fit = srmod_exo1$fit, se = srmod_exo1$se.fit)
srmod_exo1$lci <- srmod_exo1$fit-(srmod_exo1$se*1.96)
srmod_exo1$uci <- srmod_exo1$fit+(srmod_exo1$se*1.96)
#view srmod_exo1
head(srmod_exo1)
summary(srmod_exo)$coefficients
xexo_labels <- with(srmod_exo1, interaction(ab, burn_trt))
xexo_labels <- str_replace_all(xexo_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
#
plot(c(1:4), srmod_exo1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exo1$lci)), max(srmod_exo1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exo1$lci, c(1:4), srmod_exo1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xexo_labels, tick=F, cex.axis=1)
title(main = "Exotic Species Richness")

#annual
srmod_ann1 <- predictSE(mod=srmod_ann,newdata=nd1,type="response",se.fit = T)
srmod_ann1 <- data.frame(nd1, fit = srmod_ann1$fit, se = srmod_ann1$se.fit)
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
srmod_per1 <- predictSE(mod=srmod_per,newdata=nd1,type="response",se.fit = T)
srmod_per1 <- data.frame(nd1, fit = srmod_per1$fit, se = srmod_per1$se.fit)
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
srmod_leg1 <- predictSE(mod=srmod_leg,newdata=nd1,type="response",se.fit = T)
srmod_leg1 <- data.frame(nd1, fit = srmod_leg1$fit, se = srmod_leg1$se.fit)
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

#tree
srmod_tree1 <- predictSE(mod=srmod_tree,newdata=nd1,type="response",se.fit = T)
srmod_tree1 <- data.frame(nd1, fit = srmod_tree1$fit, se = srmod_tree1$se.fit)
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

#shrub
srmod_shr1 <- predictSE(mod=srmod_shr,newdata=nd1,type="response",se.fit = T)
srmod_shr1 <- data.frame(nd1, fit = srmod_shr1$fit, se = srmod_shr1$se.fit)
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
srmod_for1 <- predictSE(mod=srmod_for,newdata=nd1,type="response",se.fit = T)
srmod_for1 <- data.frame(nd1, fit = srmod_for1$fit, se = srmod_for1$se.fit)
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
srmod_gra1 <- predictSE(mod=srmod_gra,newdata=nd1,type="response",se.fit = T)
srmod_gra1 <- data.frame(nd1, fit = srmod_gra1$fit, se = srmod_gra1$se.fit)
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
srmod_sed1 <- predictSE(mod=srmod_sed,newdata=nd1,type="response",se.fit = T)
srmod_sed1 <- data.frame(nd1, fit = srmod_sed1$fit, se = srmod_sed1$se.fit)
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

#native grass
srmod_natgra1 <- predictSE(mod=srmod_natgra,newdata=nd1,type="response",se.fit = T)
srmod_natgra1 <- data.frame(nd1, fit = srmod_natgra1$fit, se = srmod_natgra1$se.fit)
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

#exotic grass
srmod_exogra1 <- predictSE(mod=srmod_exogra,newdata=nd1,type="response",se.fit = T)
srmod_exogra1 <- data.frame(nd1, fit = srmod_exogra1$fit, se = srmod_exogra1$se.fit)
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

#biplot(pca1, xlab="Component 1",ylab="Component 2",col=c("grey40","black"),var.axes=T,arrow.len=0.1, c(3:n), c(4:n))
#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, c(seq(3, 60)), c(seq(4, 60)))
#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, c(3:60), c(4:60))


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




for (i in 1:nrow(group.df)) {
  
  name.thisrun <- as.character(group.df$group[i])
  vec.thisrun <- get(name.thisrun)
  
  print(vec.thisrun)  # Print vec.thisrun
  
  if (length(vec.thisrun) == 0) {
    cat("Warning: Empty vector.\n")
    next  # Skip to next iteration if vec.thisrun is empty
  }
  
  if (substr(name.thisrun, 1, 3) == "BG.") data.thisrun <- BGmat[, colnames(BGmat) %in% vec.thisrun]
  if (substr(name.thisrun, 1, 3) == "AG.") data.thisrun <- AGmat[, colnames(AGmat) %in% vec.thisrun]
  
  print(dim(data.thisrun))
  
  if (is.null(dim(data.thisrun))) {
    if (substr(name.thisrun, 1, 3) == "AG.") rownames.thisrun <- rownames(AGmat) else rownames.thisrun <- rownames(BGmat)
    data.thisrun <- data.frame(data.thisrun) 
    colnames(data.thisrun) <- vec.thisrun
    rownames(data.thisrun) <- rownames.thisrun
  }
  
  # Convert non-numeric data to NA
  data.thisrun <- apply(data.thisrun, 2, function(x) {
    if (all(is.na(as.numeric(x)))) {
      return(NA)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Check for NA values
  if (any(is.na(data.thisrun))) {
    cat("Warning: Non-numeric values found in data.\n")
    # Handle or remove NA values as needed
    # For simplicity, we'll just remove rows with NA values here
    data.thisrun <- na.omit(data.thisrun)
  }
  
  #rich.data[[i]] <- apply(data.thisrun, 1, function(x) length(which(x > 0)))
  
  simp.data[[i]] <- diversity(data.thisrun, index = "invsimpson")
  
} # close i for

simp.res <-data.frame(do.call(cbind,simp.data))
groupnames <- tail(group.df$group, 34)[-c(1:2)]
colnames(simp.res)<-groupnames



for (i in 1:nrow(group.df)) {
  
  name.thisrun <- as.character(group.df$group[i])
  vec.thisrun <- get(name.thisrun)
  
  print(vec.thisrun)  # Print vec.thisrun
  
  if (length(vec.thisrun) == 0) {
    cat("Warning: Empty vector.\n")
    next  # Skip to next iteration if vec.thisrun is empty
  }
  
  if (substr(name.thisrun, 1, 3) == "BG.") data.thisrun <- BGmat[, colnames(BGmat) %in% vec.thisrun]
  if (substr(name.thisrun, 1, 3) == "AG.") data.thisrun <- AGmat[, colnames(AGmat) %in% vec.thisrun]
  
  print(dim(data.thisrun))
  
  if (is.null(dim(data.thisrun))) {
    if (substr(name.thisrun, 1, 3) == "AG.") rownames.thisrun <- rownames(AGmat) else rownames.thisrun <- rownames(BGmat)
    data.thisrun <- data.frame(data.thisrun) 
    colnames(data.thisrun) <- vec.thisrun
    rownames(data.thisrun) <- rownames.thisrun
  }
  
  # Convert non-numeric data to NA
  data.thisrun <- apply(data.thisrun, 2, function(x) {
    if (all(is.na(as.numeric(x)))) {
      return(NA)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Check for NA values
  if (any(is.na(data.thisrun))) {
    cat("Warning: Non-numeric values found in data.\n")
    # Handle or remove NA values as needed
    # For simplicity, we'll just remove rows with NA values here
    data.thisrun <- na.omit(data.thisrun)
  }
  
  # Check for Inf values after diversity calculation
  simp_val <- diversity(data.thisrun, index = "invsimpson")
  if (any(is.infinite(simp_val))) {
    cat("Warning: Infinite value detected for group", name.thisrun, "\n")
    # Replace infinite values with zero
    simp_val[is.infinite(simp_val)] <- 0
  }
  
  simp.data[[i]] <- simp_val
  
} # close i for