#Caitlin's script - script development - no longer need in main script


#import plant full (already imported and subsetted to soil core sites)
#pfull<-read.table("01_data/plant_full.txt",header=T)
#head(pfull,4);dim(pfull)

#extract from pfull unique quadrats

soilq<-unique(sdata$quadratID)
soilq


#check that soilqs exist in pfull - should be 0, none missing

missingq<-sdata[which(!soilq %in% unique(pfull$quadratID)),]$quadratID


psoil<-pfull[which(pfull$quadratID %in% soilq),]
rownames(psoil)<-1:nrow(psoil)
#above-ground plant data for soil core sites
head(psoil);dim(psoil)

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