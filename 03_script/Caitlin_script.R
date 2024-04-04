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