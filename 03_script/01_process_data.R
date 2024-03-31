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
psabove_all<-read.table("01_data/plantspeciesabove.txt",header=T)

#plant data for soil site above-ground data
soilabovesp<-unique(soilabove$sp)
psabove_soil<-psabove_all[which(psabove_all$sp %in% soilabovesp),]

#combined species list of above and below ground, including non identified
combospec<-read.table("01_data/combinedspecies.txt",header=T)
head(combospec,4);dim(combospec)

#check that quadratID's match across 4 datasets
head(soilabove,4);dim(soilabove)
head(sdata,4);dim(sdata)
head(tdata);dim(tdata)
head(specieslist,4);dim(specieslist)
head(specieslistID,4);dim(specieslistID)
head(psabove_all,4);dim(psabove_all)
head(psabove_soil,4);dim(psabove_soil)
head(combospec,4);dim(combospec)

#check soilabove exists in site data
table(soilabove$quadratID %in% sdata$quadratID)
#all tray data exists in site data
table(tdata$quadratID %in% sdata$quadratID)

table(soilabove$sp %in% specieslistID$code)
#all species codes in plant data exist in tray data
table(specieslist$code %in% tdata$code)
which(!specieslist$code %in% unique(tdata$code))

#which species codes in plant data exist in above-ground veg data
table(specieslistID$code %in% psabove$sp)
which(specieslistID$code %in% unique(psabove$sp))
#which species codes in below-ground species list exist in above-ground veg data
specieslistID[which(!specieslistID$code %in% psabove$sp)[11:21],]
psabove[which(psabove$species=="Sonchus oleraceus"),]

#no. species not in below ground data (false)
table(psabove_soil$sp %in% specieslistID$code)

# VENN diagram:
library(VennDiagram)

# Summarise overlap:
belowonly<-21+47
aboveonly<-49+47
overlap<-47

dev.new(width=6,height=4,dpi=160,pointsize=12, noRStudioGD = T)

par(mar=c(4,4,4,4))

venn.plot<-draw.pairwise.venn(belowonly,aboveonly,overlap, category=c("Below","Above"),scaled=F,fill=rgb(0,0,0,0.5),fontfamily="sans",cat.fontfamily="sans",cex=1, cat.pos=c(2,10),lwd=1)

pdf(file="venn.pdf",width=4,height=4,pointsize=12)

par(mar=c(4,4,1,1))

grid.draw(venn.plot)

dev.off()

#site by species matrix? attempted to use tdata and add species names from combospec, however matrix used codes as collumns - reformatting using pivot_longer returned confusing result
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

#----------------------
#merging datasets
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

#species richness? wait this sums above and below data (1 and 2 or 0 and 2) wait but its based on tray data counts not plant data - so its only BG
AG.id_sr <- aggregate(count ~ species, data = AGmergedID_data, FUN = sum)
BG.id_sr <- aggregate(count ~ species, data = BGmergedID_data, FUN = sum)

#transect richness
transectAG.id_sr <-aggregate(count ~ transect.x + species, data = AGmergedID_data, FUN = sum)
transectBG.id_sr <- aggregate(count ~ transect.x + species, data = BGmergedID_data, FUN = sum)
  
#above-ground (AG) site by species matrix id - species or code - code includes NA?, code as tdata doesn't have species?
AG_id<-combospec$code[which(combospec$location == "1" | combospec$location =="2" & combospec$speciesID == "1")]
#above-ground (AG) sitexspec matrix all
AG<-combospec$code[which(combospec$location == "1" | combospec$location =="2")]

#below-ground (BG) site by species matrix id - whats with the NA's -same as all
BG_id<-combospec$species[which(combospec$location == "0" | combospec$location == "2" & combospec$speciesID == "1")]
#below-ground sitexspec matrix all
BG<-combospec$species[which(combospec$location == "0" | combospec$location == "2")]



