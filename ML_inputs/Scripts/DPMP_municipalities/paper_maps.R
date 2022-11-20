#### MAPS FOR THE PAPER

library(rgdal)
ColombiaADM2<-readOGR("ML_inputs/Data/HUMDATA_col_adm_dane_2020_shp/col_admbnda_adm2_mgn_20200416.shp")

## PREVALENCE MAPS


##NUMBER OD INFECTION MAPS 2020
###RURAL
NbCases_ACt_StI<- readRDS("ML_inputs/res/NbCases_ACt_StI_rural_100_it")
nbC2020<- NbCases_ACt_StI[,,36]
nbC2020<- as.data.frame(nbC2020)
nbC2020$ADM2<- rownames(nbC2020)
nbC2020$Uncert<- (nbC2020$Q75-nbC2020$Q25)/nbC2020$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2020[match(COL_t@data[,"ADM2_PCODE"], nbC2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2020_rural_100it", layer="NbCases_ACt_StI_2020_rural_100it_corrected", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StI<- readRDS("ML_inputs/res/NbCases_ACt_StI_urbanBigCT_100_it")
nbC2020<- NbCases_ACt_StI[,,36]
nbC2020<- as.data.frame(nbC2020)
nbC2020$ADM2<- rownames(nbC2020)
nbC2020$Uncert<- (nbC2020$Q75-nbC2020$Q25)/nbC2020$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2020[match(COL_t@data[,"ADM2_PCODE"], nbC2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2020_urbanBigCT_100it", layer="NbCases_ACt_StI_2020_urbanBigCT_100it_corrected", driver="ESRI Shapefile")


##NUMBER OF MILD, SEVERE AND DEATH MAPS

#severe cases 2020
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2020_rural_100it", layer="NbCases_ACt_StChS_2020_rural_100it_corrected", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2020_urbanBigCT_100_it", layer="NbCases_ACt_StChS_2020_urbanBigCT_100_it_corrected", driver="ESRI Shapefile")


#Chonic mild cases 2020
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StCh_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StCh_2020_rural_100it", layer="NbCases_ACt_StCh_2020_rural_100it_corrected", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StCh_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StCh_2020_urbanBigCT_100_it", layer="NbCases_ACt_StCh_2020_urbanBigCT_100_it_corrected", driver="ESRI Shapefile")

#deaths 2020
###RURAL
Nbdeaths_ACt_StI<- readRDS("ML_inputs/res/Nbdeath_ACt_StI_rural_100_it")
nbC1990<- Nbdeaths_ACt_StI[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbDeaths_ACt_StI_2020_rural_100it", layer="NbDeaths_ACt_StI_2020_rural_100it_corrected", driver="ESRI Shapefile")

###URBAN
Nbdeaths_ACt_StI<- readRDS("ML_inputs/res/NbDeath_ACt_StI_urbanBigCT_100_it")
nbC1990<- Nbdeaths_ACt_StI[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
nbC1990$Uncert<- (nbC1990$Q75-nbC1990$Q25)/nbC1990$med
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbDeaths_ACt_StI_2020_urbanBigCT_100_it", layer="NbDeaths_ACt_StI_2020_urbanBigCT_100_it_corrected", driver="ESRI Shapefile")
