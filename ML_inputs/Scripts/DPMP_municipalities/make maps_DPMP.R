
## draft make maps
library(rgdal)
ColombiaADM2<-readOGR("ML_inputs/Data/HUMDATA_col_adm_dane_2020_shp/col_admbnda_adm2_mgn_20200416.shp")

#nb of cases in 1990 and 2010

# Creating shapefiles
###RURAL
NbCases_ACt_StI<- readRDS("ML_inputs/res/NbCases_ACt_StI_rural_100_it")
nbC1990<- NbCases_ACt_StI[,,6]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_1990_rural_100it", layer="NbCases_ACt_StI_1990_rural_100it", driver="ESRI Shapefile")


nbC2010<- NbCases_ACt_StI[,,26]
nbC2010<- as.data.frame(nbC2010)
nbC2010$ADM2<- rownames(nbC2010)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2010[match(COL_t@data[,"ADM2_PCODE"], nbC2010[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2010_rural_100it", layer="NbCases_ACt_StI_2010_rural_100it", driver="ESRI Shapefile")


nbC2020<- NbCases_ACt_StI[,,36]
nbC2020<- as.data.frame(nbC2020)
nbC2020$ADM2<- rownames(nbC2020)
nbC2020$Uncert<- nbC2020$med/(nbC2020$Q75-nbC2020$Q25)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2020[match(COL_t@data[,"ADM2_PCODE"], nbC2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2020_rural_100it", layer="NbCases_ACt_StI_2020_rural_100it_un", driver="ESRI Shapefile")

###National summaries:
National_NbDeath_ACt_StI<- readRDS("ML_inputs/res/National_NbDeath_ACt_StI_rural_100_it")
summary(National_NbDeath_ACt_StI[,36]) # deaths in 2020
summary(National_NbDeath_ACt_StI[,26]) # deaths in 2010

NbCases_NAtio<-  readRDS("ML_inputs/res/National_NbCases_ACt_StI_rural_100_it")
summary(NbCases_NAtio[,36]) # Cases in 2020
summary(NbCases_NAtio[,26]) # Cases in 2010
summary(NbCases_NAtio[,6]) # Cases in 1990

###URBAN
NbCases_ACt_StI<- readRDS("ML_inputs/res/NbCases_ACt_StI_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StI[,,6]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_1990_urbanBigCT_100_it", layer="NbCases_ACt_StI_1990_urbanBigCT_100_it", driver="ESRI Shapefile")

nbC2010<- NbCases_ACt_StI[,,26]
nbC2010<- as.data.frame(nbC2010)
nbC2010$ADM2<- rownames(nbC2010)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2010[match(COL_t@data[,"ADM2_PCODE"], nbC2010[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2010_urbanBigCT_100_it", layer="NbCases_ACt_StI_2010_urbanBigCT_100_it", driver="ESRI Shapefile")

nbC2020<- NbCases_ACt_StI[,,36]
nbC2020<- as.data.frame(nbC2020)
nbC2020$ADM2<- rownames(nbC2020)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2020[match(COL_t@data[,"ADM2_PCODE"], nbC2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2020_urbanBigCT_100_it", layer="NbCases_ACt_StI_2020_urbanBigCT_100_it", driver="ESRI Shapefile")

###National summaries:
National_NbDeath_ACt_StI<- readRDS("ML_inputs/res/National_NbDeath_ACt_StI_urbanBigCT_100_it")
summary(National_NbDeath_ACt_StI[,36]) # deaths in 2020
summary(National_NbDeath_ACt_StI[,26]) # deaths in 2010
summary(National_NbDeath_ACt_StI[,31]) # deaths in 2015


NbCases_NAtio<-  readRDS("ML_inputs/res/National_NbCases_ACt_StI_urbanBigCT_100_it")
summary(NbCases_NAtio[,36]) # Cases in 2020
summary(NbCases_NAtio[,26]) # Cases in 2010
summary(NbCases_NAtio[,6]) # Cases in 1990



library(rgdal)
ColombiaADM2<-readOGR("ML_inputs/Data/HUMDATA_col_adm_dane_2020_shp/col_admbnda_adm2_mgn_20200416.shp")

#nb of severe cases in 1990 and 2010

# Creating shapefiles
##2010
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,26]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2010_rural_100it", layer="NbCases_ACt_StChS_2010_rural_100it", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,26]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2010_urbanBigCT_100_it", layer="NbCases_ACt_StChS_2010_urbanBigCT_100_it", driver="ESRI Shapefile")

###total
rur<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
urb<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
NbCases_ACt_StChS<- rur +urb
nbC1990<- NbCases_ACt_StChS[,,26]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2010_totalBigCT_100it", layer="NbCases_ACt_StChS_2010_totalBigCT_100it", driver="ESRI Shapefile")

##1990
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,6]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_1990_rural_100it", layer="NbCases_ACt_StChS_1990_rural_100it", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,6]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_1990_urbanBigCT_100_it", layer="NbCases_ACt_StChS_1990_urbanBigCT_100_it", driver="ESRI Shapefile")

###total
rur<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
urb<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
NbCases_ACt_StChS<- rur +urb
nbC1990<- NbCases_ACt_StChS[,,6]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_1990_totalBigCT_100it", layer="NbCases_ACt_StChS_1990_totalBigCT_100it", driver="ESRI Shapefile")

#severe cases 2020
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2020_rural_100it", layer="NbCases_ACt_StChS_2020_rural_100it", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StChS_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StChS_2020_urbanBigCT_100_it", layer="NbCases_ACt_StChS_2020_urbanBigCT_100_it", driver="ESRI Shapefile")


#Chonic mild cases 2020
###RURAL
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StCh_rural_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StCh_2020_rural_100it", layer="NbCases_ACt_StCh_2020_rural_100it", driver="ESRI Shapefile")

###URBAN
NbCases_ACt_StChS<- readRDS("ML_inputs/res/NbCases_ACt_StCh_urbanBigCT_100_it")
nbC1990<- NbCases_ACt_StChS[,,36]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"ADM2_PCODE"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StCh_2020_urbanBigCT_100_it", layer="NbCases_ACt_StCh_2020_urbanBigCT_100_it", driver="ESRI Shapefile")

