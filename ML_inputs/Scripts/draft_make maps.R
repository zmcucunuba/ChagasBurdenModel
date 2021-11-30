
## draft make maps
library(rgdal)
ColombiaADM2<-readOGR("ML_inputs/Data/GDAM/gadm36_COL_shp/gadm36_COL_2.shp")

#nb of cases in 1990 and 2010

# Creating shapefiles
nbC1990<- NbCases_ACt_StI[,,6]
nbC1990[c("COL.32.4_1","COL.32.5_1"),]<- nbC1990["COL.32.1_1",]
nbC1990<- as.data.frame(nbC1990)
nbC1990$ADM2<- rownames(nbC1990)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC1990[match(COL_t@data[,"GID_2"], nbC1990[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_1990_urban_100it", layer="NbCases_ACt_StI_1990_urban_100it", driver="ESRI Shapefile")

sum(nbC1990$med)
sum(nbC1990$min)
sum(nbC1990$max)


nbC2010<- NbCases_ACt_StI[,,26]
nbC2010[c("COL.32.4_1","COL.32.5_1"),]<- nbC2010["COL.32.1_1",]
nbC2010<- as.data.frame(nbC2010)
nbC2010$ADM2<- rownames(nbC2010)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2010[match(COL_t@data[,"GID_2"], nbC2010[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2010_urban_100it", layer="NbCases_ACt_StI_2010_urban_100it", driver="ESRI Shapefile")

sum(nbC2010$med)
sum(nbC2010$min)
sum(nbC2010$max)

nbC2020<- NbCases_ACt_StI[,,36]
nbC2020[c("COL.32.4_1","COL.32.5_1"),]<- nbC2020["COL.32.1_1",]
nbC2020<- as.data.frame(nbC2020)
nbC2020$ADM2<- rownames(nbC2020)
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,nbC2020[match(COL_t@data[,"GID_2"], nbC2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/NbCases_ACt_StI_2020_urban_100it", layer="NbCases_ACt_StI_2020_urban_100it", driver="ESRI Shapefile")

sum(nbC2020$med)
sum(nbC2020$min)
sum(nbC2020$max)


nbD1990<- NbDeath_ACt_StI[,,36]
nbD1990<- as.data.frame(nbD1990)
sum(nbD1990$med, na.rm=T)
sum(nbD1990$min, na.rm=T)
sum(nbD1990$max, na.rm=T)
