###################
## Calculate prevalence total for a certain year and create a shapefile

####### To be defined
year<- 2020
object <- "Median_CasesTotal"
setting= "urban"
it = 100
#######
 library(readxl)

dataBurden<- readRDS(paste("res/summaries/ADM2", object, setting, it, "it", sep="_"))

dataPop<- matrix(NA, 1065,36)
rownames(dataPop)<- municipalities

for (m in municipalities){
  d<- readRDS(paste0("data/dem/ADM2_Obs_Alive/", setting, "_", m))
  for (i in 1:36){
    dataPop[m,i]<- sum(d[,i])
  }
}

Med_Prev <- matrix(NA,1065,36)
rownames(Med_Prev)<- municipalities
for(m in municipalities){
  for (i in 1:36){
    Med_Prev [m,i]<- dataBurden[m,i]/ dataPop[m,i]
  }
}
Med_Prev<- as.data.frame(Med_Prev)
colnames(Med_Prev)<- paste0("FoI", 1885:2020)
Med_Prev[c("COL.32.4._1", "COL.32.5_1"),]<- Med_Prev["COL.32.1_1",]
Med_Prev$ADM2<- rownames(Med_Prev)
COL <- readRDS("C:/Users/Julia L/Documents/GitHub/chagas-ML/data/Colombia BackgroundMaps/format sp/gadm36_COL_2_sp.rds")
COL@data<- data.frame(COL@data, Med_Prev[match(COL@data[,"GID_2"], Med_Prev[,"ADM2"])])
writeOGR(COL, paste("res/shapefiles/Prevalence", object,setting, it,"it", sep="_"), layer=paste("Prevalence", object, setting, it, "it", sep="_"), driver = "ESRI Shapefile")

