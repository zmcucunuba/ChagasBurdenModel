#==================================================
#        CLEANING POPULATION DATA
#           municipality level
#==================================================
# Julia Ledien, 21/09/2020
library(readxl)


###########################
#Alive population

brut<- read_xlsx("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")
dico<- read_xlsx("data/dem/dictionnaire ADM2 Names.xlsx")

brut<- merge (brut, dico, by.x="DPMP", by.y="DPMP", all.x=T)

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

municipalities <- unique(dico$GID_2_GDAM[is.na(dico$GID_2_GDAM)==F])

for (mun in municipalities){
  
  urb <- t(as.matrix(subset(Urban[,7:107], Urban$GID_2_GDAM==mun)))
  colnames(urb)<- 1985:2035
  saveRDS(urb, paste0("data/dem/ADM2_Obs_Alive/urban_",mun) )
  
  rur <- t(as.matrix(subset(Rural[,7:107], Rural$GID_2_GDAM==mun)))
  colnames(rur)<- 1985:2035
  saveRDS(rur, paste0("data/dem/ADM2_Obs_Alive/rural_",mun) )
  
  tot <- t(as.matrix(subset(Total[,7:107], Total$GID_2_GDAM==mun)))
  colnames(tot)<- 1985:2035
  saveRDS(tot, paste0("data/dem/ADM2_Obs_Alive/total_",mun) )
}

############################
# Death population


setwd("~/GitHub/ChagasBurdenModel")
dico<- read_xlsx("data/dem/dictionnaire ADM2 Names.xlsx")
municipalities <- unique(dico$GID_2_GDAM[is.na(dico$GID_2_GDAM)==F])

setwd("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

file_list <- list.files("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")


##Total
AggDeaths<- data.frame(DPMP=NA, gru_ed1=NA, ano=NA, x=NA)

for (file in file_list){
 rawDeath <- read.csv(file, header=T,fill=T )
 colnames(rawDeath)<- lapply(colnames(rawDeath), tolower)
 rawDeath$CodeDep<- ifelse(nchar(as.character(rawDeath$cod_dpto))==1, paste0("0",rawDeath$cod_dpto), as.character(rawDeath$cod_dpto) )
 rawDeath$CodeMun<- ifelse(nchar(as.character(rawDeath$cod_munic))==1, paste0("00",rawDeath$cod_munic), 
                           ifelse(nchar(as.character(rawDeath$cod_munic))==2,paste0("0",rawDeath$cod_munic), as.character(rawDeath$cod_munic) ))
 rawDeath$DPMP<- paste0(rawDeath$CodeDep, rawDeath$CodeMun)
 rawDeath$x<- 1
 data<- aggregate(x ~ DPMP + gru_ed1 +ano , data = rawDeath, sum)
 AggDeaths<- rbind(AggDeaths, data)
 }
setwd("~/GitHub/ChagasBurdenModel")

AgeClass<- data.frame(AgeClass=1:29)
for (mun in municipalities){
  data_t<- subset(AggDeaths, AggDeaths$DPMP==dico$DPMP[which(dico$GID_2_GDAM==mun)])
  dt<- AgeClass
  for (y in 1979:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  colnames(dt)<- c("AgeClass", 1979:2018)
  saveRDS(dt, paste0("data/dem/ADM2_Obs_Deaths/total_",mun))
}

##Urban
AggDeaths<- data.frame(DPMP=NA, gru_ed1=NA, ano=NA, x=NA)
setwd("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")
file_list <- list.files("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

for (file in file_list){
  rawDeath <- read.csv(file, header=T,fill=T )
  colnames(rawDeath)<- lapply(colnames(rawDeath), tolower)
  rawDeath$CodeDep<- ifelse(nchar(as.character(rawDeath$cod_dpto))==1, paste0("0",rawDeath$cod_dpto), as.character(rawDeath$cod_dpto) )
  rawDeath$CodeMun<- ifelse(nchar(as.character(rawDeath$cod_munic))==1, paste0("00",rawDeath$cod_munic), 
                            ifelse(nchar(as.character(rawDeath$cod_munic))==2,paste0("0",rawDeath$cod_munic), as.character(rawDeath$cod_munic) ))
  rawDeath$DPMP<- paste0(rawDeath$CodeDep, rawDeath$CodeMun)
  rawDeath<- subset(rawDeath, rawDeath$area_res==c(1, 2))#1 = Cabecera municipal, 2 = Centro poblado (Inspección, corregimiento o caserío) 
  rawDeath$x<- 1
  data<- aggregate(x ~ DPMP + gru_ed1 +ano , data = rawDeath, sum)
  AggDeaths<- rbind(AggDeaths, data)
}

setwd("~/GitHub/ChagasBurdenModel")

AgeClass<- data.frame(AgeClass=1:29)
for (mun in municipalities){
  data_t<- subset(AggDeaths, AggDeaths$DPMP==dico$DPMP[which(dico$GID_2_GDAM==mun)])
  dt<- AgeClass
  for (y in 1979:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  colnames(dt)<- c("AgeClass", 1979:2018)
  saveRDS(dt, paste0("data/dem/ADM2_Obs_Deaths/urban_",mun))
}

##Rural
AggDeaths<- data.frame(DPMP=NA, gru_ed1=NA, ano=NA, x=NA)
setwd("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")
file_list <- list.files("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

for (file in file_list){
  rawDeath <- read.csv(file, header=T,fill=T )
  colnames(rawDeath)<- lapply(colnames(rawDeath), tolower)
  rawDeath$CodeDep<- ifelse(nchar(as.character(rawDeath$cod_dpto))==1, paste0("0",rawDeath$cod_dpto), as.character(rawDeath$cod_dpto) )
  rawDeath$CodeMun<- ifelse(nchar(as.character(rawDeath$cod_munic))==1, paste0("00",rawDeath$cod_munic), 
                            ifelse(nchar(as.character(rawDeath$cod_munic))==2,paste0("0",rawDeath$cod_munic), as.character(rawDeath$cod_munic) ))
  rawDeath$DPMP<- paste0(rawDeath$CodeDep, rawDeath$CodeMun)
  rawDeath<- subset(rawDeath, rawDeath$area_res==3)#3 = Rural disperso 
  rawDeath$x<- 1
  data<- aggregate(x ~ DPMP + gru_ed1 +ano , data = rawDeath, sum)
  AggDeaths<- rbind(AggDeaths, data)
}

setwd("~/GitHub/ChagasBurdenModel")

AgeClass<- data.frame(AgeClass=1:29)
for (mun in municipalities){
  data_t<- subset(AggDeaths, AggDeaths$DPMP==dico$DPMP[which(dico$GID_2_GDAM==mun)])
  dt<- AgeClass
  for (y in 1979:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  colnames(dt)<- c("AgeClass", 1979:2018)
  saveRDS(dt, paste0("data/dem/ADM2_Obs_Deaths/rural_",mun))
}