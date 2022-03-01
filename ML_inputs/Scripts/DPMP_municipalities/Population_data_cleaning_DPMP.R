#==================================================
#        CLEANING POPULATION DATA
#           municipality level
#==================================================
# Julia Ledien, 21/09/2020
library(readxl)


###########################
#Alive population

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

municipalities <- unique(brut$DPMP)

for (mun in municipalities){
  
  urb <- t(as.matrix(subset(Urban[,7:107], Urban$DPMP==mun)))
  colnames(urb)<- 1985:2035
  saveRDS(urb, paste0("ML_inputs/data/dem/ADM2_Obs_Alive_DPMP/urban_CO",mun) )
  
  rur <- t(as.matrix(subset(Rural[,7:107], Rural$DPMP==mun)))
  colnames(rur)<- 1985:2035
  saveRDS(rur, paste0("ML_inputs/data/dem/ADM2_Obs_Alive_DPMP/rural_CO",mun) )
  
  tot <- t(as.matrix(subset(Total[,7:107], Total$DPMP==mun)))
  colnames(tot)<- 1985:2035
  saveRDS(tot, paste0("ML_inputs/data/dem/ADM2_Obs_Alive_DPMP/total_CO",mun) )
}

############################
# Death population


setwd("~/GitHub/ChagasBurdenModel")
dico<- read.csv("data/dem/dictionnaire_ADM2_Names_COL")
#municipalities <- unique(dico$DPMP[is.na(dico$DPMP)==F])

setwd("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

file_list <- list.files("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")


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
  data_t<- subset(AggDeaths, AggDeaths$DPMP==mun)
  dt<- AgeClass
  for (y in 1979:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  colnames(dt)<- c("AgeClass", 1979:2018)
  saveRDS(dt, paste0("ML_inputs/data/dem/ADM2_Obs_Deaths_DPMP/total_CO",mun))
}

##Urban
AggDeaths<- data.frame(DPMP=NA, gru_ed1=NA, ano=NA, x=NA)
setwd("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")
file_list <- list.files("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

#area_res variable available from 1992
for (file in file_list){
  rawDeath <- read.csv(file, header=T,fill=T )
  colnames(rawDeath)<- lapply(colnames(rawDeath), tolower)
  rawDeath$CodeDep<- ifelse(nchar(as.character(rawDeath$cod_dpto))==1, paste0("0",rawDeath$cod_dpto), as.character(rawDeath$cod_dpto) )
  rawDeath$CodeMun<- ifelse(nchar(as.character(rawDeath$cod_munic))==1, paste0("00",rawDeath$cod_munic), 
                            ifelse(nchar(as.character(rawDeath$cod_munic))==2,paste0("0",rawDeath$cod_munic), as.character(rawDeath$cod_munic) ))
  rawDeath$DPMP<- paste0(rawDeath$CodeDep, rawDeath$CodeMun)
  rawDeath2<- subset(rawDeath, rawDeath$area_res==c(1, 2))#1 = Cabecera municipal, 2 = Centro poblado (Inspección, corregimiento o caserío) 
  rawDeath$x<- 1
  data<- aggregate(x ~ DPMP + gru_ed1 +ano , data = rawDeath, sum)
  AggDeaths<- rbind(AggDeaths, data)
}

setwd("~/GitHub/ChagasBurdenModel")

AgeClass<- data.frame(AgeClass=1:29)
for (mun in municipalities){
  data_t<- subset(AggDeaths, as.numeric(AggDeaths$DPMP)==mun)
  dt<- AgeClass
  for (y in 1992:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  dtt<- matrix(0,29,7)
  dt<-cbind(dt,dtt)
  colnames(dt)<- c("AgeClass", 1992:2018, 1985:1991)
  saveRDS(dt, paste0("ML_inputs/data/dem/ADM2_Obs_Deaths_DPMP/urban_CO",mun))
}

##Rural
AggDeaths<- data.frame(DPMP=NA, gru_ed1=NA, ano=NA, x=NA)
setwd("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")
file_list <- list.files("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Deaths/CSV/")

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
  data_t<- subset(AggDeaths, as.numeric(AggDeaths$DPMP)==mun)
  dt<- AgeClass
  for (y in 1992:2018){
    data_tt<- subset(data_t, data_t$ano==y)
    data_tt <- merge(data_tt, AgeClass, by.x= "gru_ed1", by.y="AgeClass", all.y=T)
    data_tt$x[which(is.na(data_tt$x)==T)]<-0
    dt <- cbind(dt,data_tt$x)
    
  }
  dtt<- matrix(0,29,7)
  dt<-cbind(dt,dtt)
  colnames(dt)<- c("AgeClass", 1992:2018, 1985:1991)
  saveRDS(dt, paste0("ML_inputs/data/dem/ADM2_Obs_Deaths_DPMP/rural_CO",mun))
}
