## prevalence in under 5 years old
library(rgdal)
library(readxl)
library(ggplot2)

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

ColombiaADM2<-readOGR("ML_inputs/Data/HUMDATA_col_adm_dane_2020_shp/col_admbnda_adm2_mgn_20200416.shp")

NbCases_AC1_StI_rur<- readRDS("ML_inputs/res/NbCases_AC1_StI_rural_100_it")
NbCases_AC1_StI_urb<- readRDS("ML_inputs/res/NbCases_AC1_StI_urbanBigCT_100_it")

years<- 1985:2020
munic<- ColombiaADM2$ADM2_PCODE

#Population size by municipality in 2020
Pop5Y2020<- matrix(NA, length(munic), 3)
for (i in 1:length(munic)){
  Pop5Y2020[i,]<- c(munic[i],sum(Urban[which(Urban$AÑO==2020 & paste0("CO",Urban$DPMP) == munic[i]),7:12]),
                       sum(Rural[which(Rural$AÑO==2020 & paste0("CO",Rural$DPMP) == munic[i]),7:12]))
}
Pop5Y2020<- as.data.frame(Pop5Y2020)
colnames(Pop5Y2020)<- c("ADM2", "UrbanPop", "RuralPop")
Pop5Y2020$UrbanPop<- as.numeric(Pop5Y2020$UrbanPop)
Pop5Y2020$RuralPop<- as.numeric(Pop5Y2020$RuralPop)
Pop5Y2020$TotalPop<- Pop5Y2020$UrbanPop + Pop5Y2020$RuralPop

Prevalence_5Y_2020<- matrix(NA, length(munic), 10)

for (i in 1:length(munic)){
  Prevalence_5Y_2020[i,]<- c(munic[i],
                         NbCases_AC1_StI_rur[munic[i],3,36]/Pop5Y2020$RuralPop[which(Pop5Y2020$ADM2==munic[i])],
                         NbCases_AC1_StI_rur[munic[i],2,36]/Pop5Y2020$RuralPop[which(Pop5Y2020$ADM2==munic[i])],
                         NbCases_AC1_StI_rur[munic[i],4,36]/Pop5Y2020$RuralPop[which(Pop5Y2020$ADM2==munic[i])],
                         NbCases_AC1_StI_urb[munic[i],3,36]/Pop5Y2020$UrbanPop[which(Pop5Y2020$ADM2==munic[i])],
                         NbCases_AC1_StI_urb[munic[i],2,36]/Pop5Y2020$UrbanPop[which(Pop5Y2020$ADM2==munic[i])],
                         NbCases_AC1_StI_urb[munic[i],4,36]/Pop5Y2020$UrbanPop[which(Pop5Y2020$ADM2==munic[i])],
                         (NbCases_AC1_StI_rur[munic[i],3,36] + NbCases_AC1_StI_urb[munic[i],3,36])/Pop5Y2020$TotalPop[which(Pop5Y2020$ADM2==munic[i])],
                         (NbCases_AC1_StI_rur[munic[i],2,36] + NbCases_AC1_StI_urb[munic[i],2,36])/Pop5Y2020$TotalPop[which(Pop5Y2020$ADM2==munic[i])],
                         (NbCases_AC1_StI_rur[munic[i],4,36] + NbCases_AC1_StI_urb[munic[i],4,36])/Pop5Y2020$TotalPop[which(Pop5Y2020$ADM2==munic[i])]
  )
}

Prevalence_5Y_2020<- as.data.frame(Prevalence_5Y_2020)
colnames(Prevalence_5Y_2020)<- c("ADM2", "Med_Prev_rur", "Q1_Prev_rur", "Q3_Prev_rur", "Med_Prev_urb", "Q1_Prev_urb", "Q3_Prev_urb",  "Med_Prev_tot", "Q1_Prev_tot", "Q3_Prev_tot")
Prevalence_5Y_2020$Med_Prev_rur<- as.numeric(Prevalence_5Y_2020$Med_Prev_rur)
Prevalence_5Y_2020$Med_Prev_urb<- as.numeric(Prevalence_5Y_2020$Med_Prev_urb)
Prevalence_5Y_2020$Med_Prev_tot<- as.numeric(Prevalence_5Y_2020$Med_Prev_tot)
Prevalence_5Y_2020$Q1_Prev_rur<- as.numeric(Prevalence_5Y_2020$Q1_Prev_rur)
Prevalence_5Y_2020$Q3_Prev_rur<- as.numeric(Prevalence_5Y_2020$Q3_Prev_rur)
Prevalence_5Y_2020$Q1_Prev_urb<- as.numeric(Prevalence_5Y_2020$Q1_Prev_urb)
Prevalence_5Y_2020$Q3_Prev_urb<- as.numeric(Prevalence_5Y_2020$Q3_Prev_urb)
Prevalence_5Y_2020$Q1_Prev_tot<- as.numeric(Prevalence_5Y_2020$Q1_Prev_tot)
Prevalence_5Y_2020$Q3_Prev_tot<- as.numeric(Prevalence_5Y_2020$Q3_Prev_tot)

Prevalence_5Y_2020$IQRMed_rur<- (Prevalence_5Y_2020$Q3_Prev_rur-Prevalence_5Y_2020$Q1_Prev_rur)/Prevalence_5Y_2020$Med_Prev_rur
Prevalence_5Y_2020$IQRMed_urb<- (Prevalence_5Y_2020$Q3_Prev_urb-Prevalence_5Y_2020$Q1_Prev_urb)/Prevalence_5Y_2020$Med_Prev_urb
Prevalence_5Y_2020$IQRMed_tot<- (Prevalence_5Y_2020$Q3_Prev_tot-Prevalence_5Y_2020$Q1_Prev_tot)/Prevalence_5Y_2020$Med_Prev_tot


##add certification layer
dat_COL<- readRDS("C:/Users/ledie/Box/Analyses/MachineLearning/MunLM/BurdenModel/Predictors/RprolixusElimYear_COL")
Elim<- as.data.frame(dat_COL[,71])
Elim$ADM2<- rownames(dat_COL)
Prevalence_5Y_2020$certifYear<- 0
for (m in 1:nrow(Prevalence_5Y_2020)){
  Prevalence_5Y_2020$certifYear[m]<- Elim$`dat_COL[, 71]`[which(Elim$ADM2==Prevalence_5Y_2020$ADM2[m])]
}

Prevalence_5Y_2020$certif<- ifelse(is.na(Prevalence_5Y_2020$certifYear)==T, "NoElim", "Elim")
table(Prevalence_5Y_2020$certif)
Prevalence_5Y_2020$Prev_under_2<-ifelse(Prevalence_5Y_2020$Med_Prev_rur<=0.02 & Prevalence_5Y_2020$Med_Prev_urb<=0.02, "under", "above")
table(Prevalence_5Y_2020$Prev_under_2)

summary(Prevalence_5Y_2020$Med_Prev_rur)
summary(Prevalence_5Y_2020$Med_Prev_urb)

# Creating shapefiles
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,Prevalence2020[match(COL_t@data[,"ADM2_PCODE"], Prevalence2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/Prevalence2020", layer="PrevalenceTotale2020_100it", driver="ESRI Shapefile")

