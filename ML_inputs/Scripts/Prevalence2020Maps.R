##### Prevalence maps

## draft make maps
library(rgdal)
library(readxl)
library(ggplot2)

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

ColombiaADM2<-readOGR("ML_inputs/Data/HUMDATA_col_adm_dane_2020_shp/col_admbnda_adm2_mgn_20200416.shp")

NbCases_ACt_StI_rur<- readRDS("ML_inputs/res/NbCases_ACt_StI_rural_100_it")
NbCases_ACt_StI_urb<- readRDS("ML_inputs/res/NbCases_ACt_StI_urbanBigCT_100_it")

years<- 1985:2020
munic<- ColombiaADM2$ADM2_PCODE

#Population size by municipality in 2020
PopTotal2020<- matrix(NA, length(munic), 3)
for (i in 1:length(munic)){
  PopTotal2020[i,]<- c(munic[i],sum(Urban[which(Urban$AÑO==2020 & paste0("CO",Urban$DPMP) == munic[i]),7:107]),
                   sum(Rural[which(Rural$AÑO==2020 & paste0("CO",Rural$DPMP) == munic[i]),7:107]))
}
PopTotal2020<- as.data.frame(PopTotal2020)
colnames(PopTotal2020)<- c("ADM2", "UrbanPop", "RuralPop")
PopTotal2020$UrbanPop<- as.numeric(PopTotal2020$UrbanPop)
PopTotal2020$RuralPop<- as.numeric(PopTotal2020$RuralPop)
PopTotal2020$TotalPop<- PopTotal2020$UrbanPop + PopTotal2020$RuralPop

Prevalence2020<- matrix(NA, length(munic), 10)

for (i in 1:length(munic)){
  Prevalence2020[i,]<- c(munic[i],
                         NbCases_ACt_StI_rur[munic[i],3,36]/PopTotal2020$RuralPop[which(PopTotal2020$ADM2==munic[i])],
                         NbCases_ACt_StI_rur[munic[i],2,36]/PopTotal2020$RuralPop[which(PopTotal2020$ADM2==munic[i])],
                         NbCases_ACt_StI_rur[munic[i],4,36]/PopTotal2020$RuralPop[which(PopTotal2020$ADM2==munic[i])],
                         NbCases_ACt_StI_urb[munic[i],3,36]/PopTotal2020$UrbanPop[which(PopTotal2020$ADM2==munic[i])],
                         NbCases_ACt_StI_urb[munic[i],2,36]/PopTotal2020$UrbanPop[which(PopTotal2020$ADM2==munic[i])],
                         NbCases_ACt_StI_urb[munic[i],4,36]/PopTotal2020$UrbanPop[which(PopTotal2020$ADM2==munic[i])],
                         (NbCases_ACt_StI_rur[munic[i],3,36] + NbCases_ACt_StI_urb[munic[i],3,36])/PopTotal2020$TotalPop[which(PopTotal2020$ADM2==munic[i])],
                         (NbCases_ACt_StI_rur[munic[i],2,36] + NbCases_ACt_StI_urb[munic[i],2,36])/PopTotal2020$TotalPop[which(PopTotal2020$ADM2==munic[i])],
                         (NbCases_ACt_StI_rur[munic[i],4,36] + NbCases_ACt_StI_urb[munic[i],4,36])/PopTotal2020$TotalPop[which(PopTotal2020$ADM2==munic[i])]
                      )
}

Prevalence2020<- as.data.frame(Prevalence2020)
colnames(Prevalence2020)<- c("ADM2", "Med_Prev_rur", "Q1_Prev_rur", "Q3_Prev_rur", "Med_Prev_urb", "Q1_Prev_urb", "Q3_Prev_urb",  "Med_Prev_tot", "Q1_Prev_tot", "Q3_Prev_tot")
Prevalence2020$Med_Prev_rur<- as.numeric(Prevalence2020$Med_Prev_rur)
Prevalence2020$Med_Prev_urb<- as.numeric(Prevalence2020$Med_Prev_urb)
Prevalence2020$Med_Prev_tot<- as.numeric(Prevalence2020$Med_Prev_tot)
Prevalence2020$Q1_Prev_rur<- as.numeric(Prevalence2020$Q1_Prev_rur)
Prevalence2020$Q3_Prev_rur<- as.numeric(Prevalence2020$Q3_Prev_rur)
Prevalence2020$Q1_Prev_urb<- as.numeric(Prevalence2020$Q1_Prev_urb)
Prevalence2020$Q3_Prev_urb<- as.numeric(Prevalence2020$Q3_Prev_urb)
Prevalence2020$Q1_Prev_tot<- as.numeric(Prevalence2020$Q1_Prev_tot)
Prevalence2020$Q3_Prev_tot<- as.numeric(Prevalence2020$Q3_Prev_tot)

Prevalence2020$IQRMed_rur<- (Prevalence2020$Q3_Prev_rur-Prevalence2020$Q1_Prev_rur)/Prevalence2020$Med_Prev_rur
Prevalence2020$IQRMed_urb<- (Prevalence2020$Q3_Prev_urb-Prevalence2020$Q1_Prev_urb)/Prevalence2020$Med_Prev_urb
Prevalence2020$IQRMed_tot<- (Prevalence2020$Q3_Prev_tot-Prevalence2020$Q1_Prev_tot)/Prevalence2020$Med_Prev_tot



# Creating shapefiles
COL_t<- ColombiaADM2
COL_t@data<- data.frame(COL_t@data,Prevalence2020[match(COL_t@data[,"ADM2_PCODE"], Prevalence2020[,"ADM2"]),])
writeOGR(COL_t, "ML_inputs/res/shapefiles/100it/Prevalence2020", layer="PrevalenceTotale2020_100it", driver="ESRI Shapefile")

summary(Prevalence2020$Med_Prev_rur)
summary(Prevalence2020$Med_Prev_urb)
summary(Prevalence2020$IQRMed_tot)
summary(Prevalence2020$IQRMed_urb)
summary(Prevalence2020$IQRMed_rur)
