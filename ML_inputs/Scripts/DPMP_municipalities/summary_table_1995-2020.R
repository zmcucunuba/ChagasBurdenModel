####summary table
library(readxl)

summ<- matrix(NA,11,6 )
colnames(summ)<- c( "Q2.5_1995" ,"Med_1995", "Q97.5_1995",  "Q2.5_2020", "Med_2020","Q97.5_2020")
rownames(summ)<- c("Total Number of infections","Total Number of severe cases", "Total prevalence", "Urban prevalence" ,
                   "Rural Prevalence", "Prevalence under 5", "Prevalence above 60", "Total Number of deaths", "", "", "")

#pop size
brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
AC<-c("AC1", "AC2", "AC3", "AC4", "AC5")


PopTotalAC<- matrix(NA, length(AC), 5)

PopTotalAC[1,]<- c(AC[1],sum(Urban[which(Urban$AÑO=="2020"),7:12]), sum(Rural[which(Rural$AÑO=="2020"),7:12]),
                   sum(Urban[which(Urban$AÑO=="1995"),7:12]), sum(Rural[which(Rural$AÑO=="1995"),7:12]))
PopTotalAC[2,]<- c(AC[2],sum(Urban[which(Urban$AÑO=="2020"),13:27]), sum(Rural[which(Rural$AÑO=="2020"),13:27]),
                   sum(Urban[which(Urban$AÑO=="1995"),13:27]), sum(Rural[which(Rural$AÑO=="1995"),13:27]))
PopTotalAC[3,]<- c(AC[3],sum(Urban[which(Urban$AÑO=="2020"),28:47]), sum(Rural[which(Rural$AÑO=="2020"),28:47]),
                   sum(Urban[which(Urban$AÑO=="1995"),28:47]), sum(Rural[which(Rural$AÑO=="1995"),28:47]))
PopTotalAC[4,]<- c(AC[4],sum(Urban[which(Urban$AÑO=="2020"),48:67]), sum(Rural[which(Rural$AÑO=="2020"),48:67]),
                   sum(Urban[which(Urban$AÑO=="1995"),48:67]), sum(Rural[which(Rural$AÑO=="1995"),48:67]))
PopTotalAC[5,]<- c(AC[5],sum(Urban[which(Urban$AÑO=="2020"),68:107]), sum(Rural[which(Rural$AÑO=="2020"),68:107]),
                   sum(Urban[which(Urban$AÑO=="1995"),68:107]), sum(Rural[which(Rural$AÑO=="1995"),68:107]))

PopTotalAC<- as.data.frame(PopTotalAC)
colnames(PopTotalAC)<- c("AC", "UrbanPop2020", "RuralPop2020", "UrbanPop1995", "RuralPop1995")
PopTotalAC$UrbanPop2020 <- as.numeric(PopTotalAC$UrbanPop2020 )
PopTotalAC$RuralPop2020 <- as.numeric(PopTotalAC$RuralPop2020 )
PopTotalAC$UrbanPop1995 <- as.numeric(PopTotalAC$UrbanPop1995 )
PopTotalAC$RuralPop1995 <- as.numeric(PopTotalAC$RuralPop1995 )

PopTotalAC$TotalPop2020<- PopTotalAC$UrbanPop2020 + PopTotalAC$RuralPop2020
PopTotalAC$TotalPop1995<- PopTotalAC$UrbanPop1995 + PopTotalAC$RuralPop1995



#cases
TotalCasesR<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_rural_100_it")
TotalCasesU<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_UrbanBigCT_100_it")
TotalCases<- TotalCasesR +TotalCasesU
summ[1,1:3]<-quantile(TotalCases[,11], c(0.025, 0.5, 0.975))
summ[1,4:6]<-quantile(TotalCases[,36], c(0.025, 0.5, 0.975))

# severe cases
TotalSCasesR<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_rural_100_it")
TotalSCasesU<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_UrbanBigCT_100_it")
TotalSCases<- TotalSCasesR +TotalSCasesU
summ[2,1:3]<-quantile(TotalSCases[,11], c(0.025, 0.5, 0.975))
summ[2,4:6]<-quantile(TotalSCases[,36], c(0.025, 0.5, 0.975))

# mild cases
TotalSCasesR<- readRDS("ML_inputs/res/National_NbCases_ACt_StCh_rural_100_it")
TotalSCasesU<- readRDS("ML_inputs/res/National_NbCases_ACt_StCh_UrbanBigCT_100_it")
TotalSCases<- TotalSCasesR +TotalSCasesU
quantile(TotalSCases[,11], c(0.025, 0.5, 0.975))
quantile(TotalSCases[,36], c(0.025, 0.5, 0.975))



#prevalence totale
summ[3,1:3]<-quantile(TotalCases[,11], c(0.025, 0.5, 0.975))/sum(PopTotalAC$TotalPop1995)
summ[3,4:6]<-quantile(TotalCases[,36], c(0.025, 0.5, 0.975))/sum(PopTotalAC$TotalPop2020)


#prevalence urban
summ[4,1:3]<-quantile(TotalCasesU[,11], c(0.025, 0.5, 0.975))/sum(PopTotalAC$UrbanPop1995)
summ[4,4:6]<-quantile(TotalCasesU[,36], c(0.025, 0.5, 0.975))/sum(PopTotalAC$UrbanPop2020)

#prevalence rural
summ[5,1:3]<-quantile(TotalCasesR[,11], c(0.025, 0.5, 0.975))/sum(PopTotalAC$RuralPop1995)
summ[5,4:6]<-quantile(TotalCasesR[,36], c(0.025, 0.5, 0.975))/sum(PopTotalAC$RuralPop2020)

#prevalence under 5
U5R<- readRDS("ML_inputs/res/National_NbCases_AC1_StI_rural_100_it")
U5U<- readRDS("ML_inputs/res/National_NbCases_AC1_StI_UrbanBigCT_100_it")
U5<- U5R +U5U

summ[6,1:3]<-quantile(U5[,11], c(0.025, 0.5, 0.975))/PopTotalAC$TotalPop1995[1]
summ[6,4:6]<-quantile(U5[,36], c(0.025, 0.5, 0.975))/PopTotalAC$TotalPop2020[1]

#prevalence above 60
A60R<- readRDS("ML_inputs/res/National_NbCases_AC5_StI_rural_100_it")
A60U<- readRDS("ML_inputs/res/National_NbCases_AC5_StI_UrbanBigCT_100_it")
A60<- A60R +A60U

summ[7,1:3]<-quantile(A60[,11], c(0.025, 0.5, 0.975))/PopTotalAC$TotalPop1995[5]
summ[7,4:6]<-quantile(A60[,36], c(0.025, 0.5, 0.975))/PopTotalAC$TotalPop2020[5]

#number of deaths
deathR<- readRDS("ML_inputs/res/National_NbDeath_ACt_StI_rural_100_it")
deathU<- readRDS("ML_inputs/res/National_NbDeath_ACt_StI_UrbanBigCT_100_it")
death<- deathU+deathR

summ[8,1:3]<-quantile(death[,11], c(0.025, 0.5, 0.975))# deaths from 1992
summ[8,4:6]<-quantile(death[,36], c(0.025, 0.5, 0.975))

# departmental deaths
Dep_deathR<- readRDS("ML_inputs/res/Dep_NbDeath_ACt_StI_rural_100_it")
Dep_deathU<- readRDS("ML_inputs/res/Dep_NbDeath_ACt_StI_UrbanBigCT_100_it")
Dep_death<- Dep_deathU+Dep_deathR

Dep_death1995<- Dep_death[,,11]
DeathMed1995<- t(apply(Dep_death1995, 1,quantile, c(0.025, 0.5, 0.975)))


Dep_death2020<- Dep_death[,,36]
DeathMed2020<- t(apply(Dep_death2020, 1,quantile, c(0.025, 0.5, 0.975)))

write.csv(summ, "ML_inputs/res/summary_table1995-2020.csv")

# departmental cases2010
###total
Dep_deathR<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StI_rural_100_it")
Dep_deathU<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StI_UrbanBigCT_100_it")
Dep_death<- Dep_deathU+Dep_deathR


Dep_death2020<- Dep_death[,,35]
Cases<- t(apply(Dep_death2020, 1,quantile, c(0.025, 0.5, 0.975)))
colnames(Cases)<- c("TotalCases_Q25","TotalCases_med", "TotalCases_Q75")

###acute
Dep_deathR<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StAc_rural_100_it")
Dep_deathU<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StAc_UrbanBigCT_100_it")
Dep_death<- Dep_deathU+Dep_deathR

Dep_death2020<- Dep_death[,,35]
tt<- t(apply(Dep_death2020, 1,quantile, c(0.025, 0.5, 0.975)))
colnames(tt)<- c("AcuteCases_Q25","AcuteCases_med", "AcuteCases_Q75")
Cases<- cbind(Cases, tt) 

###chronic mild
Dep_deathR<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StCh_rural_100_it")
Dep_deathU<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StCh_UrbanBigCT_100_it")
Dep_death<- Dep_deathU+Dep_deathR

Dep_death2020<- Dep_death[,,35]
tt<- t(apply(Dep_death2020, 1,quantile, c(0.025, 0.5, 0.975)))
colnames(tt)<- c("MildCases_Q25","MildCases_med", "MildCases_Q75")
Cases<- cbind(Cases, tt) 

###Severe
Dep_deathR<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StChS_rural_100_it")
Dep_deathU<- readRDS("ML_inputs/res/Dep_NbCases_ACt_StChS_UrbanBigCT_100_it")
Dep_death<- Dep_deathU+Dep_deathR

Dep_death2020<- Dep_death[,,35]
tt<- t(apply(Dep_death2020, 1,quantile, c(0.025, 0.5, 0.975)))
colnames(tt)<- c("SevereCases_Q25","SevereCases_med", "SevereCases_Q75")
Cases<- cbind(Cases, tt) 

write.csv(Cases, "ML_inputs/res/Dep_summary_table1995-2020.csv")
