### Calculate prevalence

library(readxl)
library(ggplot2)

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

years<- 1985:2020

PopTotal<- matrix(NA, length(years), 3)
for (i in 1:length(years)){
  PopTotal[i,]<- c(years[i],sum(Urban[which(Urban$AÑO==years[i]),7:107]),
                   sum(Rural[which(Rural$AÑO==years[i]),7:107]))
}
PopTotal<- as.data.frame(PopTotal)
colnames(PopTotal)<- c("Year", "UrbanPop", "RuralPop")
PopTotal$TotalPop<- PopTotal$UrbanPop+PopTotal$RuralPop

###Prevalence infection total population niveau national
NbCasesTotalU<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_urban_100_it")
NbCasesTotalR<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_rural_100_it")
NbCasesTotal<- NbCasesTotalR +NbCasesTotalU

summary_Cases_total<- apply(NbCasesTotal, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_total<- as.data.frame(t(summary_Cases_total))
summary_Cases_total$year<- 1985:2020

ggplot(data=summary_Cases_total) + geom_line(aes(x=year, y=`50%`), colour="#6600CC") + geom_ribbon(aes(x=year,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3,fill="#6600CC")+theme_classic()

summary_Cases_totalU<- apply(NbCasesTotalU, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalU<- as.data.frame(t(summary_Cases_totalU))

summary_Cases_totalR<- apply(NbCasesTotalR, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalR<- as.data.frame(t(summary_Cases_totalR))


National_Summary_StI_ACt<- cbind(years,summary_Cases_total[,1:3], summary_Cases_totalU, summary_Cases_totalR)
colnames(National_Summary_StI_ACt)<- c("Year", "NbCasesTotal_Q2.5", "NbCasesTotal_Med", "NbCasesTotal_Q97.5", 
                                       "NbCasesUrban_Q2.5", "NbCasesUrban_Med", "NbCasesUrban_Q97.5", 
                                       "NbCasesRural_Q2.5", "NbCasesRural_Med", "NbCasesRural_Q97.5")

National_Summary_StI_ACt$PrevTotal_Q2.5<- National_Summary_StI_ACt$NbCasesTotal_Q2.5/PopTotal$TotalPop
National_Summary_StI_ACt$PrevTotal_Med<- National_Summary_StI_ACt$NbCasesTotal_Med/PopTotal$TotalPop
National_Summary_StI_ACt$PrevTotal_Q97.5<- National_Summary_StI_ACt$NbCasesTotal_Q97.5/PopTotal$TotalPop

National_Summary_StI_ACt$PrevUrban_Q2.5<- National_Summary_StI_ACt$NbCasesUrban_Q2.5/PopTotal$UrbanPop
National_Summary_StI_ACt$PrevUrban_Med<- National_Summary_StI_ACt$NbCasesUrban_Med/PopTotal$UrbanPop
National_Summary_StI_ACt$PrevUrban_Q97.5<- National_Summary_StI_ACt$NbCasesUrban_Q97.5/PopTotal$UrbanPop

National_Summary_StI_ACt$PrevRural_Q2.5<- National_Summary_StI_ACt$NbCasesRural_Q2.5/PopTotal$RuralPop
National_Summary_StI_ACt$PrevRural_Med<- National_Summary_StI_ACt$NbCasesRural_Med/PopTotal$RuralPop
National_Summary_StI_ACt$PrevRural_Q97.5<- National_Summary_StI_ACt$NbCasesRural_Q97.5/PopTotal$RuralPop


write.csv(National_Summary_StI_ACt,file="ML_inputs/res/National_Summary_StI_ACt.csv")


####Prevalence in Children
Pop5Y<- matrix(NA, length(years), 3)
for (i in 1:length(years)){
  Pop5Y[i,]<- c(years[i],sum(Urban[which(Urban$AÑO==years[i]),7:12]),
                   sum(Rural[which(Rural$AÑO==years[i]),7:12]))
}
Pop5Y<- as.data.frame(Pop5Y)
colnames(Pop5Y)<- c("Year", "UrbanPop", "RuralPop")
Pop5Y$TotalPop<- Pop5Y$UrbanPop+Pop5Y$RuralPop

NbCasesTotalU<- readRDS("ML_inputs/res/National_NbCases_AC1_StI_urban_100_it")
NbCasesTotalR<- readRDS("ML_inputs/res/National_NbCases_AC1_StI_rural_100_it")
NbCasesTotal<- NbCasesTotalR +NbCasesTotalU

summary_Cases_total<- apply(NbCasesTotal, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_total<- as.data.frame(t(summary_Cases_total))
summary_Cases_total$year<- 1985:2020

ggplot(data=summary_Cases_total) + geom_line(aes(x=year, y=`50%`), colour="#6600CC") + geom_ribbon(aes(x=year,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3,fill="#6600CC")+theme_classic()

summary_Cases_totalU<- apply(NbCasesTotalU, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalU<- as.data.frame(t(summary_Cases_totalU))

summary_Cases_totalR<- apply(NbCasesTotalR, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalR<- as.data.frame(t(summary_Cases_totalR))


National_Summary_StI_AC1<- cbind(years,summary_Cases_total[,1:3], summary_Cases_totalU, summary_Cases_totalR)
colnames(National_Summary_StI_AC1)<- c("Year", "NbCasesTotal_Q2.5", "NbCasesTotal_Med", "NbCasesTotal_Q97.5", 
                                       "NbCasesUrban_Q2.5", "NbCasesUrban_Med", "NbCasesUrban_Q97.5", 
                                       "NbCasesRural_Q2.5", "NbCasesRural_Med", "NbCasesRural_Q97.5")

National_Summary_StI_AC1$PrevTotal_Q2.5<- National_Summary_StI_AC1$NbCasesTotal_Q2.5/Pop5Y$TotalPop
National_Summary_StI_AC1$PrevTotal_Med<- National_Summary_StI_AC1$NbCasesTotal_Med/Pop5Y$TotalPop
National_Summary_StI_AC1$PrevTotal_Q97.5<- National_Summary_StI_AC1$NbCasesTotal_Q97.5/Pop5Y$TotalPop

National_Summary_StI_AC1$PrevUrban_Q2.5<- National_Summary_StI_AC1$NbCasesUrban_Q2.5/Pop5Y$UrbanPop
National_Summary_StI_AC1$PrevUrban_Med<- National_Summary_StI_AC1$NbCasesUrban_Med/Pop5Y$UrbanPop
National_Summary_StI_AC1$PrevUrban_Q97.5<- National_Summary_StI_AC1$NbCasesUrban_Q97.5/Pop5Y$UrbanPop

National_Summary_StI_AC1$PrevRural_Q2.5<- National_Summary_StI_AC1$NbCasesRural_Q2.5/Pop5Y$RuralPop
National_Summary_StI_AC1$PrevRural_Med<- National_Summary_StI_AC1$NbCasesRural_Med/Pop5Y$RuralPop
National_Summary_StI_AC1$PrevRural_Q97.5<- National_Summary_StI_AC1$NbCasesRural_Q97.5/Pop5Y$RuralPop


write.csv(National_Summary_StI_AC1,file="ML_inputs/res/National_Summary_StI_AC1.csv")

####Prevalence in elderly
Pop60Y<- matrix(NA, length(years), 3)
for (i in 1:length(years)){
  Pop60Y[i,]<- c(years[i],sum(Urban[which(Urban$AÑO==years[i]),67:107]),
                sum(Rural[which(Rural$AÑO==years[i]),67:107]))
}
Pop60Y<- as.data.frame(Pop60Y)
colnames(Pop60Y)<- c("Year", "UrbanPop", "RuralPop")
Pop60Y$TotalPop<- Pop60Y$UrbanPop+Pop5Y$RuralPop


NbCasesTotalU<- readRDS("ML_inputs/res/National_NbCases_AC5_StI_urban_100_it")
NbCasesTotalR<- readRDS("ML_inputs/res/National_NbCases_AC5_StI_rural_100_it")
NbCasesTotal<- NbCasesTotalR +NbCasesTotalU

summary_Cases_total<- apply(NbCasesTotal, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_total<- as.data.frame(t(summary_Cases_total))
summary_Cases_total$year<- 1985:2020

ggplot(data=summary_Cases_total) + geom_line(aes(x=year, y=`50%`), colour="#6600CC") + geom_ribbon(aes(x=year,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3,fill="#6600CC")+theme_classic()

summary_Cases_totalU<- apply(NbCasesTotalU, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalU<- as.data.frame(t(summary_Cases_totalU))

summary_Cases_totalR<- apply(NbCasesTotalR, 2, quantile, c(0.025, 0.5,0.975))
summary_Cases_totalR<- as.data.frame(t(summary_Cases_totalR))


National_Summary_StI_AC5<- cbind(years,summary_Cases_total[,1:3], summary_Cases_totalU, summary_Cases_totalR)
colnames(National_Summary_StI_AC5)<- c("Year", "NbCasesTotal_Q2.5", "NbCasesTotal_Med", "NbCasesTotal_Q97.5", 
                                       "NbCasesUrban_Q2.5", "NbCasesUrban_Med", "NbCasesUrban_Q97.5", 
                                       "NbCasesRural_Q2.5", "NbCasesRural_Med", "NbCasesRural_Q97.5")

National_Summary_StI_AC5$PrevTotal_Q2.5<- National_Summary_StI_AC5$NbCasesTotal_Q2.5/Pop60Y$TotalPop
National_Summary_StI_AC5$PrevTotal_Med<- National_Summary_StI_AC5$NbCasesTotal_Med/Pop60Y$TotalPop
National_Summary_StI_AC5$PrevTotal_Q97.5<- National_Summary_StI_AC5$NbCasesTotal_Q97.5/Pop60Y$TotalPop

National_Summary_StI_AC5$PrevUrban_Q2.5<- National_Summary_StI_AC5$NbCasesUrban_Q2.5/Pop60Y$UrbanPop
National_Summary_StI_AC5$PrevUrban_Med<- National_Summary_StI_AC5$NbCasesUrban_Med/Pop60Y$UrbanPop
National_Summary_StI_AC5$PrevUrban_Q97.5<- National_Summary_StI_AC5$NbCasesUrban_Q97.5/Pop60Y$UrbanPop

National_Summary_StI_AC5$PrevRural_Q2.5<- National_Summary_StI_AC5$NbCasesRural_Q2.5/Pop60Y$RuralPop
National_Summary_StI_AC5$PrevRural_Med<- National_Summary_StI_AC5$NbCasesRural_Med/Pop60Y$RuralPop
National_Summary_StI_AC5$PrevRural_Q97.5<- National_Summary_StI_AC5$NbCasesRural_Q97.5/Pop60Y$RuralPop


write.csv(National_Summary_StI_AC5,file="ML_inputs/res/National_Summary_StI_AC5.csv")

ggplot(data=National_Summary_StI_ACt) + geom_line(aes(x=Year, y=PrevTotal_Med), colour="#6600CC") + geom_ribbon(aes(x=Year,ymin=PrevTotal_Q2.5,ymax=PrevTotal_Q97.5),alpha=0.3,fill="#6600CC")+
  geom_line(data= National_Summary_StI_AC5,aes(x=Year, y=PrevTotal_Med), colour="#CC3300")+ geom_ribbon(data= National_Summary_StI_AC5,aes( x=Year,ymin=PrevTotal_Q2.5,ymax=PrevTotal_Q97.5),alpha=0.3,fill="#CC3300")+
  geom_line(data= National_Summary_StI_AC1,aes(x=Year, y=PrevTotal_Med), colour="#33CC00")+ geom_ribbon(data= National_Summary_StI_AC1,aes( x=Year,ymin=PrevTotal_Q2.5,ymax=PrevTotal_Q97.5),alpha=0.3,fill="#33CC00")+
  theme_classic()


ggplot(data=National_Summary_StI_ACt) + geom_line(aes(x=Year, y=NbCasesTotal_Med), colour="#6600CC") + geom_ribbon(aes(x=Year,ymin=NbCasesTotal_Q2.5,ymax=NbCasesTotal_Q97.5),alpha=0.3,fill="#6600CC")+
  geom_line(data= National_Summary_StI_AC5,aes(x=Year, y=NbCasesTotal_Med), colour="#CC3300")+ geom_ribbon(data= National_Summary_StI_AC5,aes( x=Year,ymin=NbCasesTotal_Q2.5,ymax=NbCasesTotal_Q97.5),alpha=0.3,fill="#CC3300")+
  geom_line(data= National_Summary_StI_AC1,aes(x=Year, y=NbCasesTotal_Med), colour="#33CC00")+ geom_ribbon(data= National_Summary_StI_AC1,aes( x=Year,ymin=NbCasesTotal_Q2.5,ymax=NbCasesTotal_Q97.5),alpha=0.3,fill="#33CC00")+
  theme_classic()

ggplot(data=National_Summary_StI_ACt) + geom_line(aes(x=Year, y=NbCasesUrban_Med), colour="#6600FF") + geom_ribbon(aes(x=Year,ymin=NbCasesUrban_Q2.5,ymax=NbCasesUrban_Q97.5),alpha=0.3,fill="#6600FF")+
  geom_line(data= National_Summary_StI_ACt,aes(x=Year, y=NbCasesRural_Med), colour="#006600")+ geom_ribbon(data= National_Summary_StI_ACt,aes( x=Year,ymin=NbCasesRural_Q2.5,ymax=NbCasesRural_Q97.5),alpha=0.3,fill="#006600")+
  theme_classic()

ggplot(data=National_Summary_StI_ACt) + geom_line(aes(x=Year, y=PrevUrban_Med), colour="#6600FF") + geom_ribbon(aes(x=Year,ymin=PrevUrban_Q2.5,ymax=PrevUrban_Q97.5),alpha=0.3,fill="#6600FF")+
  geom_line(data= National_Summary_StI_ACt,aes(x=Year, y=PrevRural_Med), colour="#006600")+ geom_ribbon(data= National_Summary_StI_ACt,aes( x=Year,ymin=PrevRural_Q2.5,ymax=PrevRural_Q97.5),alpha=0.3,fill="#006600")+
  theme_classic()
