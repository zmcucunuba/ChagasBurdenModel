####Graphs prevalence along time

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


##RURAL
Rur_National_ACt_StAc<- readRDS("ML_inputs/res/National_NbCases_ACt_StAc_rural_100_it")
Rur_National_ACt_StAs<- readRDS("ML_inputs/res/National_NbCases_ACt_StAs_rural_100_it")
Rur_National_ACt_StCh<- readRDS("ML_inputs/res/National_NbCases_ACt_StCh_rural_100_it")
Rur_National_ACt_StChS<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_rural_100_it")

Rur_Sum_StAc<- t(apply(Rur_National_ACt_StAc, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Rur_Sum_StAc)<- c("Rur_StAc_lo", "Rur_StAc_med", "Rur_StAc_up")

Rur_Sum_StAs<- t(apply(Rur_National_ACt_StAs, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Rur_Sum_StAs)<- c("Rur_StAs_lo", "Rur_StAs_med", "Rur_StAs_up")

Rur_Sum_StCh<- t(apply(Rur_National_ACt_StCh, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Rur_Sum_StCh)<- c("Rur_StCh_lo", "Rur_StCh_med", "Rur_StCh_up")

Rur_Sum_StChS<- t(apply(Rur_National_ACt_StChS, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Rur_Sum_StChS)<- c("Rur_StChS_lo", "Rur_StChS_med", "Rur_StChS_up")

Rur_Sum<- as.data.frame(cbind(Rur_Sum_StAc, Rur_Sum_StAs, Rur_Sum_StCh, Rur_Sum_StChS))
rownames(Rur_Sum)<- 1985:2020

Rur_Sum_Prev<- Rur_Sum
for (y in as.character(1985:2020)){
  Rur_Sum_Prev[y,]<- Rur_Sum[y,]/PopTotal$RuralPop[which(PopTotal$Year==y)]
}

Rur_Sum_Prev$year<- as.numeric(rownames(Rur_Sum_Prev))

ggplot(data=Rur_Sum_Prev)+
  geom_line(aes( year, Rur_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Rur_StAc_lo,ymax=Rur_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Rur_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Rur_StAs_lo,ymax=Rur_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Rur_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Rur_StCh_lo,ymax=Rur_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Rur_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Rur_StChS_lo,ymax=Rur_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National cases of Chagas Disease (Rural)", y="Prevalence", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+scale_y_log10()
ggsave("ML_inputs/res/Graphs/Graph_Prevalence_Time_National_Rural.png",height = 6, width = 8)

##RURAL
Urb_National_ACt_StAc<- readRDS("ML_inputs/res/National_NbCases_ACt_StAc_UrbanBigCT_100_it")
Urb_National_ACt_StAs<- readRDS("ML_inputs/res/National_NbCases_ACt_StAs_UrbanBigCT_100_it")
Urb_National_ACt_StCh<- readRDS("ML_inputs/res/National_NbCases_ACt_StCh_UrbanBigCT_100_it")
Urb_National_ACt_StChS<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_UrbanBigCT_100_it")

Urb_Sum_StAc<- t(apply(Urb_National_ACt_StAc, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StAc)<- c("Urb_StAc_lo", "Urb_StAc_med", "Urb_StAc_up")

Urb_Sum_StAs<- t(apply(Urb_National_ACt_StAs, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StAs)<- c("Urb_StAs_lo", "Urb_StAs_med", "Urb_StAs_up")

Urb_Sum_StCh<- t(apply(Urb_National_ACt_StCh, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StCh)<- c("Urb_StCh_lo", "Urb_StCh_med", "Urb_StCh_up")

Urb_Sum_StChS<- t(apply(Urb_National_ACt_StChS, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StChS)<- c("Urb_StChS_lo", "Urb_StChS_med", "Urb_StChS_up")

Urb_Sum<- as.data.frame(cbind(Urb_Sum_StAc, Urb_Sum_StAs, Urb_Sum_StCh, Urb_Sum_StChS))
rownames(Urb_Sum)<- 1985:2020

Urb_Sum_Prev<- Urb_Sum
for (y in as.character(1985:2020)){
  Urb_Sum_Prev[y,]<- Urb_Sum[y,]/PopTotal$UrbanPop[which(PopTotal$Year==y)]
}

Urb_Sum_Prev$year<- as.numeric(rownames(Urb_Sum_Prev))

ggplot(data=Urb_Sum_Prev)+
  geom_line(aes( year, Urb_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Urb_StAc_lo,ymax=Urb_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Urb_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Urb_StAs_lo,ymax=Urb_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Urb_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Urb_StCh_lo,ymax=Urb_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Urb_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Urb_StChS_lo,ymax=Urb_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National cases of Chagas Disease (Urban)", y="Prevalence", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+scale_y_log10()
ggsave("ML_inputs/res/Graphs/Graph_Prevalence_Time_National_UrbanBigCT.png",height = 6, width = 8)

###TOTAL

Tot_National_ACt_StAc<- Urb_National_ACt_StAc+Rur_National_ACt_StAc
Tot_National_ACt_StAs<- Urb_National_ACt_StAs+Rur_National_ACt_StAs
Tot_National_ACt_StCh<- Urb_National_ACt_StCh + Rur_National_ACt_StCh
Tot_National_ACt_StChS<-Urb_National_ACt_StChS +Rur_National_ACt_StChS


Tot_Sum_StAc<- t(apply(Tot_National_ACt_StAc, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StAc)<- c("Tot_StAc_lo", "Tot_StAc_med", "Tot_StAc_up")

Tot_Sum_StAs<- t(apply(Tot_National_ACt_StAs, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StAs)<- c("Tot_StAs_lo", "Tot_StAs_med", "Tot_StAs_up")

Tot_Sum_StCh<- t(apply(Tot_National_ACt_StCh, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StCh)<- c("Tot_StCh_lo", "Tot_StCh_med", "Tot_StCh_up")

Tot_Sum_StChS<- t(apply(Tot_National_ACt_StChS, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StChS)<- c("Tot_StChS_lo", "Tot_StChS_med", "Tot_StChS_up")

Tot_Sum<- as.data.frame(cbind(Tot_Sum_StAc, Tot_Sum_StAs, Tot_Sum_StCh, Tot_Sum_StChS))
rownames(Tot_Sum)<- 1985:2020

Tot_Sum_Prev<- Tot_Sum
for (y in as.character(1985:2020)){
  Tot_Sum_Prev[y,]<- Tot_Sum[y,]/PopTotal$TotalPop[which(PopTotal$Year==y)]
}

Tot_Sum_Prev$year<- as.numeric(rownames(Tot_Sum_Prev))

ggplot(data=Tot_Sum_Prev)+
  geom_line(aes( year, Tot_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Tot_StAc_lo,ymax=Tot_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Tot_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Tot_StAs_lo,ymax=Tot_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Tot_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Tot_StCh_lo,ymax=Tot_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Tot_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Tot_StChS_lo,ymax=Tot_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National prevalence of Chagas disease (Total)", y="Prevalence", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+
 coord_cartesian(ylim=c(0,0.01))
ggsave("ML_inputs/res/Graphs/Graph_Prevalence_Time_National_Total_notlog.png", height = 3, width = 4.5)

##figures for the paper:
Urb_National_ACt_StI<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_UrbanBigCT_100_it")
Rur_National_ACt_StI<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_rural_100_it")
Tot_National_ACt_StI<- Urb_National_ACt_StI+Rur_National_ACt_StI

Tot_Sum_StI<- t(apply(Tot_National_ACt_StI, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StI)<- c("Tot_StAc_lo", "Tot_StAc_med", "Tot_StAc_up")

Tot_Sum_StI[6,]/PopTotal$TotalPop[which(PopTotal$Year==1995)]#prevalence total in 1995
Tot_Sum_StI[26,]/PopTotal$TotalPop[which(PopTotal$Year==2010)]#prevalence total in 2010
Tot_Sum_StI[36,]/PopTotal$TotalPop[which(PopTotal$Year==2020)]#prevalence total in 2020


# 
(Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==2020)]-Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==1985)])/Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==1985)]
(Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==2020)]-Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==1985)])/Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==1985)]

(Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==2020)]-Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==1995)])/Tot_Sum_Prev$Tot_StAc_med[which(Tot_Sum_Prev$year==1995)]
(Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==2020)]-Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==1995)])/Tot_Sum_Prev$Tot_StChS_med[which(Tot_Sum_Prev$year==1995)]
