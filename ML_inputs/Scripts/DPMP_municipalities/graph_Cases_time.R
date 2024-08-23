####Graph Cases along time by disease stage
####Julia Ledien 08/02/2022

library(ggplot2)

###RURAL
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
Rur_Sum$year<- 1985:2020

ggplot(data=Rur_Sum)+
  geom_line(aes( year, Rur_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Rur_StAc_lo,ymax=Rur_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Rur_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Rur_StAs_lo,ymax=Rur_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Rur_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Rur_StCh_lo,ymax=Rur_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Rur_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Rur_StChS_lo,ymax=Rur_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National cases of Chagas Disease (Rural)", y="Number of cases", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+scale_y_log10()
ggsave("ML_inputs/res/Graphs/Graph_NbCases_Time_National_Rural.png")



#### URBAN
Urb_National_ACt_StAc<- readRDS("ML_inputs/res/National_NbCases_ACt_StAc_urbanBigCT_100_it")
Urb_National_ACt_StAs<- readRDS("ML_inputs/res/National_NbCases_ACt_StAs_urbanBigCT_100_it")
Urb_National_ACt_StCh<- readRDS("ML_inputs/res/National_NbCases_ACt_StCh_urbanBigCT_100_it")
Urb_National_ACt_StChS<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_urbanBigCT_100_it")

Urb_Sum_StAc<- t(apply(Urb_National_ACt_StAc, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StAc)<- c("Urb_StAc_lo", "Urb_StAc_med", "Urb_StAc_up")

Urb_Sum_StAs<- t(apply(Urb_National_ACt_StAs, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StAs)<- c("Urb_StAs_lo", "Urb_StAs_med", "Urb_StAs_up")

Urb_Sum_StCh<- t(apply(Urb_National_ACt_StCh, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StCh)<- c("Urb_StCh_lo", "Urb_StCh_med", "Urb_StCh_up")

Urb_Sum_StChS<- t(apply(Urb_National_ACt_StChS, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Urb_Sum_StChS)<- c("Urb_StChS_lo", "Urb_StChS_med", "Urb_StChS_up")

Urb_Sum<- as.data.frame(cbind(Urb_Sum_StAc, Urb_Sum_StAs, Urb_Sum_StCh, Urb_Sum_StChS))
Urb_Sum$year<- 1985:2020

ggplot(data=Urb_Sum)+
  geom_line(aes( year, Urb_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Urb_StAc_lo,ymax=Urb_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Urb_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Urb_StAs_lo,ymax=Urb_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Urb_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Urb_StCh_lo,ymax=Urb_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Urb_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Urb_StChS_lo,ymax=Urb_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National cases of Chagas Disease (Urban)", y="Number of cases", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+scale_y_log10()
ggsave("ML_inputs/res/Graphs/Graph_NbCases_Time_National_Urban.png")


###TOTAL
Tot_National_ACt_StAc<- Rur_National_ACt_StAc + Urb_National_ACt_StAc
Tot_National_ACt_StAs<- Rur_National_ACt_StAs + Urb_National_ACt_StAs
Tot_National_ACt_StCh<- Rur_National_ACt_StCh + Urb_National_ACt_StCh
Tot_National_ACt_StChS<- Rur_National_ACt_StChS + Urb_National_ACt_StChS

Tot_Sum_StAc<- t(apply(Tot_National_ACt_StAc, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StAc)<- c("Tot_StAc_lo", "Tot_StAc_med", "Tot_StAc_up")

Tot_Sum_StAs<- t(apply(Tot_National_ACt_StAs, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StAs)<- c("Tot_StAs_lo", "Tot_StAs_med", "Tot_StAs_up")

Tot_Sum_StCh<- t(apply(Tot_National_ACt_StCh, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StCh)<- c("Tot_StCh_lo", "Tot_StCh_med", "Tot_StCh_up")

Tot_Sum_StChS<- t(apply(Tot_National_ACt_StChS, 2,quantile, c(0.025, 0.5, 0.975)))
colnames(Tot_Sum_StChS)<- c("Tot_StChS_lo", "Tot_StChS_med", "Tot_StChS_up")

Tot_Sum<- as.data.frame(cbind(Tot_Sum_StAc, Tot_Sum_StAs, Tot_Sum_StCh, Tot_Sum_StChS))
Tot_Sum$year<- 1985:2020

ggplot(data=Tot_Sum)+
  geom_line(aes( year, Tot_StAc_med, color="Acute")) +
  geom_ribbon(aes(x=year,ymin=Tot_StAc_lo,ymax=Tot_StAc_up),alpha=0.3,fill="#00CC66")+
  
  geom_line(aes( year, Tot_StAs_med, color="Asymptomatic")) +
  geom_ribbon(aes(x=year,ymin=Tot_StAs_lo,ymax=Tot_StAs_up),alpha=0.3,fill="#3399FF")+
  
  geom_line(aes( year, Tot_StCh_med, color="Chronic Mild")) +
  geom_ribbon(aes(x=year,ymin=Tot_StCh_lo,ymax=Tot_StCh_up),alpha=0.3,fill="#FF9933")+
  
  geom_line(aes( year, Tot_StChS_med, color="Chronic Severe")) +
  geom_ribbon(aes(x=year,ymin=Tot_StChS_lo,ymax=Tot_StChS_up),alpha=0.3,fill="#990066")+
  
  theme_classic()+labs(title = "National cases of Chagas Disease (Total)", y="Number of cases", x="Years")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", breaks=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("Acute"="#00CC66", "Asymptomatic"="#3399FF", "Chronic Mild"="#FF9933","Chronic Severe"="#990066"))+scale_y_log10()
ggsave("ML_inputs/res/Graphs/Graph_NbCases_Time_National_Total.png")
