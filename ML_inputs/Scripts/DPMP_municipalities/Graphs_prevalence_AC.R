####Graphs prevalence by age classes

library(readxl)
library(ggplot2)

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

AC<-c("AC1", "AC2", "AC3", "AC4", "AC5")


PopTotalAC<- matrix(NA, length(AC), 3)

PopTotalAC[1,]<- c(AC[1],sum(Urban[which(Urban$AÑO=="2020"),7:12]), sum(Rural[which(Rural$AÑO=="2020"),7:12]))
PopTotalAC[2,]<- c(AC[2],sum(Urban[which(Urban$AÑO=="2020"),13:27]), sum(Rural[which(Rural$AÑO=="2020"),13:27]))
PopTotalAC[3,]<- c(AC[3],sum(Urban[which(Urban$AÑO=="2020"),28:47]), sum(Rural[which(Rural$AÑO=="2020"),28:47]))
PopTotalAC[4,]<- c(AC[4],sum(Urban[which(Urban$AÑO=="2020"),48:67]), sum(Rural[which(Rural$AÑO=="2020"),48:67]))
PopTotalAC[5,]<- c(AC[5],sum(Urban[which(Urban$AÑO=="2020"),68:107]), sum(Rural[which(Rural$AÑO=="2020"),68:107]))

PopTotalAC<- as.data.frame(PopTotalAC)
colnames(PopTotalAC)<- c("AC", "UrbanPop", "RuralPop")
PopTotalAC$UrbanPop <- as.numeric(PopTotalAC$UrbanPop )
PopTotalAC$RuralPop <- as.numeric(PopTotalAC$RuralPop )

PopTotalAC$TotalPop<- PopTotalAC$UrbanPop + PopTotalAC$RuralPop

###RURAL
St <-c("StAc", "StAs", "StCh", "StChS")
Rur_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    dat<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_rural_100_it"))
    Rur_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36])/PopTotalAC$RuralPop[which(PopTotalAC$AC==AC[a])],
                                    quantile(dat[,36], 0.025)/PopTotalAC$RuralPop[which(PopTotalAC$AC==AC[a])], 
                                    quantile(dat[,36], 0.975)/PopTotalAC$RuralPop[which(PopTotalAC$AC==AC[a])])
    
  }
}

Rur_Sum_2020<- as.data.frame(Rur_Sum_2020)
colnames(Rur_Sum_2020)<- c("AC", "St", "Prev", "Q2.5", "Q97.5")
Rur_Sum_2020$Prev<- as.numeric(Rur_Sum_2020$Prev)
Rur_Sum_2020$Q2.5<- as.numeric(Rur_Sum_2020$Q2.5)
Rur_Sum_2020$Q97.5<- as.numeric(Rur_Sum_2020$Q97.5)

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Rur_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=Prev, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National prevalence of Chagas Disease (Rural)", y="Prevalence", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066")) +
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))
ggsave("ML_inputs/res/Graphs/Graph_Prevalence_AC_National_Rural.png",height = 6, width = 8)

###URBAN
Urb_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    dat<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_UrbanBigCT_100_it"))
    Urb_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36])/PopTotalAC$UrbanPop[which(PopTotalAC$AC==AC[a])],
                                    quantile(dat[,36], 0.025)/PopTotalAC$UrbanPop[which(PopTotalAC$AC==AC[a])], 
                                    quantile(dat[,36], 0.975)/PopTotalAC$UrbanPop[which(PopTotalAC$AC==AC[a])])
    
  }
}

Urb_Sum_2020<- as.data.frame(Urb_Sum_2020)
colnames(Urb_Sum_2020)<- c("AC", "St", "Prev", "Q2.5", "Q97.5")
Urb_Sum_2020$Prev<- as.numeric(Urb_Sum_2020$Prev)
Urb_Sum_2020$Q2.5<- as.numeric(Urb_Sum_2020$Q2.5)
Urb_Sum_2020$Q97.5<- as.numeric(Urb_Sum_2020$Q97.5)

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Urb_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=Prev, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National prevalence of Chagas Disease (Urban)", y="Prevalence", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066"))+
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))

ggsave("ML_inputs/res/Graphs/Graph_Prevalence_AC_National_UrbanBigCT.png",height = 6, width = 8)

###TOTAL
Tot_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    datU<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_UrbanBigCT_100_it"))
    datR<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_rural_100_it"))
    dat<-datU+datR
    Tot_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36])/PopTotalAC$TotalPop[which(PopTotalAC$AC==AC[a])],
                                    quantile(dat[,36], 0.025)/PopTotalAC$TotalPop[which(PopTotalAC$AC==AC[a])], 
                                    quantile(dat[,36], 0.975)/PopTotalAC$TotalPop[which(PopTotalAC$AC==AC[a])])
    
  }
}

Tot_Sum_2020<- as.data.frame(Tot_Sum_2020)
colnames(Tot_Sum_2020)<- c("AC", "St", "Prev", "Q2.5", "Q97.5")
Tot_Sum_2020$Prev<- as.numeric(Tot_Sum_2020$Prev)
Tot_Sum_2020$Q2.5<- as.numeric(Tot_Sum_2020$Q2.5)
Tot_Sum_2020$Q97.5<- as.numeric(Tot_Sum_2020$Q97.5)

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Tot_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=Prev, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National prevalence of Chagas disease (Total)", y="Prevalence", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066"))+
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))

ggsave("ML_inputs/res/Graphs/Graph_Prevalence_AC_National_Total.png", height = 3, width = 4.5)

