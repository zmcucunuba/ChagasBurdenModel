#### Graphs Cases by age classes
###Julia Ledien 08/02/2022

###RURAL

###RURAL

AC<-c("AC1", "AC2", "AC3", "AC4", "AC5")
St <-c("StAc", "StAs", "StCh", "StChS")
Rur_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    dat<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_rural_100_it"))
    Rur_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36]), quantile(dat[,36], 0.025), quantile(dat[,36], 0.975))
    
  }
}

Rur_Sum_2020<- as.data.frame(Rur_Sum_2020)
colnames(Rur_Sum_2020)<- c("AC", "St", "NbCases", "Q2.5", "Q97.5")
Rur_Sum_2020$NbCases<- as.numeric(Rur_Sum_2020$NbCases)
Rur_Sum_2020$Q2.5<- as.numeric(Rur_Sum_2020$Q2.5)
Rur_Sum_2020$Q97.5<- as.numeric(Rur_Sum_2020$Q97.5)
#Rur_Sum_2020$AC2<- ifelse(Rur_Sum_2020$AC== "AC1", "[0;5]", ifelse(Rur_Sum_2020$AC=="AC2", "[6;20]", ifelse(Rur_Sum_2020$AC=="AC3", "[21;40]", ifelse(Rur_Sum_2020$AC=="AC4", "[41;60]", ">60"))))

ggplot(data=Rur_Sum_2020)+
  geom_col(position="stack", aes(x=AC, y=NbCases, fill=St))

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Rur_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=NbCases, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National cases of Chagas Disease (Rural)", y="Number of cases", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066")) +
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))
ggsave("ML_inputs/res/Graphs/Graph_NbCases_AC_National_Rural.png")


###URBAN
Urb_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    dat<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_UrbanBigCT_100_it"))
    Urb_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36]), quantile(dat[,36], 0.025), quantile(dat[,36], 0.975))
    
  }
}

Urb_Sum_2020<- as.data.frame(Urb_Sum_2020)
colnames(Urb_Sum_2020)<- c("AC", "St", "NbCases", "Q2.5", "Q97.5")
Urb_Sum_2020$NbCases<- as.numeric(Urb_Sum_2020$NbCases)
Urb_Sum_2020$Q2.5<- as.numeric(Urb_Sum_2020$Q2.5)
Urb_Sum_2020$Q97.5<- as.numeric(Urb_Sum_2020$Q97.5)
Urb_Sum_2020$AC2<- ifelse(Urb_Sum_2020$AC== "AC1", "[0;5]", ifelse(Urb_Sum_2020$AC=="AC2", "[6;20]", ifelse(Urb_Sum_2020$AC=="AC3", "[21;40]", ifelse(Urb_Sum_2020$AC=="AC4", "[41;60]", ">60"))))

ggplot(data=Urb_Sum_2020)+
  geom_col(position="stack", aes(x=AC, y=NbCases, fill=St))

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Urb_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=NbCases, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National cases of Chagas Disease (Urban)", y="Number of cases", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066"))+
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))

ggsave("ML_inputs/res/Graphs/Graph_NbCases_AC_National_UrbanBigCT.png")


###TOTAL
Tot_Sum_2020<- matrix(NA,length(AC)*length(St),5 )

for ( a in 1: length(AC) ){
  for (s in 1: length(St) ){
    datU<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_UrbanBigCT_100_it"))
    datR<- readRDS(paste0("ML_inputs/res/National_NbCases_", AC[a], "_",St[s], "_rural_100_it"))
    dat<-datU+datR
    Tot_Sum_2020[(4*(a-1))+s,] <- c(AC[a], St[s], median(dat[,36]), quantile(dat[,36], 0.025), quantile(dat[,36], 0.975))
    
  }
}

Tot_Sum_2020<- as.data.frame(Tot_Sum_2020)
colnames(Tot_Sum_2020)<- c("AC", "St", "NbCases", "Q2.5", "Q97.5")
Tot_Sum_2020$NbCases<- as.numeric(Tot_Sum_2020$NbCases)
Tot_Sum_2020$Q2.5<- as.numeric(Tot_Sum_2020$Q2.5)
Tot_Sum_2020$Q97.5<- as.numeric(Tot_Sum_2020$Q97.5)
Tot_Sum_2020$AC2<- ifelse(Tot_Sum_2020$AC== "AC1", "[0;5]", ifelse(Tot_Sum_2020$AC=="AC2", "[6;20]", ifelse(Tot_Sum_2020$AC=="AC3", "[21;40]", ifelse(Tot_Sum_2020$AC=="AC4", "[41;60]", ">60"))))

ggplot(data=Tot_Sum_2020)+
  geom_col(position="stack", aes(x=AC, y=NbCases, fill=St))

pd <- position_dodge(0.5) # move them .05 to the left and right
ggplot(data=Tot_Sum_2020)+
  geom_point(position=pd, aes(x=AC, y=NbCases, colour=St))+
  geom_errorbar(aes(x=AC,ymin=Q2.5, ymax=Q97.5, colour=St), width=.1, position=pd) +
  theme_classic()+labs(title = "National cases of Chagas Disease (Total)", y="Number of cases", x="Age classes")+ theme(legend.position="right")+
  scale_color_manual(name="Disease Stages", 
                     breaks=c("StAc", "StAs", "StCh", "StChS"),
                     labels=c("Acute", "Asymptomatic", "Chronic Mild", "Chronic Severe"),
                     values = c("#00CC66", "#3399FF", "#FF9933","#990066"))+
  scale_x_discrete(labels=c("[0;5]"  , "[6;20]" , "[21;40]", "[41;60]" ,">60"  ))

ggsave("ML_inputs/res/Graphs/Graph_NbCases_AC_National_Total.png")
