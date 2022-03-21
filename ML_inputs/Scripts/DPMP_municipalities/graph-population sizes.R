#### population data analyses

library(readxl)
library(ggplot2)

brut<- read_xlsx("C:/Users/ledie/Dropbox/Chagas modelling/BurdenModel/Population_data/Brut_ADM2_population_size.xlsx")

Urban<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Cabecera Municipal")
Rural<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Centros Poblados y Rural Disperso")
Total<- subset(brut, brut$`ÁREA GEOGRÁFICA`== "Total")

years<- 1985:2020

PopTotal<- matrix(NA, length(years), 2)
PopUrban<- matrix(NA, length(years), 2)

for (i in 1:length(years)){
  PopTotal[i,]<- c(years[i],sum(Total[which(Total$AÑO==years[i]),7:107]))
  PopUrban[i,]<- c(years[i],sum(Urban[which(Total$AÑO==years[i]),7:107]))
  
}
PopTotal<- as.data.frame(cbind(PopTotal, PopUrban[,2]))
colnames(PopTotal)<- c("Year", "Population", "urb")

ggplot(data=PopTotal)+ geom_line(aes(x=Year, y=Population)) + theme_classic()+coord_cartesian(ylim=c(0, 6e+7)) + geom_line(aes(x=Year, y=urb), color="blue")
ggsave("ML_inputs/res/Plot_PopTime.png")

(PopTotal$Pop[which(PopTotal$Year==2020)]-PopTotal$Pop[which(PopTotal$Year==1995)])/PopTotal$Pop[which(PopTotal$Year==1995)]

AC<-c("AC1", "AC2", "AC3", "AC4", "AC5")

PopTotalAC<- matrix(NA, length(AC), 3)

PopTotalAC[1,]<- c(AC[1],sum(Total[which(Total$AÑO=="2020"),7:12]), sum(Total[which(Total$AÑO=="1990"),7:12]))
PopTotalAC[2,]<- c(AC[2],sum(Total[which(Total$AÑO=="2020"),13:27]), sum(Total[which(Total$AÑO=="1990"),13:27]))
PopTotalAC[3,]<- c(AC[3],sum(Total[which(Total$AÑO=="2020"),28:47]), sum(Total[which(Total$AÑO=="1990"),28:47]))
PopTotalAC[4,]<- c(AC[4],sum(Total[which(Total$AÑO=="2020"),48:67]), sum(Total[which(Total$AÑO=="1990"),48:67]))
PopTotalAC[5,]<- c(AC[5],sum(Total[which(Total$AÑO=="2020"),68:107]), sum(Total[which(Total$AÑO=="1990"),68:107]))

PopTotalAC<- as.data.frame(PopTotalAC)
colnames(PopTotalAC)<- c("AC", "Total2020", "Total1990")
PopTotalAC$Total2020 <- as.numeric(PopTotalAC$Total2020 )
PopTotalAC$Total1990 <- as.numeric(PopTotalAC$Total1990 )

ggplot(data=PopTotalAC)+ geom_col(aes(x=Total2020, y=AC), fill="#99CCCC")+ theme_classic()+coord_cartesian(xlim=c(0, 1.5e+7))
ggsave("ML_inputs/res/Plot_PopAC2020.png")

ggplot(data=PopTotalAC)+ geom_col(aes(x=Total1990, y=AC), fill="#006666")+ theme_classic() +coord_cartesian(xlim=c(0, 1.5e+7))
ggsave("ML_inputs/res/Plot_PopAC1990.png")



