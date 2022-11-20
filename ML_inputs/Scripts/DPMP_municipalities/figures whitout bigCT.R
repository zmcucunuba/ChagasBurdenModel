##estimates without big cities


library (readxl)
PopSize1985<- read_xlsx("C:/Users/ledie/Box/notes/PopSize1985.xlsx")
BigCities<- PopSize1985$DPMP[which(PopSize1985$`1985`>100000)]
Municipalities<- PopSize1985$DPMP[which(PopSize1985$`1985`<=100000)]

NbCases_ACt_StI_urb<- readRDS("ML_inputs/res/NbCases_ACt_StI_urbanBigCT_100_it")

bigCt<- NbCases_ACt_StI_urb[paste0("CO",BigCities),,]
bigCt2020<-bigCt[,,36]
sum(bigCt[,3,36])
sum(bigCt[,3,11])


##deaths

NbCases_ACt_StI_urb<- readRDS("ML_inputs/res/Nbdeath_ACt_StI_urbanBigCT_100_it")

bigCt<- NbCases_ACt_StI_urb[paste0("CO",BigCities),,]
bigCt2020<-bigCt[,,36]
sum(bigCt[,3,36])
sum(bigCt[,3,11])

##
