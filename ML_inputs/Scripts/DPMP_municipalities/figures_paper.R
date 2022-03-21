###number in the text od the paper


urbCases<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_urbanBigCT_100_it")
rurCases<- readRDS("ML_inputs/res/National_NbCases_ACt_StI_rural_100_it")

totCases<- urbCases+rurCases

median(urbCases[,36])/median(totCases[,36])
(median(totCases[,36])-median(totCases[,11]))/median(totCases[,11])

#
SevurbCases<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_urbanBigCT_100_it")
SevrurCases<- readRDS("ML_inputs/res/National_NbCases_ACt_StChS_rural_100_it")

SevtotCases<- SevurbCases+SevrurCases
(median(SevtotCases[,36])-median(SevtotCases[,11]))/median(SevtotCases[,11])

#
urbDeaths<- readRDS("ML_inputs/res/National_NbDeath_ACt_StChS_urbanBigCT_100_it")
rurDeath<- readRDS("ML_inputs/res/National_NbDeath_ACt_StChS_rural_100_it")

totDeath<- urbDeaths+rurDeath
(median(totDeath[,36])-median(totDeath[,11]))/median(totDeath[,11])

#
FoI_urb<-readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_urb_10it_10boots")
FoI_rur<-readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_rur_10it_10boots")
FoI_ind<-readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_ind_10it_10boots")

dim(FoI_urb2020)
ye<- 1950:2020
summary(apply(FoI_urb[,31,], 1, median))# median of the municipal median foi in 1980
summary(apply(FoI_rur[,31,], 1, median))# median of the municipal median foi in 1980

summary(apply(FoI_urb[,61,], 1, median))# median of the municipal median foi in 2010
summary(apply(FoI_rur[,61,], 1, median))# median of the municipal median foi in 2010

summary(apply(FoI_urb[,46,], 1, median))# median of the municipal median foi in 1995
summary(apply(FoI_rur[,46,], 1, median))# median of the municipal median foi in 1995

summary(apply(FoI_urb[,71,], 1, median))# median of the municipal median foi in 2020
summary(apply(FoI_rur[,71,], 1, median))# median of the municipal median foi in 2020


#
library(rgdal)
MADCV_rur<-readOGR("C:/Users/ledie/Box/Analyses/MachineLearning/MunLM/BurdenModel/FoI_Inputs/RFlog_1000it/Shapefiles/RFlog_NestedResampled_noSS_noLatLon_10folds_rural_MAD_CV.shp")
MADCV_urb<-readOGR("C:/Users/ledie/Box/Analyses/MachineLearning/MunLM/BurdenModel/FoI_Inputs/RFlog_1000it/Shapefiles/RFlog_NestedResampled_noSS_noLatLon_10folds_urban_MAD_CV.shp")

length(MADCV_rur$MAD_CV_198[which(MADCV_rur$MAD_CV_198>=2)])# number of municipalities with cv>2 in 1980
length(MADCV_urb$MAD_CV_198[which(MADCV_urb$MAD_CV_198>=2)])

length(MADCV_rur$MAD_CV_210[which(MADCV_rur$MAD_CV_210>=2)])# number of municipalities with cv>2 in 2010
length(MADCV_urb$MAD_CV_210[which(MADCV_urb$MAD_CV_210>=2)])

length(MADCV_rur$MAD_CV_141[which(MADCV_rur$MAD_CV_141>=2)])# number of municipalities with cv>2 in 1995
length(MADCV_urb$MAD_CV_141[which(MADCV_urb$MAD_CV_141>=2)])

length(MADCV_rur$MAD_CV_202[which(MADCV_rur$MAD_CV_202>=2)])# number of municipalities with cv>2 in 2020
length(MADCV_urb$MAD_CV_202[which(MADCV_urb$MAD_CV_202>=2)])
