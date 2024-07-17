#====================================================================
#####################################################################
#               CHAGAS DISEASE BURDEN MODEL
#        INPUT DATA GENERATION FOR ML MODELS
#####################################################################
#====================================================================
# Julia Ledien 08/10/2021

## Municipality data level
## Shapefile = GDAM
## Models: ML_A3_RFlog

ADM2<-readRDS("ML_inputs/Data/RawFoi/ADM2_Codes_DPMP")
RawU<- readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_urb_10it_10boots")
RawR<- readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_rur_10it_10boots")
RawI<- readRDS("ML_inputs/Data/RawFoi/RFlog_NestedResampled_noSS_noLatLon_10folds_pred_new_ind_10it_10boots")

it=100

for(i in 1:length(ADM2)){
  t<- as.data.frame(t(RawU[i,,1:it]))
  colnames(t)<-1950:(1950+ncol(t)-1)
  saveRDS(t,paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_urban_", ADM2[i]))
}

for(i in 1:length(ADM2)){
  t<- as.data.frame(t(RawR[i,,1:it]))
  colnames(t)<-1950:(1950+ncol(t)-1)
  saveRDS(t,paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_rural_", ADM2[i]))
}

for(i in 1:length(ADM2)){
  t<- as.data.frame(t(RawI[i,,1:it]))
  colnames(t)<-1950:(1950+ncol(t)-1)
  saveRDS(t,paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_indigenous_", ADM2[i]))
}
