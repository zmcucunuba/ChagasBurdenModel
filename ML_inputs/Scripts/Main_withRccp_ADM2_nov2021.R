#####################################################
#====================================================
#             MODEL CHAGAS BURDEN WITH ML INPUTS              
#====================================================
#####################################################

# 08/11/2021
# Julia Ledien, Zulma Cucunuba
# FoI inputs model : ML_A3_RFlog

rm(list=ls(all.names=TRUE))
library(xtable); library(Hmisc); require(ggplot2); library(reshape); require(grid);library(Rcpp); library(readxl)

source('ML_inputs/Scripts/ADM2_FormatingData.R')
source('ML_inputs/Scripts/ADM2_GetParameterDistributions.R')
sourceCpp('ML_inputs/Scripts/Rcpp_BurdenModel2.cpp')

#======== To be defined ==============
setting <- "rural"
Nb_iter=100
years<- 1985:2020
# ====================================


dico<- read_xlsx("ML_inputs/Data/dem/dictionnaire ADM2 Names.xlsx")
municipalities<- unique(dico$GID_2_GDAM)

municipalities <- municipalities[is.na(municipalities)==F]
iterdis<-Nb_iter

source("ML_inputs/Scripts/Generate_output_files.R")


for (place_name in municipalities){
  
  lambdaNy <- ADM2_FOI_import_and_format(place_name, setting,Nb_iter)
  params   <- ADM2_get_parameter_distribution(lambdaNy, place_name, setting)
  
  # params$alphaD <-params$alphaD *(1+5/100)     Need to keep this row?????
  output <- Rcpp_BurdenModel2(params)
  
  
  S_Array<- output$S
  Sm_Array<- output$Sm
  Ss_Array<- output$Ss
  Am_Array<- output$Am
  As_Array<- output$As
  I_Array<- output$I
  Cm_Array<- output$Cm
  Cs_Array<- output$Cs
  Da_Array<- output$Da
  Di_Array<- output$Di
  Dm_Array<- output$Dm
  DmI_Array<- output$DmI
  Ds_Array<- output$Ds
  DsI_Array<- output$DsI
  Du_Array<- output$Du
  DmS_Array<- output$DmS
  DsS_Array<- output$DsS
  
  rm(output)
  Obs.Pop<- ADM2_Pop_import_and_format(place_name, setting_type )
  
  #========================================
  # Number of Cases by year from 1985 to 2020 at ADM2 level
  
  TotPop = (S_Array + Sm_Array + Ss_Array + Am_Array + As_Array + I_Array + Cm_Array + Cs_Array) # Total Population per year
  StAc_Pop <-  Am_Array + As_Array 
  StAs_Pop <-  I_Array 
  StCh_Pop <-  Cm_Array + Cs_Array
  StChS_Pop <- Cs_Array
  StI_Pop <-  Am_Array + As_Array + I_Array + Cm_Array + Cs_Array
  
  Nb_AgeClass<- nrow(Obs.Pop)
  
  Prev <-array(NA, dim=c(Nb_AgeClass, iterdis, 160))
  Prev_Ac<- Prev;  Prev_As<- Prev; Prev_Ch<- Prev; Prev_ChS<- Prev; Prev_I<- Prev;
  for (l in 1:160){
    for(n in 1:iterdis){
      for(k in 1:85){
        Prev_Ac[k,n,l]<-  StAc_Pop[k,n,l]/TotPop[k,n,l]
        Prev_As[k,n,l]<-  StAs_Pop[k,n,l]/TotPop[k,n,l]
        Prev_Ch[k,n,l]<-  StCh_Pop[k,n,l]/TotPop[k,n,l]
        Prev_ChS[k,n,l]<-  StChS_Pop[k,n,l]/TotPop[k,n,l]
        Prev_I[k,n,l]<-  StI_Pop[k,n,l]/TotPop[k,n,l]
      }
      for (k in 85: Nb_AgeClass){
        Prev_Ac[k,n,l]<-  StAc_Pop[85,n,l]/TotPop[85,n,l] #extending data to all age classes
        Prev_As[k,n,l]<-  StAs_Pop[85,n,l]/TotPop[85,n,l]
        Prev_Ch[k,n,l]<-  StCh_Pop[85,n,l]/TotPop[85,n,l]
        Prev_ChS[k,n,l]<-  StChS_Pop[85,n,l]/TotPop[85,n,l]
        Prev_I[k,n,l]<-  StI_Pop[85,n,l]/TotPop[85,n,l]
      }}}
  
  Prev_Ac.36y<- Prev_Ac[,,(115:150)] # from 1985 to 2020
  Prev_As.36y<- Prev_As[,,(115:150)]
  Prev_Ch.36y<- Prev_Ch[,,(115:150)]
  Prev_ChS.36y<- Prev_ChS[,,(115:150)]
  Prev_I.36y<- Prev_I[,,(115:150)]
 
  Case <- array(NA, dim=c(Nb_AgeClass,iterdis,36)) # TOTAL number of cases
  Case_Ac<- Case;  Case_As<- Case; Case_Ch<- Case; Case_ChS<- Case; Case_I<- Case;
  
  for (i in 1:iterdis){
    for (j in 1:Nb_AgeClass){
      for (k in 1:36){
        Case_Ac[j,i,k]<- (Prev_Ac.36y[j,i,k] * Obs.Pop[j,k] )
        Case_As[j,i,k]<- (Prev_As.36y[j,i,k] * Obs.Pop[j,k] )
        Case_Ch[j,i,k]<- (Prev_Ch.36y[j,i,k] * Obs.Pop[j,k] )
        Case_ChS[j,i,k]<- (Prev_ChS.36y[j,i,k] * Obs.Pop[j,k] )
        Case_I[j,i,k]<- (Prev_I.36y[j,i,k] * Obs.Pop[j,k] )
      }}}
  
  Cases_AC<- matrix(NA, iterdis,36)
  Cases_AC1_Ac<- Cases_AC; Cases_AC1_As<- Cases_AC; Cases_AC1_Ch<- Cases_AC;Cases_AC1_ChS<- Cases_AC; Cases_AC1_I<- Cases_AC
  Cases_AC2_Ac<- Cases_AC; Cases_AC2_As<- Cases_AC; Cases_AC2_Ch<- Cases_AC;Cases_AC2_ChS<- Cases_AC; Cases_AC2_I<- Cases_AC
  Cases_AC3_Ac<- Cases_AC; Cases_AC3_As<- Cases_AC; Cases_AC3_Ch<- Cases_AC;Cases_AC3_ChS<- Cases_AC; Cases_AC3_I<- Cases_AC
  Cases_AC4_Ac<- Cases_AC; Cases_AC4_As<- Cases_AC; Cases_AC4_Ch<- Cases_AC;Cases_AC4_ChS<- Cases_AC; Cases_AC4_I<- Cases_AC
  Cases_AC5_Ac<- Cases_AC; Cases_AC5_As<- Cases_AC; Cases_AC5_Ch<- Cases_AC;Cases_AC5_ChS<- Cases_AC; Cases_AC5_I<- Cases_AC
  Cases_ACt_Ac<- Cases_AC; Cases_ACt_As<- Cases_AC; Cases_ACt_Ch<- Cases_AC;Cases_ACt_ChS<- Cases_AC; Cases_ACt_I<- Cases_AC
  
  
  for(i in 1:iterdis){
    for(k in 1:36){
      Cases_AC1_Ac[i,k]<- sum(Case_Ac[1:6,i,k]) #aged 0 to 5
      Cases_AC1_As[i,k]<- sum(Case_As[1:6,i,k])
      Cases_AC1_Ch[i,k]<- sum(Case_Ch[1:6,i,k])
      Cases_AC1_ChS[i,k]<- sum(Case_ChS[1:6,i,k])
      Cases_AC1_I[i,k]<- sum(Case_I[1:6,i,k])
      
      Cases_AC2_Ac[i,k]<- sum(Case_Ac[7:21,i,k]) #aged 6 to 20
      Cases_AC2_As[i,k]<- sum(Case_As[7:21,i,k])
      Cases_AC2_Ch[i,k]<- sum(Case_Ch[7:21,i,k])
      Cases_AC2_ChS[i,k]<- sum(Case_ChS[7:21,i,k])
      Cases_AC2_I[i,k]<- sum(Case_I[7:21,i,k])
      
      Cases_AC3_Ac[i,k]<- sum(Case_Ac[22:41,i,k])#aged 21 to 40
      Cases_AC3_As[i,k]<- sum(Case_As[22:41,i,k])
      Cases_AC3_Ch[i,k]<- sum(Case_Ch[22:41,i,k])
      Cases_AC3_ChS[i,k]<- sum(Case_ChS[22:41,i,k])
      Cases_AC3_I[i,k]<- sum(Case_I[22:41,i,k])
      
      Cases_AC4_Ac[i,k]<- sum(Case_Ac[42:61,i,k])#aged 41 to 60
      Cases_AC4_As[i,k]<- sum(Case_As[42:61,i,k])
      Cases_AC4_Ch[i,k]<- sum(Case_Ch[42:61,i,k])
      Cases_AC4_ChS[i,k]<- sum(Case_ChS[42:61,i,k])
      Cases_AC4_I[i,k]<- sum(Case_I[42:61,i,k])
      
      Cases_AC5_Ac[i,k]<- sum(Case_Ac[62:Nb_AgeClass,i,k])#aged > 60
      Cases_AC5_As[i,k]<- sum(Case_As[62:Nb_AgeClass,i,k])
      Cases_AC5_Ch[i,k]<- sum(Case_Ch[62:Nb_AgeClass,i,k])
      Cases_AC5_ChS[i,k]<- sum(Case_ChS[62:Nb_AgeClass,i,k])
      Cases_AC5_I[i,k]<- sum(Case_I[62:Nb_AgeClass,i,k])
      
      Cases_ACt_Ac[i,k]<- sum(Case_Ac[,i,k])# all ages
      Cases_ACt_As[i,k]<- sum(Case_As[,i,k])
      Cases_ACt_Ch[i,k]<- sum(Case_Ch[,i,k])
      Cases_ACt_ChS[i,k]<- sum(Case_ChS[,i,k])
      Cases_ACt_I[i,k]<- sum(Case_I[,i,k])
    }
  }
  
  
  IClo<-function(x){
    mean(x)-qnorm(.975)*(sd(x)/sqrt(iterdis))
  }
  ICup<-function(x){
    mean(x)+qnorm(.975)*(sd(x)/sqrt(iterdis))
  }
  
  suma<- function (x){
    c(min(x), quantile(x, c(0.25, 0.5, 0.75)), max(x),mean(x), sd(x),IClo(x),ICup(x))
  }
  
  for (k in 1:36){
  NbCases_AC1_StAc[place_name,,k] <- suma(Cases_AC1_Ac[,k])
  NbCases_AC1_StAs [place_name,,k] <- suma(Cases_AC1_As[,k])
  NbCases_AC1_StCh [place_name,,k] <- suma(Cases_AC1_Ch[,k])
  NbCases_AC1_StChS [place_name,,k] <- suma(Cases_AC1_ChS[,k])
  NbCases_AC1_StI [place_name,,k] <- suma(Cases_AC1_I[,k])
  
  NbCases_AC2_StAc <- template
  NbCases_AC2_StAs <- template
  NbCases_AC2_StCh <- template
  NbCases_AC2_StChS <- template
  NbCases_AC2_StI <- template
  
  NbCases_AC3_StAc <- template
  NbCases_AC3_StAs <- template
  NbCases_AC3_StCh <- template
  NbCases_AC3_StChS <- template
  NbCases_AC3_StI <- template
  
  NbCases_AC4_StAc <- template
  NbCases_AC4_StAs <- template
  NbCases_AC4_StCh <- template
  NbCases_AC4_StChS <- template
  NbCases_AC4_StI <- template
  
  NbCases_AC5_StAc <- template
  NbCases_AC5_StAs <- template
  NbCases_AC5_StCh <- template
  NbCases_AC5_StChS <- template
  NbCases_AC5_StI <- template
  
  NbCases_ACt_StAc <- template
  NbCases_ACt_StAs <- template
  NbCases_ACt_StCh <- template
  NbCases_ACt_StChS <- template
  NbCases_ACt_StI <- template
  }
  
}