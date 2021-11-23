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
source ("ML_inputs/Scripts/FUN.R")

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
  Obs.Pop<- ADM2_Pop_import_and_format(place_name, setting )
  DeathsObs<-ADM2_Deaths_import_and_format(place_name, setting)
  
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
  BU_prev_Am<- Prev; BU_prev_As<- Prev; BU_prev_Cm<- Prev
  
  for (l in 1:160){
    for(n in 1:iterdis){
      for(k in 1:85){
        Prev_Ac[k,n,l]<-  StAc_Pop[k,n,l]/TotPop[k,n,l]
        Prev_As[k,n,l]<-  StAs_Pop[k,n,l]/TotPop[k,n,l]
        Prev_Ch[k,n,l]<-  StCh_Pop[k,n,l]/TotPop[k,n,l]
        Prev_ChS[k,n,l]<-  StChS_Pop[k,n,l]/TotPop[k,n,l]
        Prev_I[k,n,l]<-  StI_Pop[k,n,l]/TotPop[k,n,l]
        BU_prev_Am[k,n,l]<-  Am_Array[k,n,l]/TotPop[k,n,l]
        BU_prev_As[k,n,l]<-  As_Array[k,n,l]/TotPop[k,n,l]
        BU_prev_Cm[k,n,l]<-  Cm_Array[k,n,l]/TotPop[k,n,l]
      }
      for (k in 85: Nb_AgeClass){
        Prev_Ac[k,n,l]<-  StAc_Pop[85,n,l]/TotPop[85,n,l] #extending data to all age classes
        Prev_As[k,n,l]<-  StAs_Pop[85,n,l]/TotPop[85,n,l]
        Prev_Ch[k,n,l]<-  StCh_Pop[85,n,l]/TotPop[85,n,l]
        Prev_ChS[k,n,l]<-  StChS_Pop[85,n,l]/TotPop[85,n,l]
        Prev_I[k,n,l]<-  StI_Pop[85,n,l]/TotPop[85,n,l]
        BU_prev_Am[k,n,l]<-  Am_Array[85,n,l]/TotPop[85,n,l]
        BU_prev_As[k,n,l]<-  As_Array[85,n,l]/TotPop[85,n,l]
        BU_prev_Cm[k,n,l]<-  Cm_Array[85,n,l]/TotPop[85,n,l]
      }}}
  
  Prev_Ac.36y<- Prev_Ac[,,(115:150)] # from 1985 to 2020
  Prev_As.36y<- Prev_As[,,(115:150)]
  Prev_Ch.36y<- Prev_Ch[,,(115:150)]
  Prev_ChS.36y<- Prev_ChS[,,(115:150)]
  Prev_I.36y<- Prev_I[,,(115:150)]
  BU_prev_Am.36y<-BU_prev_Am[,,(115:150)]
  BU_prev_As.36y<-BU_prev_As[,,(115:150)]
  BU_prev_Cm.36y<-BU_prev_Cm[,,(115:150)]
  
  Case <- array(NA, dim=c(Nb_AgeClass,iterdis,36)) # TOTAL number of cases
  Case_Ac<- Case;  Case_As<- Case; Case_Ch<- Case; Case_ChS<- Case; Case_I<- Case;
  BU_Case_Am<- Case; BU_Case_As<- Case; BU_Case_Cm<- Case;
  
  for (i in 1:iterdis){
    for (j in 1:Nb_AgeClass){
      for (k in 1:36){
        Case_Ac[j,i,k]<- (Prev_Ac.36y[j,i,k] * Obs.Pop[j,k] )
        Case_As[j,i,k]<- (Prev_As.36y[j,i,k] * Obs.Pop[j,k] )
        Case_Ch[j,i,k]<- (Prev_Ch.36y[j,i,k] * Obs.Pop[j,k] )
        Case_ChS[j,i,k]<- (Prev_ChS.36y[j,i,k] * Obs.Pop[j,k] )
        Case_I[j,i,k]<- (Prev_I.36y[j,i,k] * Obs.Pop[j,k] )
        BU_Case_Am[j,i,k]<- (BU_prev_Am.36y[j,i,k] * Obs.Pop[j,k] )
        BU_Case_As[j,i,k]<- (BU_prev_As.36y[j,i,k] * Obs.Pop[j,k] )
        BU_Case_Cm[j,i,k]<- (BU_prev_Cm.36y[j,i,k] * Obs.Pop[j,k] )
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
  
  NbCases_AC2_StAc [place_name,,k] <- suma(Cases_AC2_Ac[,k])
  NbCases_AC2_StAs [place_name,,k] <- suma(Cases_AC2_As[,k])
  NbCases_AC2_StCh[place_name,,k] <- suma(Cases_AC2_Ch[,k])
  NbCases_AC2_StChS [place_name,,k] <- suma(Cases_AC2_ChS[,k])
  NbCases_AC2_StI [place_name,,k] <- suma(Cases_AC2_I[,k])
  
  NbCases_AC3_StAc [place_name,,k] <- suma(Cases_AC3_Ac[,k])
  NbCases_AC3_StAs [place_name,,k] <- suma(Cases_AC3_As[,k])
  NbCases_AC3_StCh [place_name,,k] <- suma(Cases_AC3_Ch[,k])
  NbCases_AC3_StChS [place_name,,k] <- suma(Cases_AC3_ChS[,k])
  NbCases_AC3_StI [place_name,,k] <- suma(Cases_AC3_I[,k])
  
  NbCases_AC4_StAc [place_name,,k] <- suma(Cases_AC4_Ac[,k])
  NbCases_AC4_StAs [place_name,,k] <- suma(Cases_AC4_As[,k])
  NbCases_AC4_StCh [place_name,,k] <- suma(Cases_AC4_Ch[,k])
  NbCases_AC4_StChS [place_name,,k] <- suma(Cases_AC4_ChS[,k])
  NbCases_AC4_StI [place_name,,k] <- suma(Cases_AC4_I[,k])
  
  NbCases_AC5_StAc [place_name,,k] <- suma(Cases_AC5_Ac[,k])
  NbCases_AC5_StAs [place_name,,k] <- suma(Cases_AC5_As[,k])
  NbCases_AC5_StCh [place_name,,k] <- suma(Cases_AC5_Ch[,k])
  NbCases_AC5_StChS [place_name,,k] <- suma(Cases_AC5_ChS[,k])
  NbCases_AC5_StI [place_name,,k] <- suma(Cases_AC5_I[,k])
  
  NbCases_ACt_StAc [place_name,,k] <- suma(Cases_ACt_Ac[,k])
  NbCases_ACt_StAs [place_name,,k] <- suma(Cases_ACt_As[,k])
  NbCases_ACt_StCh [place_name,,k] <- suma(Cases_ACt_Ch[,k])
  NbCases_ACt_StChS [place_name,,k] <- suma(Cases_ACt_ChS[,k])
  NbCases_ACt_StI [place_name,,k] <- suma(Cases_ACt_I[,k])
  }
  
  #========================================
  # Number of Deaths by year from 1985 to 2020 at ADM2 level
  
  TotDeaths <- (Du_Array + Da_Array + Di_Array + Dm_Array + Ds_Array + DmI_Array+ DsI_Array + DmS_Array+ DsS_Array) # Total Deaths per year 
  StAc_Deaths <-  Da_Array 
  StAs_Deaths <-  Di_Array 
  StCh_Deaths <-  Dm_Array + Ds_Array
  StChS_Deaths <- Ds_Array
  StI_Deaths <-  Da_Array + Di_Array +  Dm_Array + Ds_Array
  
  Nb_AgeClass<- nrow(Obs.Pop)
  
  PrevD <-array(NA, dim=c(Nb_AgeClass, iterdis, 160))
  PrevD_Ac<- PrevD;  PrevD_As<- PrevD; PrevD_Ch<- PrevD; PrevD_ChS<- PrevD; PrevD_I<- PrevD;
  
  for (l in 1:160){
    for(n in 1:iterdis){
      for(k in 1:85){
        PrevD_Ac[k,n,l]<-  StAc_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_As[k,n,l]<-  StAs_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_Ch[k,n,l]<-  StCh_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_ChS[k,n,l]<-  StChS_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_I[k,n,l]<-  StI_Deaths[k,n,l]/TotDeaths[k,n,l]
      }
      for (k in 85: Nb_AgeClass){
        PrevD_Ac[k,n,l]<-  StAc_Deaths[85,n,l]/TotDeaths[85,n,l] #extending data to all age classes
        PrevD_As[k,n,l]<-  StAs_Deaths[85,n,l]/TotDeaths[85,n,l]
        PrevD_Ch[k,n,l]<-  StCh_Deaths[85,n,l]/TotDeaths[85,n,l]
        PrevD_ChS[k,n,l]<-  StChS_Deaths[85,n,l]/TotDeaths[85,n,l]
        PrevD_I[k,n,l]<-  StI_Deaths[85,n,l]/TotDeaths[85,n,l]
      }}}
  
  
  PrevD_Ac_23AC<- AC101to23(PrevD_Ac)
  PrevD_As_23AC<- AC101to23(PrevD_As)
  PrevD_Ch_23AC<- AC101to23(PrevD_Ch)
  PrevD_ChS_23AC<- AC101to23(PrevD_ChS)
  PrevD_I_23AC<- AC101to23(PrevD_I)
  
  PrevD_Ac_23AC.36y<-PrevD_Ac_23AC[,,(115:150)] # from 1985 to 2020
  PrevD_As_23AC.36y<-PrevD_As_23AC[,,(115:150)] 
  PrevD_Ch_23AC.36y<-PrevD_Ch_23AC[,,(115:150)] 
  PrevD_ChS_23AC.36y<-PrevD_ChS_23AC[,,(115:150)] 
  PrevD_I_23AC.36y<-PrevD_I_23AC[,,(115:150)] 
  
  Dyears<- as.character(1985:2020)
  Obs.Deaths <- matrix(NA, 23, 36)
  
  
  for (k in 1:34){
    Obs.Deaths[1,k] <- sum(DeathsObs[1:6,Dyears[k]])
    for (i in 2:23){
      Obs.Deaths[i,k] <- DeathsObs[(i+6),Dyears[k]]
    } } 
  for (k in 35:36){
    Obs.Deaths[1,k] <- sum(DeathsObs[1:6,Dyears[34]])
    for (i in 2:23){
      Obs.Deaths[i,k] <- DeathsObs[(i+6),Dyears[34]]
  }}
  
  
  Death <- array(NA, dim=c(23,iterdis,36)) # TOTAL number of Deaths
  Death_Ac<- Death;  Death_As<- Death; Death_Ch<- Death; Death_ChS<- Death; Death_I<- Death;
  
  for (i in 1:iterdis){
    for (j in 1:23){
      for (k in 1:36){
        Death_Ac[j,i,k]<- (PrevD_Ac_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_As[j,i,k]<- (PrevD_As_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_Ch[j,i,k]<- (PrevD_Ch_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_ChS[j,i,k]<- (PrevD_ChS_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_I[j,i,k]<- (PrevD_I_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
      }}}
  
  Deaths_AC<- matrix(NA, iterdis,36)
  Deaths_AC1_Ac<- Deaths_AC; Deaths_AC1_As<- Deaths_AC; Deaths_AC1_Ch<- Deaths_AC;Deaths_AC1_ChS<- Deaths_AC; Deaths_AC1_I<- Deaths_AC
  Deaths_AC2_Ac<- Deaths_AC; Deaths_AC2_As<- Deaths_AC; Deaths_AC2_Ch<- Deaths_AC;Deaths_AC2_ChS<- Deaths_AC; Deaths_AC2_I<- Deaths_AC
  Deaths_AC3_Ac<- Deaths_AC; Deaths_AC3_As<- Deaths_AC; Deaths_AC3_Ch<- Deaths_AC;Deaths_AC3_ChS<- Deaths_AC; Deaths_AC3_I<- Deaths_AC
  Deaths_AC4_Ac<- Deaths_AC; Deaths_AC4_As<- Deaths_AC; Deaths_AC4_Ch<- Deaths_AC;Deaths_AC4_ChS<- Deaths_AC; Deaths_AC4_I<- Deaths_AC
  Deaths_AC5_Ac<- Deaths_AC; Deaths_AC5_As<- Deaths_AC; Deaths_AC5_Ch<- Deaths_AC;Deaths_AC5_ChS<- Deaths_AC; Deaths_AC5_I<- Deaths_AC
  Deaths_ACt_Ac<- Deaths_AC; Deaths_ACt_As<- Deaths_AC; Deaths_ACt_Ch<- Deaths_AC;Deaths_ACt_ChS<- Deaths_AC; Deaths_ACt_I<- Deaths_AC
  
  
  for(i in 1:iterdis){
    for(k in 1:36){
      Deaths_AC1_Ac[i,k]<- sum(Death_Ac[1:3,i,k]) #aged 0 to 5
      Deaths_AC1_As[i,k]<- sum(Death_As[1:3,i,k])
      Deaths_AC1_Ch[i,k]<- sum(Death_Ch[1:3,i,k])
      Deaths_AC1_ChS[i,k]<- sum(Death_ChS[1:3,i,k])
      Deaths_AC1_I[i,k]<- sum(Death_I[1:3,i,k])
      
      Deaths_AC2_Ac[i,k]<- sum(Death_Ac[4:6,i,k]) #aged 6 to 20
      Deaths_AC2_As[i,k]<- sum(Death_As[4:6,i,k])
      Deaths_AC2_Ch[i,k]<- sum(Death_Ch[4:6,i,k])
      Deaths_AC2_ChS[i,k]<- sum(Death_ChS[4:6,i,k])
      Deaths_AC2_I[i,k]<- sum(Death_I[4:6,i,k])
      
      Deaths_AC3_Ac[i,k]<- sum(Death_Ac[7:10,i,k])#aged 21 to 40
      Deaths_AC3_As[i,k]<- sum(Death_As[7:10,i,k])
      Deaths_AC3_Ch[i,k]<- sum(Death_Ch[7:10,i,k])
      Deaths_AC3_ChS[i,k]<- sum(Death_ChS[7:10,i,k])
      Deaths_AC3_I[i,k]<- sum(Death_I[7:10,i,k])
      
      Deaths_AC4_Ac[i,k]<- sum(Death_Ac[11:14,i,k])#aged 41 to 60
      Deaths_AC4_As[i,k]<- sum(Death_As[11:14,i,k])
      Deaths_AC4_Ch[i,k]<- sum(Death_Ch[11:14,i,k])
      Deaths_AC4_ChS[i,k]<- sum(Death_ChS[11:14,i,k])
      Deaths_AC4_I[i,k]<- sum(Death_I[11:14,i,k])
      
      Deaths_AC5_Ac[i,k]<- sum(Death_Ac[15:23,i,k])#aged > 60
      Deaths_AC5_As[i,k]<- sum(Death_As[15:23,i,k])
      Deaths_AC5_Ch[i,k]<- sum(Death_Ch[15:23,i,k])
      Deaths_AC5_ChS[i,k]<- sum(Death_ChS[15:23,i,k])
      Deaths_AC5_I[i,k]<- sum(Death_I[15:23,i,k])
      
      Deaths_ACt_Ac[i,k]<- sum(Death_Ac[,i,k])# all ages
      Deaths_ACt_As[i,k]<- sum(Death_As[,i,k])
      Deaths_ACt_Ch[i,k]<- sum(Death_Ch[,i,k])
      Deaths_ACt_ChS[i,k]<- sum(Death_ChS[,i,k])
      Deaths_ACt_I[i,k]<- sum(Death_I[,i,k])
    }
  }
  for (k in 1:36){
    NbDeath_AC1_StAc[place_name,,k] <- suma(Deaths_AC1_Ac[,k])
    NbDeath_AC1_StAs [place_name,,k] <- suma(Deaths_AC1_As[,k])
    NbDeath_AC1_StCh [place_name,,k] <- suma(Deaths_AC1_Ch[,k])
    NbDeath_AC1_StChS [place_name,,k] <- suma(Deaths_AC1_ChS[,k])
    NbDeath_AC1_StI [place_name,,k] <- suma(Deaths_AC1_I[,k])
    
    NbDeath_AC2_StAc [place_name,,k] <- suma(Deaths_AC2_Ac[,k])
    NbDeath_AC2_StAs [place_name,,k] <- suma(Deaths_AC2_As[,k])
    NbDeath_AC2_StCh[place_name,,k] <- suma(Deaths_AC2_Ch[,k])
    NbDeath_AC2_StChS [place_name,,k] <- suma(Deaths_AC2_ChS[,k])
    NbDeath_AC2_StI [place_name,,k] <- suma(Deaths_AC2_I[,k])
    
    NbDeath_AC3_StAc [place_name,,k] <- suma(Deaths_AC3_Ac[,k])
    NbDeath_AC3_StAs [place_name,,k] <- suma(Deaths_AC3_As[,k])
    NbDeath_AC3_StCh [place_name,,k] <- suma(Deaths_AC3_Ch[,k])
    NbDeath_AC3_StChS [place_name,,k] <- suma(Deaths_AC3_ChS[,k])
    NbDeath_AC3_StI [place_name,,k] <- suma(Deaths_AC3_I[,k])
    
    NbDeath_AC4_StAc [place_name,,k] <- suma(Deaths_AC4_Ac[,k])
    NbDeath_AC4_StAs [place_name,,k] <- suma(Deaths_AC4_As[,k])
    NbDeath_AC4_StCh [place_name,,k] <- suma(Deaths_AC4_Ch[,k])
    NbDeath_AC4_StChS [place_name,,k] <- suma(Deaths_AC4_ChS[,k])
    NbDeath_AC4_StI [place_name,,k] <- suma(Deaths_AC4_I[,k])
    
    NbDeath_AC5_StAc [place_name,,k] <- suma(Deaths_AC5_Ac[,k])
    NbDeath_AC5_StAs [place_name,,k] <- suma(Deaths_AC5_As[,k])
    NbDeath_AC5_StCh [place_name,,k] <- suma(Deaths_AC5_Ch[,k])
    NbDeath_AC5_StChS [place_name,,k] <- suma(Deaths_AC5_ChS[,k])
    NbDeath_AC5_StI [place_name,,k] <- suma(Deaths_AC5_I[,k])
    
    NbDeath_ACt_StAc [place_name,,k] <- suma(Deaths_ACt_Ac[,k])
    NbDeath_ACt_StAs [place_name,,k] <- suma(Deaths_ACt_As[,k])
    NbDeath_ACt_StCh [place_name,,k] <- suma(Deaths_ACt_Ch[,k])
    NbDeath_ACt_StChS [place_name,,k] <- suma(Deaths_ACt_ChS[,k])
    NbDeath_ACt_StI [place_name,,k] <- suma(Deaths_ACt_I[,k])
    

  }
  
  #========================================
  # Burden from 1985 to 2020 at ADM2 level
  
  #================================== 
  #                YLL 
  #==================================
  
  YLL <- array(NA, dim=c(23,iterdis,36))
  YLL_Ac<-YLL; YLL_As<-YLL;YLL_Ch<-YLL;YLL_ChS<-YLL;YLL_I<-YLL;
  
  for (k in 1:36){  
    for (i in 1:iterdis){  
      for (j in 1:23){
        YLL_Ac[j,i,k]<- Death_Ac[j,i,k] * LEx5[j]
        YLL_As[j,i,k]<- Death_As[j,i,k] * LEx5[j]
        YLL_Ch[j,i,k]<- Death_Ch[j,i,k] * LEx5[j]
        YLL_ChS[j,i,k]<- Death_ChS[j,i,k] * LEx5[j]
        YLL_I[j,i,k]<- Death_I[j,i,k] * LEx5[j]
        }}}
 
  YLL_AC<- matrix(NA, iterdis,36)
  YLL_AC1_Ac<- YLL_AC; YLL_AC1_As<- YLL_AC; YLL_AC1_Ch<- YLL_AC;YLL_AC1_ChS<- YLL_AC; YLL_AC1_I<- YLL_AC
  YLL_AC2_Ac<- YLL_AC; YLL_AC2_As<- YLL_AC; YLL_AC2_Ch<- YLL_AC;YLL_AC2_ChS<- YLL_AC; YLL_AC2_I<- YLL_AC
  YLL_AC3_Ac<- YLL_AC; YLL_AC3_As<- YLL_AC; YLL_AC3_Ch<- YLL_AC;YLL_AC3_ChS<- YLL_AC; YLL_AC3_I<- YLL_AC
  YLL_AC4_Ac<- YLL_AC; YLL_AC4_As<- YLL_AC; YLL_AC4_Ch<- YLL_AC;YLL_AC4_ChS<- YLL_AC; YLL_AC4_I<- YLL_AC
  YLL_AC5_Ac<- YLL_AC; YLL_AC5_As<- YLL_AC; YLL_AC5_Ch<- YLL_AC;YLL_AC5_ChS<- YLL_AC; YLL_AC5_I<- YLL_AC
  YLL_ACt_Ac<- YLL_AC; YLL_ACt_As<- YLL_AC; YLL_ACt_Ch<- YLL_AC;YLL_ACt_ChS<- YLL_AC; YLL_ACt_I<- YLL_AC
  
  
  for(i in 1:iterdis){
    for(k in 1:36){
      YLL_AC1_Ac[i,k]<- sum(YLL_Ac[1:3,i,k]) #aged 0 to 5
      YLL_AC1_As[i,k]<- sum(YLL_As[1:3,i,k])
      YLL_AC1_Ch[i,k]<- sum(YLL_Ch[1:3,i,k])
      YLL_AC1_ChS[i,k]<- sum(YLL_ChS[1:3,i,k])
      YLL_AC1_I[i,k]<- sum(YLL_I[1:3,i,k])
      
      YLL_AC2_Ac[i,k]<- sum(YLL_Ac[4:6,i,k]) #aged 6 to 20
      YLL_AC2_As[i,k]<- sum(YLL_As[4:6,i,k])
      YLL_AC2_Ch[i,k]<- sum(YLL_Ch[4:6,i,k])
      YLL_AC2_ChS[i,k]<- sum(YLL_ChS[4:6,i,k])
      YLL_AC2_I[i,k]<- sum(YLL_I[4:6,i,k])
      
      YLL_AC3_Ac[i,k]<- sum(YLL_Ac[7:10,i,k])#aged 21 to 40
      YLL_AC3_As[i,k]<- sum(YLL_As[7:10,i,k])
      YLL_AC3_Ch[i,k]<- sum(YLL_Ch[7:10,i,k])
      YLL_AC3_ChS[i,k]<- sum(YLL_ChS[7:10,i,k])
      YLL_AC3_I[i,k]<- sum(YLL_I[7:10,i,k])
      
      YLL_AC4_Ac[i,k]<- sum(YLL_Ac[11:14,i,k])#aged 41 to 60
      YLL_AC4_As[i,k]<- sum(YLL_As[11:14,i,k])
      YLL_AC4_Ch[i,k]<- sum(YLL_Ch[11:14,i,k])
      YLL_AC4_ChS[i,k]<- sum(YLL_ChS[11:14,i,k])
      YLL_AC4_I[i,k]<- sum(YLL_I[11:14,i,k])
      
      YLL_AC5_Ac[i,k]<- sum(YLL_Ac[15:23,i,k])#aged > 60
      YLL_AC5_As[i,k]<- sum(YLL_As[15:23,i,k])
      YLL_AC5_Ch[i,k]<- sum(YLL_Ch[15:23,i,k])
      YLL_AC5_ChS[i,k]<- sum(YLL_ChS[15:23,i,k])
      YLL_AC5_I[i,k]<- sum(YLL_I[15:23,i,k])
      
      YLL_ACt_Ac[i,k]<- sum(YLL_Ac[,i,k])# all ages
      YLL_ACt_As[i,k]<- sum(YLL_As[,i,k])
      YLL_ACt_Ch[i,k]<- sum(YLL_Ch[,i,k])
      YLL_ACt_ChS[i,k]<- sum(YLL_ChS[,i,k])
      YLL_ACt_I[i,k]<- sum(YLL_I[,i,k])
    }
  }
  
  for (k in 1:36){
    YLL_AC1_StAc[place_name,,k] <- suma(YLL_AC1_Ac[,k])
    YLL_AC1_StAs [place_name,,k] <- suma(YLL_AC1_As[,k])
    YLL_AC1_StCh [place_name,,k] <- suma(YLL_AC1_Ch[,k])
    YLL_AC1_StChS [place_name,,k] <- suma(YLL_AC1_ChS[,k])
    YLL_AC1_StI [place_name,,k] <- suma(YLL_AC1_I[,k])
    
    YLL_AC2_StAc [place_name,,k] <- suma(YLL_AC2_Ac[,k])
    YLL_AC2_StAs [place_name,,k] <- suma(YLL_AC2_As[,k])
    YLL_AC2_StCh[place_name,,k] <- suma(YLL_AC2_Ch[,k])
    YLL_AC2_StChS [place_name,,k] <- suma(YLL_AC2_ChS[,k])
    YLL_AC2_StI [place_name,,k] <- suma(YLL_AC2_I[,k])
    
    YLL_AC3_StAc [place_name,,k] <- suma(YLL_AC3_Ac[,k])
    YLL_AC3_StAs [place_name,,k] <- suma(YLL_AC3_As[,k])
    YLL_AC3_StCh [place_name,,k] <- suma(YLL_AC3_Ch[,k])
    YLL_AC3_StChS [place_name,,k] <- suma(YLL_AC3_ChS[,k])
    YLL_AC3_StI [place_name,,k] <- suma(YLL_AC3_I[,k])
    
    YLL_AC4_StAc [place_name,,k] <- suma(YLL_AC4_Ac[,k])
    YLL_AC4_StAs [place_name,,k] <- suma(YLL_AC4_As[,k])
    YLL_AC4_StCh [place_name,,k] <- suma(YLL_AC4_Ch[,k])
    YLL_AC4_StChS [place_name,,k] <- suma(YLL_AC4_ChS[,k])
    YLL_AC4_StI [place_name,,k] <- suma(YLL_AC4_I[,k])
    
    YLL_AC5_StAc [place_name,,k] <- suma(YLL_AC5_Ac[,k])
    YLL_AC5_StAs [place_name,,k] <- suma(YLL_AC5_As[,k])
    YLL_AC5_StCh [place_name,,k] <- suma(YLL_AC5_Ch[,k])
    YLL_AC5_StChS [place_name,,k] <- suma(YLL_AC5_ChS[,k])
    YLL_AC5_StI [place_name,,k] <- suma(YLL_AC5_I[,k])
    
    YLL_ACt_StAc [place_name,,k] <- suma(YLL_ACt_Ac[,k])
    YLL_ACt_StAs [place_name,,k] <- suma(YLL_ACt_As[,k])
    YLL_ACt_StCh [place_name,,k] <- suma(YLL_ACt_Ch[,k])
    YLL_ACt_StChS [place_name,,k] <- suma(YLL_ACt_ChS[,k])
    YLL_ACt_StI [place_name,,k] <- suma(YLL_ACt_I[,k])
    
  }
  
  #================================== 
  #                YLD 
  #==================================
  
YLD<- array(NA, dim=c(Nb_AgeClass,iterdis,36))
  YLD_Ac<-YLD; YLD_Ch<-YLD;YLD_ChS<-YLD;YLD_I<-YLD;
 
   for (k in 1:36){  
    for (i in 1:iterdis){  
      for (j in 1: Nb_AgeClass){
        YLD_Ac[j,i,k] <- (BU_Case_Am[j,i,k] * params$wamD[j]) + (BU_Case_As[j,i,k] * params$wasD[j])
        YLD_Ch[j,i,k] <- (BU_Case_Cm[j,i,k]* params$wcmD[j]) + (Case_ChS[j,i,k]* params$wcsD[j] )
        YLD_ChS[j,i,k] <- Case_ChS[j,i,k]* params$wcsD[j]
        YLD_I[j,i,k] <-  YLD_Ac[j,i,k] + YLD_Ch[j,i,k]
      }}}
  
  YLD_AC<- matrix(NA, iterdis,36)
  YLD_AC1_Ac<- YLD_AC;  YLD_AC1_Ch<- YLD_AC;YLD_AC1_ChS<- YLD_AC; YLD_AC1_I<- YLD_AC
  YLD_AC2_Ac<- YLD_AC;  YLD_AC2_Ch<- YLD_AC;YLD_AC2_ChS<- YLD_AC; YLD_AC2_I<- YLD_AC
  YLD_AC3_Ac<- YLD_AC;  YLD_AC3_Ch<- YLD_AC;YLD_AC3_ChS<- YLD_AC; YLD_AC3_I<- YLD_AC
  YLD_AC4_Ac<- YLD_AC;  YLD_AC4_Ch<- YLD_AC;YLD_AC4_ChS<- YLD_AC; YLD_AC4_I<- YLD_AC
  YLD_AC5_Ac<- YLD_AC;  YLD_AC5_Ch<- YLD_AC;YLD_AC5_ChS<- YLD_AC; YLD_AC5_I<- YLD_AC
  YLD_ACt_Ac<- YLD_AC;  YLD_ACt_Ch<- YLD_AC;YLD_ACt_ChS<- YLD_AC; YLD_ACt_I<- YLD_AC
  
  
  for(i in 1:iterdis){
    for(k in 1:36){
      YLD_AC1_Ac[i,k]<- sum(YLD_Ac[1:6,i,k]) #aged 0 to 5
      YLD_AC1_Ch[i,k]<- sum(YLD_Ch[1:6,i,k])
      YLD_AC1_ChS[i,k]<- sum(YLD_ChS[1:6,i,k])
      YLD_AC1_I[i,k]<- sum(YLD_I[1:6,i,k])
      
      YLD_AC2_Ac[i,k]<- sum(YLD_Ac[7:21,i,k]) #aged 6 to 20
      YLD_AC2_Ch[i,k]<- sum(YLD_Ch[7:21,i,k])
      YLD_AC2_ChS[i,k]<- sum(YLD_ChS[7:21,i,k])
      YLD_AC2_I[i,k]<- sum(YLD_I[7:21,i,k])
      
      YLD_AC3_Ac[i,k]<- sum(YLD_Ac[22:41,i,k])#aged 21 to 40
      YLD_AC3_Ch[i,k]<- sum(YLD_Ch[22:41,i,k])
      YLD_AC3_ChS[i,k]<- sum(YLD_ChS[22:41,i,k])
      YLD_AC3_I[i,k]<- sum(YLD_I[22:41,i,k])
      
      YLD_AC4_Ac[i,k]<- sum(YLD_Ac[42:61,i,k])#aged 41 to 60
      YLD_AC4_Ch[i,k]<- sum(YLD_Ch[42:61,i,k])
      YLD_AC4_ChS[i,k]<- sum(YLD_ChS[42:61,i,k])
      YLD_AC4_I[i,k]<- sum(YLD_I[42:61,i,k])
      
      YLD_AC5_Ac[i,k]<- sum(YLD_Ac[62:101,i,k], na.rm=T)#aged > 60
      YLD_AC5_Ch[i,k]<- sum(YLD_Ch[62:101,i,k], na.rm=T)
      YLD_AC5_ChS[i,k]<- sum(YLD_ChS[62:101,i,k], na.rm=T)
      YLD_AC5_I[i,k]<- sum(YLD_I[62:101,i,k], na.rm=T)
      
      YLD_ACt_Ac[i,k]<- sum(YLD_Ac[,i,k], na.rm=T)# all ages
      YLD_ACt_Ch[i,k]<- sum(YLD_Ch[,i,k], na.rm=T)
      YLD_ACt_ChS[i,k]<- sum(YLD_ChS[,i,k], na.rm=T)
      YLD_ACt_I[i,k]<- sum(YLD_I[,i,k], na.rm=T)
    }
  }
  
  for (k in 1:36){
    YLD_AC1_StAc[place_name,,k] <- suma(YLD_AC1_Ac[,k])
    YLD_AC1_StCh [place_name,,k] <- suma(YLD_AC1_Ch[,k])
    YLD_AC1_StChS [place_name,,k] <- suma(YLD_AC1_ChS[,k])
    YLD_AC1_StI [place_name,,k] <- suma(YLD_AC1_I[,k])
    
    YLD_AC2_StAc [place_name,,k] <- suma(YLD_AC2_Ac[,k])
    YLD_AC2_StCh[place_name,,k] <- suma(YLD_AC2_Ch[,k])
    YLD_AC2_StChS [place_name,,k] <- suma(YLD_AC2_ChS[,k])
    YLD_AC2_StI [place_name,,k] <- suma(YLD_AC2_I[,k])
    
    YLD_AC3_StAc [place_name,,k] <- suma(YLD_AC3_Ac[,k])
    YLD_AC3_StCh [place_name,,k] <- suma(YLD_AC3_Ch[,k])
    YLD_AC3_StChS [place_name,,k] <- suma(YLD_AC3_ChS[,k])
    YLD_AC3_StI [place_name,,k] <- suma(YLD_AC3_I[,k])
    
    YLD_AC4_StAc [place_name,,k] <- suma(YLD_AC4_Ac[,k])
    YLD_AC4_StCh [place_name,,k] <- suma(YLD_AC4_Ch[,k])
    YLD_AC4_StChS [place_name,,k] <- suma(YLD_AC4_ChS[,k])
    YLD_AC4_StI [place_name,,k] <- suma(YLD_AC4_I[,k])
    
    YLD_AC5_StAc [place_name,,k] <- suma(YLD_AC5_Ac[,k])
    YLD_AC5_StCh [place_name,,k] <- suma(YLD_AC5_Ch[,k])
    YLD_AC5_StChS [place_name,,k] <- suma(YLD_AC5_ChS[,k])
    YLD_AC5_StI [place_name,,k] <- suma(YLD_AC5_I[,k])
    
    YLD_ACt_StAc [place_name,,k] <- suma(YLD_ACt_Ac[,k])
    YLD_ACt_StCh [place_name,,k] <- suma(YLD_ACt_Ch[,k])
    YLD_ACt_StChS [place_name,,k] <- suma(YLD_ACt_ChS[,k])
    YLD_ACt_StI [place_name,,k] <- suma(YLD_ACt_I[,k])
    
  }
  
  #================================== 
  #                DALYs
  #==================================
  
  DALY_AC1_Ac<- YLL_AC1_Ac + YLD_AC1_Ac ;  
  DALY_AC1_As<- YLL_AC1_As;   
  DALY_AC1_Ch<- YLL_AC1_Ch + YLD_AC1_Ch;  
  DALY_AC1_ChS<- YLL_AC1_ChS + YLD_AC1_ChS; 
  DALY_AC1_I<- YLL_AC1_I + YLD_AC1_I
  
  DALY_AC2_Ac<- YLL_AC2_Ac + YLD_AC2_Ac ;  
  DALY_AC2_As<- YLL_AC2_As;   
  DALY_AC2_Ch<- YLL_AC2_Ch + YLD_AC2_Ch;  
  DALY_AC2_ChS<- YLL_AC2_ChS + YLD_AC2_ChS; 
  DALY_AC2_I<- YLL_AC2_I + YLD_AC2_I
  
  DALY_AC3_Ac<- YLL_AC3_Ac + YLD_AC3_Ac ;  
  DALY_AC3_As<- YLL_AC3_As;   
  DALY_AC3_Ch<- YLL_AC3_Ch + YLD_AC3_Ch;  
  DALY_AC3_ChS<- YLL_AC3_ChS + YLD_AC3_ChS; 
  DALY_AC3_I<- YLL_AC3_I + YLD_AC3_I
  
  DALY_AC4_Ac<- YLL_AC4_Ac + YLD_AC4_Ac ;  
  DALY_AC4_As<- YLL_AC4_As;   
  DALY_AC4_Ch<- YLL_AC4_Ch + YLD_AC4_Ch;  
  DALY_AC4_ChS<- YLL_AC4_ChS + YLD_AC4_ChS; 
  DALY_AC4_I<- YLL_AC4_I + YLD_AC4_I
  
  DALY_AC5_Ac<- YLL_AC5_Ac + YLD_AC5_Ac ;  
  DALY_AC5_As<- YLL_AC5_As;   
  DALY_AC5_Ch<- YLL_AC5_Ch + YLD_AC5_Ch;  
  DALY_AC5_ChS<- YLL_AC5_ChS + YLD_AC5_ChS; 
  DALY_AC5_I<- YLL_AC5_I + YLD_AC5_I
  
  DALY_ACt_Ac<- YLL_ACt_Ac + YLD_ACt_Ac ;  
  DALY_ACt_As<- YLL_ACt_As;   
  DALY_ACt_Ch<- YLL_ACt_Ch + YLD_ACt_Ch;  
  DALY_ACt_ChS<- YLL_ACt_ChS + YLD_ACt_ChS; 
  DALY_ACt_I<- YLL_ACt_I + YLD_ACt_I
  
  for (k in 1:36){
    DALYs_AC1_StAc[place_name,,k] <- suma(DALY_AC1_Ac[,k])
    DALYs_AC1_StAs [place_name,,k] <- suma(DALY_AC1_As[,k])
    DALYs_AC1_StCh [place_name,,k] <- suma(DALY_AC1_Ch[,k])
    DALYs_AC1_StChS [place_name,,k] <- suma(DALY_AC1_ChS[,k])
    DALYs_AC1_StI [place_name,,k] <- suma(DALY_AC1_I[,k])
    
    DALYs_AC2_StAc [place_name,,k] <- suma(DALY_AC2_Ac[,k])
    DALYs_AC2_StAs [place_name,,k] <- suma(DALY_AC2_As[,k])
    DALYs_AC2_StCh[place_name,,k] <- suma(DALY_AC2_Ch[,k])
    DALYs_AC2_StChS [place_name,,k] <- suma(DALY_AC2_ChS[,k])
    DALYs_AC2_StI [place_name,,k] <- suma(DALY_AC2_I[,k])
    
    DALYs_AC3_StAc [place_name,,k] <- suma(DALY_AC3_Ac[,k])
    DALYs_AC3_StAs [place_name,,k] <- suma(DALY_AC3_As[,k])
    DALYs_AC3_StCh [place_name,,k] <- suma(DALY_AC3_Ch[,k])
    DALYs_AC3_StChS [place_name,,k] <- suma(DALY_AC3_ChS[,k])
    DALYs_AC3_StI [place_name,,k] <- suma(DALY_AC3_I[,k])
    
    DALYs_AC4_StAc [place_name,,k] <- suma(DALY_AC4_Ac[,k])
    DALYs_AC4_StAs [place_name,,k] <- suma(DALY_AC4_As[,k])
    DALYs_AC4_StCh [place_name,,k] <- suma(DALY_AC4_Ch[,k])
    DALYs_AC4_StChS [place_name,,k] <- suma(DALY_AC4_ChS[,k])
    DALYs_AC4_StI [place_name,,k] <- suma(DALY_AC4_I[,k])
    
    DALYs_AC5_StAc [place_name,,k] <- suma(DALY_AC5_Ac[,k])
    DALYs_AC5_StAs [place_name,,k] <- suma(DALY_AC5_As[,k])
    DALYs_AC5_StCh [place_name,,k] <- suma(DALY_AC5_Ch[,k])
    DALYs_AC5_StChS [place_name,,k] <- suma(DALY_AC5_ChS[,k])
    DALYs_AC5_StI [place_name,,k] <- suma(DALY_AC5_I[,k])
    
    DALYs_ACt_StAc [place_name,,k] <- suma(DALY_ACt_Ac[,k])
    DALYs_ACt_StAs [place_name,,k] <- suma(DALY_ACt_As[,k])
    DALYs_ACt_StCh [place_name,,k] <- suma(DALY_ACt_Ch[,k])
    DALYs_ACt_StChS [place_name,,k] <- suma(DALY_ACt_ChS[,k])
    DALYs_ACt_StI [place_name,,k] <- suma(DALY_ACt_I[,k])
    
  }
  
  }

#cases
saveRDS(NbCases_AC1_StAc, paste("ML_inputs/res/NbCases_AC1_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC1_StAs, paste("ML_inputs/res/NbCases_AC1_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC1_StCh, paste("ML_inputs/res/NbCases_AC1_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC1_StChS, paste("ML_inputs/res/NbCases_AC1_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC1_StI, paste("ML_inputs/res/NbCases_AC1_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbCases_AC2_StAc, paste("ML_inputs/res/NbCases_AC2_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC2_StAs, paste("ML_inputs/res/NbCases_AC2_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC2_StCh, paste("ML_inputs/res/NbCases_AC2_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC2_StChS, paste("ML_inputs/res/NbCases_AC2_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC2_StI, paste("ML_inputs/res/NbCases_AC2_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbCases_AC3_StAc, paste("ML_inputs/res/NbCases_AC3_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC3_StAs, paste("ML_inputs/res/NbCases_AC3_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC3_StCh, paste("ML_inputs/res/NbCases_AC3_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC3_StChS, paste("ML_inputs/res/NbCases_AC3_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC3_StI, paste("ML_inputs/res/NbCases_AC3_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbCases_AC4_StAc, paste("ML_inputs/res/NbCases_AC4_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC4_StAs, paste("ML_inputs/res/NbCases_AC4_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC4_StCh, paste("ML_inputs/res/NbCases_AC4_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC4_StChS, paste("ML_inputs/res/NbCases_AC4_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC4_StI, paste("ML_inputs/res/NbCases_AC4_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbCases_AC5_StAc, paste("ML_inputs/res/NbCases_AC5_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC5_StAs, paste("ML_inputs/res/NbCases_AC5_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC5_StCh, paste("ML_inputs/res/NbCases_AC5_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC5_StChS, paste("ML_inputs/res/NbCases_AC5_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_AC5_StI, paste("ML_inputs/res/NbCases_AC5_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbCases_ACt_StAc, paste("ML_inputs/res/NbCases_ACt_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_ACt_StAs, paste("ML_inputs/res/NbCases_ACt_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_ACt_StCh, paste("ML_inputs/res/NbCases_ACt_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_ACt_StChS, paste("ML_inputs/res/NbCases_ACt_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbCases_ACt_StI, paste("ML_inputs/res/NbCases_ACt_StI", setting, Nb_iter, "it",sep="_"))

#deaths
saveRDS(NbDeath_AC1_StAc, paste("ML_inputs/res/NbDeath_AC1_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC1_StAs, paste("ML_inputs/res/NbDeath_AC1_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC1_StCh, paste("ML_inputs/res/NbDeath_AC1_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC1_StChS, paste("ML_inputs/res/NbDeath_AC1_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC1_StI, paste("ML_inputs/res/NbDeath_AC1_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbDeath_AC2_StAc, paste("ML_inputs/res/NbDeath_AC2_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC2_StAs, paste("ML_inputs/res/NbDeath_AC2_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC2_StCh, paste("ML_inputs/res/NbDeath_AC2_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC2_StChS, paste("ML_inputs/res/NbDeath_AC2_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC2_StI, paste("ML_inputs/res/NbDeath_AC2_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbDeath_AC3_StAc, paste("ML_inputs/res/NbDeath_AC3_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC3_StAs, paste("ML_inputs/res/NbDeath_AC3_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC3_StCh, paste("ML_inputs/res/NbDeath_AC3_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC3_StChS, paste("ML_inputs/res/NbDeath_AC3_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC3_StI, paste("ML_inputs/res/NbDeath_AC3_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbDeath_AC4_StAc, paste("ML_inputs/res/NbDeath_AC4_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC4_StAs, paste("ML_inputs/res/NbDeath_AC4_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC4_StCh, paste("ML_inputs/res/NbDeath_AC4_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC4_StChS, paste("ML_inputs/res/NbDeath_AC4_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC4_StI, paste("ML_inputs/res/NbDeath_AC4_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbDeath_AC5_StAc, paste("ML_inputs/res/NbDeath_AC5_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC5_StAs, paste("ML_inputs/res/NbDeath_AC5_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC5_StCh, paste("ML_inputs/res/NbDeath_AC5_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC5_StChS, paste("ML_inputs/res/NbDeath_AC5_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_AC5_StI, paste("ML_inputs/res/NbDeath_AC5_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(NbDeath_ACt_StAc, paste("ML_inputs/res/NbDeath_ACt_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_ACt_StAs, paste("ML_inputs/res/NbDeath_ACt_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_ACt_StCh, paste("ML_inputs/res/NbDeath_ACt_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_ACt_StChS, paste("ML_inputs/res/NbDeath_ACt_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(NbDeath_ACt_StI, paste("ML_inputs/res/NbDeath_ACt_StI", setting, Nb_iter, "it",sep="_"))


#YLL
saveRDS(YLL_AC1_StAc, paste("ML_inputs/res/YLL_AC1_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC1_StAs, paste("ML_inputs/res/YLL_AC1_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC1_StCh, paste("ML_inputs/res/YLL_AC1_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC1_StChS, paste("ML_inputs/res/YLL_AC1_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC1_StI, paste("ML_inputs/res/YLL_AC1_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLL_AC2_StAc, paste("ML_inputs/res/YLL_AC2_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC2_StAs, paste("ML_inputs/res/YLL_AC2_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC2_StCh, paste("ML_inputs/res/YLL_AC2_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC2_StChS, paste("ML_inputs/res/YLL_AC2_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC2_StI, paste("ML_inputs/res/YLL_AC2_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLL_AC3_StAc, paste("ML_inputs/res/YLL_AC3_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC3_StAs, paste("ML_inputs/res/YLL_AC3_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC3_StCh, paste("ML_inputs/res/YLL_AC3_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC3_StChS, paste("ML_inputs/res/YLL_AC3_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC3_StI, paste("ML_inputs/res/YLL_AC3_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLL_AC4_StAc, paste("ML_inputs/res/YLL_AC4_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC4_StAs, paste("ML_inputs/res/YLL_AC4_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC4_StCh, paste("ML_inputs/res/YLL_AC4_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC4_StChS, paste("ML_inputs/res/YLL_AC4_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC4_StI, paste("ML_inputs/res/YLL_AC4_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLL_AC5_StAc, paste("ML_inputs/res/YLL_AC5_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC5_StAs, paste("ML_inputs/res/YLL_AC5_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC5_StCh, paste("ML_inputs/res/YLL_AC5_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC5_StChS, paste("ML_inputs/res/YLL_AC5_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_AC5_StI, paste("ML_inputs/res/YLL_AC5_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLL_ACt_StAc, paste("ML_inputs/res/YLL_ACt_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_ACt_StAs, paste("ML_inputs/res/YLL_ACt_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_ACt_StCh, paste("ML_inputs/res/YLL_ACt_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_ACt_StChS, paste("ML_inputs/res/YLL_ACt_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLL_ACt_StI, paste("ML_inputs/res/YLL_ACt_StI", setting, Nb_iter, "it",sep="_"))

#YLD
saveRDS(YLD_AC1_StAc, paste("ML_inputs/res/YLD_AC1_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC1_StCh, paste("ML_inputs/res/YLD_AC1_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC1_StChS, paste("ML_inputs/res/YLD_AC1_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC1_StI, paste("ML_inputs/res/YLD_AC1_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLD_AC2_StAc, paste("ML_inputs/res/YLD_AC2_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC2_StCh, paste("ML_inputs/res/YLD_AC2_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC2_StChS, paste("ML_inputs/res/YLD_AC2_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC2_StI, paste("ML_inputs/res/YLD_AC2_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLD_AC3_StAc, paste("ML_inputs/res/YLD_AC3_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC3_StCh, paste("ML_inputs/res/YLD_AC3_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC3_StChS, paste("ML_inputs/res/YLD_AC3_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC3_StI, paste("ML_inputs/res/YLD_AC3_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLD_AC4_StAc, paste("ML_inputs/res/YLD_AC4_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC4_StCh, paste("ML_inputs/res/YLD_AC4_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC4_StChS, paste("ML_inputs/res/YLD_AC4_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC4_StI, paste("ML_inputs/res/YLD_AC4_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLD_AC5_StAc, paste("ML_inputs/res/YLD_AC5_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC5_StCh, paste("ML_inputs/res/YLD_AC5_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC5_StChS, paste("ML_inputs/res/YLD_AC5_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_AC5_StI, paste("ML_inputs/res/YLD_AC5_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(YLD_ACt_StAc, paste("ML_inputs/res/YLD_ACt_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_ACt_StCh, paste("ML_inputs/res/YLD_ACt_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_ACt_StChS, paste("ML_inputs/res/YLD_ACt_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(YLD_ACt_StI, paste("ML_inputs/res/YLD_ACt_StI", setting, Nb_iter, "it",sep="_"))


#DALYs
saveRDS(DALYs_AC1_StAc, paste("ML_inputs/res/DALYs_AC1_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC1_StAs, paste("ML_inputs/res/DALYs_AC1_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC1_StCh, paste("ML_inputs/res/DALYs_AC1_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC1_StChS, paste("ML_inputs/res/DALYs_AC1_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC1_StI, paste("ML_inputs/res/DALYs_AC1_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(DALYs_AC2_StAc, paste("ML_inputs/res/DALYs_AC2_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC2_StAs, paste("ML_inputs/res/DALYs_AC2_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC2_StCh, paste("ML_inputs/res/DALYs_AC2_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC2_StChS, paste("ML_inputs/res/DALYs_AC2_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC2_StI, paste("ML_inputs/res/DALYs_AC2_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(DALYs_AC3_StAc, paste("ML_inputs/res/DALYs_AC3_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC3_StAs, paste("ML_inputs/res/DALYs_AC3_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC3_StCh, paste("ML_inputs/res/DALYs_AC3_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC3_StChS, paste("ML_inputs/res/DALYs_AC3_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC3_StI, paste("ML_inputs/res/DALYs_AC3_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(DALYs_AC4_StAc, paste("ML_inputs/res/DALYs_AC4_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC4_StAs, paste("ML_inputs/res/DALYs_AC4_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC4_StCh, paste("ML_inputs/res/DALYs_AC4_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC4_StChS, paste("ML_inputs/res/DALYs_AC4_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC4_StI, paste("ML_inputs/res/DALYs_AC4_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(DALYs_AC5_StAc, paste("ML_inputs/res/DALYs_AC5_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC5_StAs, paste("ML_inputs/res/DALYs_AC5_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC5_StCh, paste("ML_inputs/res/DALYs_AC5_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC5_StChS, paste("ML_inputs/res/DALYs_AC5_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_AC5_StI, paste("ML_inputs/res/DALYs_AC5_StI", setting, Nb_iter, "it",sep="_"))

saveRDS(DALYs_ACt_StAc, paste("ML_inputs/res/DALYs_ACt_StAc", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_ACt_StAs, paste("ML_inputs/res/DALYs_ACt_StAs", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_ACt_StCh, paste("ML_inputs/res/DALYs_ACt_StCh", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_ACt_StChS, paste("ML_inputs/res/DALYs_ACt_StChS", setting, Nb_iter, "it",sep="_"))
saveRDS(DALYs_ACt_StI, paste("ML_inputs/res/DALYs_ACt_StI", setting, Nb_iter, "it",sep="_"))

