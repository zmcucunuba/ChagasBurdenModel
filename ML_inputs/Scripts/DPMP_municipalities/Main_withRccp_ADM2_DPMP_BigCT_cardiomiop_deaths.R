#####################################################
#====================================================
#             MODEL CHAGAS BURDEN WITH ML INPUTS              
#====================================================
#####################################################

# 08/11/2021
# Julia Ledien, Zulma Cucunuba
# FoI inputs model : ML_A3_RFlog

rm(list=ls(all.names=TRUE))
library(xtable)
library(Hmisc)
require(ggplot2)
library(reshape) 
require(grid)
library(Rcpp)
library(readxl)

source('ML_inputs/Scripts/DPMP_municipalities/ADM2_FormatingData_DPMP.R')
source('ML_inputs/Scripts/ADM2_GetParameterDistributions.R')
sourceCpp('ML_inputs/Scripts/Rcpp_BurdenModel2.cpp')
source ("ML_inputs/Scripts/FUN.R")

#======== To be defined ==============
setting <- "rural"
setting2<- "rural"
Nb_iter=100
years<- 1985:2020
# ====================================


dico<- read.csv("ML_inputs/Data/dem/dictionnaire_ADM2_Names_COL")
municipalities<- unique(dico$HUMDATA_ADM2_PCODE[1:1122])

iterdis<-Nb_iter

# generation of outputs files

National_template<- matrix(0,iterdis,length(years))
c.names<-1:iterdis
r.names<- unique(dico$DPNOM)
m.names<- years
Dep_template<- array(0,dim=c(length(unique(dico$DPNOM)),iterdis,length(years)),dimnames = list(r.names, c.names, m.names))

###
National_NbDeath_AC1_Cmild <- National_template
National_NbDeath_AC1_Csev <- National_template
National_NbDeath_AC1_H <- National_template

National_NbDeath_AC2_Cmild <- National_template
National_NbDeath_AC2_Csev <- National_template
National_NbDeath_AC2_H <- National_template

National_NbDeath_AC3_Cmild <- National_template
National_NbDeath_AC3_Csev <- National_template
National_NbDeath_AC3_H <- National_template

National_NbDeath_AC4_Cmild <- National_template
National_NbDeath_AC4_Csev <- National_template
National_NbDeath_AC4_H <- National_template

National_NbDeath_AC5_Cmild <- National_template
National_NbDeath_AC5_Csev <- National_template
National_NbDeath_AC5_H <- National_template

National_NbDeath_ACt_Cmild <- National_template
National_NbDeath_ACt_Csev <- National_template
National_NbDeath_ACt_H <- National_template

Dep_NbDeath_AC1_Cmild <- Dep_template
Dep_NbDeath_AC1_Csev <- Dep_template
Dep_NbDeath_AC1_H <- Dep_template

Dep_NbDeath_AC2_Cmild <- Dep_template
Dep_NbDeath_AC2_Csev <- Dep_template
Dep_NbDeath_AC2_H <- Dep_template

Dep_NbDeath_AC3_Cmild <- Dep_template
Dep_NbDeath_AC3_Csev <- Dep_template
Dep_NbDeath_AC3_H <- Dep_template

Dep_NbDeath_AC4_Cmild <- Dep_template
Dep_NbDeath_AC4_Csev <- Dep_template
Dep_NbDeath_AC4_H <- Dep_template

Dep_NbDeath_AC5_Cmild <- Dep_template
Dep_NbDeath_AC5_Csev <- Dep_template
Dep_NbDeath_AC5_H <- Dep_template

Dep_NbDeath_ACt_Cmild <- Dep_template
Dep_NbDeath_ACt_Csev <- Dep_template
Dep_NbDeath_ACt_H <- Dep_template



for (place_name in municipalities){
  # place_name <- municipalities[1]
  
  lambdaNy <- ADM2_FOI_import_and_format(place_name, setting2,Nb_iter)
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
  
 
  
  
  IClo<-function(x){
    mean(x)-qnorm(.975)*(sd(x)/sqrt(iterdis))
  }
  ICup<-function(x){
    mean(x)+qnorm(.975)*(sd(x)/sqrt(iterdis))
  }
  
  suma<- function (x){
    c(min(x), quantile(x, c(0.25, 0.5, 0.75)), max(x),mean(x), sd(x),IClo(x),ICup(x)) #quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975))? and remove IClo(x),ICup(x))?
  }
  
 
  
  #========================================
  # Number of Deaths by year from 1985 to 2020 at ADM2 level
  
  TotDeaths <- (Du_Array + Da_Array + Di_Array + Dm_Array + Ds_Array + DmI_Array+ DsI_Array + DmS_Array+ DsS_Array) # Total Deaths per year 
  StAc_Deaths <-  Da_Array 
  StAs_Deaths <-  Di_Array 
  StCh_Deaths <-  Dm_Array 
  StChS_Deaths <- Ds_Array
  StI_Deaths <-  Da_Array + Di_Array +  Dm_Array + Ds_Array
  
  Cm_Deaths<- DmS_Array + DmI_Array
  Cs_Deaths<- DsI_Array + DsS_Array
  Heart_Deaths<- DsI_Array + DsS_Array + DmS_Array + DmI_Array
  
  dep<- dico$DPNOM[which(dico$HUMDATA_ADM2_PCODE==place_name)]
  
  Nb_AgeClass<- nrow(Obs.Pop)
  
  PrevD <-array(NA, dim=c(Nb_AgeClass, iterdis, 160))
  PrevD_H<- PrevD;  PrevD_Cmild<- PrevD; PrevD_Csev<- PrevD;
  
  for (l in 1:160){
    for(n in 1:iterdis){
      for(k in 1:85){
        
        PrevD_H[k,n,l]<-  Heart_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_Cmild[k,n,l]<-  Cm_Deaths[k,n,l]/TotDeaths[k,n,l]
        PrevD_Csev[k,n,l]<-  Cs_Deaths[k,n,l]/TotDeaths[k,n,l]
      }
      for (k in 85: Nb_AgeClass){
        PrevD_H[k,n,l]<-  Heart_Deaths[85,n,l]/TotDeaths[85,n,l] #extending data to all age classes
        PrevD_Cmild[k,n,l]<-  Cm_Deaths[85,n,l]/TotDeaths[85,n,l]
        PrevD_Csev[k,n,l]<-  Cs_Deaths[85,n,l]/TotDeaths[85,n,l]
        }}}
  
  
  PrevD_H_23AC<- AC101to23(PrevD_H)
  PrevD_Cmild_23AC<- AC101to23(PrevD_Cmild)
  PrevD_Csev_23AC<- AC101to23(PrevD_Csev)
  
  PrevD_H_23AC.36y<-PrevD_H_23AC[,,(115:150)] # from 1985 to 2020
  PrevD_Cmild_23AC.36y<-PrevD_Cmild_23AC[,,(115:150)] 
  PrevD_Csev_23AC.36y<-PrevD_Csev_23AC[,,(115:150)] 
  
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
  Death_H<- Death;  Death_Cmild<- Death; Death_Csev<- Death
  
  for (i in 1:iterdis){
    for (j in 1:23){
      for (k in 1:36){
        Death_H[j,i,k]<- (PrevD_H_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_Cmild[j,i,k]<- (PrevD_Cmild_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
        Death_Csev[j,i,k]<- (PrevD_Csev_23AC.36y[j,i,k] * Obs.Deaths[j,k] )
         }}}
  
  Deaths_AC<- matrix(NA, iterdis,36)
  Deaths_AC1_H<- Deaths_AC; Deaths_AC1_Cmild<- Deaths_AC; Deaths_AC1_Csev<- Deaths_AC
  Deaths_AC2_H<- Deaths_AC; Deaths_AC2_Cmild<- Deaths_AC; Deaths_AC2_Csev<- Deaths_AC
  Deaths_AC3_H<- Deaths_AC; Deaths_AC3_Cmild<- Deaths_AC; Deaths_AC3_Csev<- Deaths_AC
  Deaths_AC4_H<- Deaths_AC; Deaths_AC4_Cmild<- Deaths_AC; Deaths_AC4_Csev<- Deaths_AC
  Deaths_AC5_H<- Deaths_AC; Deaths_AC5_Cmild<- Deaths_AC; Deaths_AC5_Csev<- Deaths_AC
  Deaths_ACt_H<- Deaths_AC; Deaths_ACt_Cmild<- Deaths_AC; Deaths_ACt_Csev<- Deaths_AC
  
  
  for(i in 1:iterdis){
    for(k in 1:36){
      Deaths_AC1_Cmild[i,k]<- sum(Death_Cmild[1:3,i,k])
      Deaths_AC1_Csev[i,k]<- sum(Death_Csev[1:3,i,k])
      Deaths_AC1_H[i,k]<- sum(Death_H[1:3,i,k])
      
      Deaths_AC2_Cmild[i,k]<- sum(Death_Cmild[4:6,i,k])
      Deaths_AC2_Csev[i,k]<- sum(Death_Csev[4:6,i,k])
      Deaths_AC2_H[i,k]<- sum(Death_H[4:6,i,k])
      
      Deaths_AC3_Cmild[i,k]<- sum(Death_Cmild[7:10,i,k])
      Deaths_AC3_Csev[i,k]<- sum(Death_Csev[7:10,i,k])
      Deaths_AC3_H[i,k]<- sum(Death_H[7:10,i,k])
      
      Deaths_AC4_Cmild[i,k]<- sum(Death_Cmild[11:14,i,k])
      Deaths_AC4_Csev[i,k]<- sum(Death_Csev[11:14,i,k])
      Deaths_AC4_H[i,k]<- sum(Death_H[11:14,i,k])
      
      Deaths_AC5_Cmild[i,k]<- sum(Death_Cmild[15:23,i,k])
      Deaths_AC5_Csev[i,k]<- sum(Death_Csev[15:23,i,k])
      Deaths_AC5_H[i,k]<- sum(Death_H[15:23,i,k])
      
      Deaths_ACt_Cmild[i,k]<- sum(Death_Cmild[,i,k])
      Deaths_ACt_Csev[i,k]<- sum(Death_Csev[,i,k])
      Deaths_ACt_H[i,k]<- sum(Death_H[,i,k])
    }
  }
  
  # National level
  
  for (k in 1:36){
    National_NbDeath_AC1_Cmild [,k] <- National_NbDeath_AC1_Cmild [,k] + Deaths_AC1_Cmild[,k]
    National_NbDeath_AC1_Csev [,k] <- National_NbDeath_AC1_Csev [,k] + Deaths_AC1_Csev[,k]
    National_NbDeath_AC1_H [,k] <-  National_NbDeath_AC1_H [,k] + Deaths_AC1_H[,k]
    
    National_NbDeath_AC2_Cmild[,k] <- National_NbDeath_AC2_Cmild[,k] + Deaths_AC2_Cmild[,k]
    National_NbDeath_AC2_Csev[,k] <- National_NbDeath_AC2_Csev[,k] + Deaths_AC2_Csev[,k]
    National_NbDeath_AC2_H[,k] <- National_NbDeath_AC2_H[,k] + Deaths_AC2_H[,k]
    
    National_NbDeath_AC3_Cmild[,k] <- National_NbDeath_AC3_Cmild[,k] + Deaths_AC3_Cmild[,k]
    National_NbDeath_AC3_Csev[,k] <- National_NbDeath_AC3_Csev[,k] + Deaths_AC3_Csev[,k]
    National_NbDeath_AC3_H[,k] <- National_NbDeath_AC3_H[,k] + Deaths_AC3_H[,k]
    
    National_NbDeath_AC4_Cmild[,k] <- National_NbDeath_AC4_Cmild[,k] + Deaths_AC4_Cmild[,k]
    National_NbDeath_AC4_Csev[,k] <- National_NbDeath_AC4_Csev[,k] + Deaths_AC4_Csev[,k]
    National_NbDeath_AC4_H[,k] <- National_NbDeath_AC4_H[,k] + Deaths_AC4_H[,k]
    
    National_NbDeath_AC5_Cmild[,k] <- National_NbDeath_AC5_Cmild[,k] + Deaths_AC5_Cmild[,k]
    National_NbDeath_AC5_Csev[,k] <- National_NbDeath_AC5_Csev[,k] + Deaths_AC5_Csev[,k]
    National_NbDeath_AC5_H[,k] <- National_NbDeath_AC5_H[,k] + Deaths_AC5_H[,k]
    
    National_NbDeath_ACt_Cmild[,k] <- National_NbDeath_ACt_Cmild[,k] + Deaths_ACt_Cmild[,k]
    National_NbDeath_ACt_Csev[,k] <- National_NbDeath_ACt_Csev[,k] + Deaths_ACt_Csev[,k]
    National_NbDeath_ACt_H[,k] <-  National_NbDeath_ACt_H[,k] + Deaths_ACt_H[,k]
    
  }
  
  #departmental level
 
    for (k in 1:36){
      Dep_NbDeath_AC1_Cmild[dep,,k] <- Dep_NbDeath_AC1_Cmild[dep,,k] + Deaths_AC1_Cmild[,k]
      Dep_NbDeath_AC1_Csev[dep,,k] <- Dep_NbDeath_AC1_Csev[dep,,k] + Deaths_AC1_Csev[,k]
      Dep_NbDeath_AC1_H[dep,,k] <- Dep_NbDeath_AC1_H[dep,,k] + Deaths_AC1_H[,k]
      
      Dep_NbDeath_AC2_Cmild[dep,,k] <- Dep_NbDeath_AC2_Cmild[dep,,k] + Deaths_AC2_Cmild[,k]
      Dep_NbDeath_AC2_Csev[dep,,k] <- Dep_NbDeath_AC2_Csev[dep,,k] + Deaths_AC2_Csev[,k]
      Dep_NbDeath_AC2_H[dep,,k] <- Dep_NbDeath_AC2_H[dep,,k] + Deaths_AC2_H[,k]
      
      Dep_NbDeath_AC3_Cmild[dep,,k] <- Dep_NbDeath_AC3_Cmild[dep,,k] + Deaths_AC3_Cmild[,k]
      Dep_NbDeath_AC3_Csev[dep,,k] <- Dep_NbDeath_AC3_Csev[dep,,k] + Deaths_AC3_Csev[,k]
      Dep_NbDeath_AC3_H[dep,,k] <- Dep_NbDeath_AC3_H[dep,,k] + Deaths_AC3_H[,k]
      
      Dep_NbDeath_AC4_Cmild[dep,,k] <- Dep_NbDeath_AC4_Cmild[dep,,k] + Deaths_AC4_Cmild[,k]
      Dep_NbDeath_AC4_Csev[dep,,k] <- Dep_NbDeath_AC4_Csev[dep,,k] + Deaths_AC4_Csev[,k]
      Dep_NbDeath_AC4_H[dep,,k] <- Dep_NbDeath_AC4_H[dep,,k] + Deaths_AC4_H[,k]
      
      Dep_NbDeath_AC5_Cmild [dep,,k] <- Dep_NbDeath_AC5_Cmild [dep,,k] + Deaths_AC5_Cmild[,k]
      Dep_NbDeath_AC5_Csev [dep,,k] <- Dep_NbDeath_AC5_Csev [dep,,k] + Deaths_AC5_Csev[,k]
      Dep_NbDeath_AC5_H [dep,,k] <- Dep_NbDeath_AC5_H [dep,,k] + Deaths_AC5_H[,k]
      
      Dep_NbDeath_ACt_Cmild [dep,,k] <- Dep_NbDeath_ACt_Cmild [dep,,k] + Deaths_ACt_Cmild[,k]
      Dep_NbDeath_ACt_Csev [dep,,k] <- Dep_NbDeath_ACt_Csev [dep,,k] + Deaths_ACt_Csev[,k]
      Dep_NbDeath_ACt_H [dep,,k] <- Dep_NbDeath_ACt_H [dep,,k] + Deaths_ACt_H[,k]
      
    }
       
  
    
  }


#deaths
saveRDS(Dep_NbDeath_AC1_Cmild, paste("ML_inputs/res/Dep_NbDeath_AC1_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC1_Csev, paste("ML_inputs/res/Dep_NbDeath_AC1_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC1_H, paste("ML_inputs/res/Dep_NbDeath_AC1_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(Dep_NbDeath_AC2_Cmild, paste("ML_inputs/res/Dep_NbDeath_AC2_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC2_Csev, paste("ML_inputs/res/Dep_NbDeath_AC2_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC2_H, paste("ML_inputs/res/Dep_NbDeath_AC2_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(Dep_NbDeath_AC3_Cmild, paste("ML_inputs/res/Dep_NbDeath_AC3_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC3_Csev, paste("ML_inputs/res/Dep_NbDeath_AC3_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC3_H, paste("ML_inputs/res/Dep_NbDeath_AC3_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(Dep_NbDeath_AC4_Cmild, paste("ML_inputs/res/Dep_NbDeath_AC4_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC4_Csev, paste("ML_inputs/res/Dep_NbDeath_AC4_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC4_H, paste("ML_inputs/res/Dep_NbDeath_AC4_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(Dep_NbDeath_AC5_Cmild, paste("ML_inputs/res/Dep_NbDeath_AC5_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC5_Csev, paste("ML_inputs/res/Dep_NbDeath_AC5_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_AC5_H, paste("ML_inputs/res/Dep_NbDeath_AC5_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(Dep_NbDeath_ACt_Cmild, paste("ML_inputs/res/Dep_NbDeath_ACt_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_ACt_Csev, paste("ML_inputs/res/Dep_NbDeath_ACt_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(Dep_NbDeath_ACt_H, paste("ML_inputs/res/Dep_NbDeath_ACt_H", setting2, Nb_iter, "it",sep="_"))


saveRDS(National_NbDeath_AC1_Cmild, paste("ML_inputs/res/National_NbDeath_AC1_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC1_Csev, paste("ML_inputs/res/National_NbDeath_AC1_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC1_H, paste("ML_inputs/res/National_NbDeath_AC1_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(National_NbDeath_AC2_Cmild, paste("ML_inputs/res/National_NbDeath_AC2_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC2_Csev, paste("ML_inputs/res/National_NbDeath_AC2_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC2_H, paste("ML_inputs/res/National_NbDeath_AC2_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(National_NbDeath_AC3_Cmild, paste("ML_inputs/res/National_NbDeath_AC3_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC3_Csev, paste("ML_inputs/res/National_NbDeath_AC3_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC3_H, paste("ML_inputs/res/National_NbDeath_AC3_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(National_NbDeath_AC4_Cmild, paste("ML_inputs/res/National_NbDeath_AC4_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC4_Csev, paste("ML_inputs/res/National_NbDeath_AC4_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC4_H, paste("ML_inputs/res/National_NbDeath_AC4_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(National_NbDeath_AC5_Cmild, paste("ML_inputs/res/National_NbDeath_AC5_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC5_Csev, paste("ML_inputs/res/National_NbDeath_AC5_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_AC5_H, paste("ML_inputs/res/National_NbDeath_AC5_H", setting2, Nb_iter, "it",sep="_"))

saveRDS(National_NbDeath_ACt_Cmild, paste("ML_inputs/res/National_NbDeath_ACt_Cmild", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_ACt_Csev, paste("ML_inputs/res/National_NbDeath_ACt_Csev", setting2, Nb_iter, "it",sep="_"))
saveRDS(National_NbDeath_ACt_H, paste("ML_inputs/res/National_NbDeath_ACt_H", setting2, Nb_iter, "it",sep="_"))

