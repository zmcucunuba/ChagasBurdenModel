#========================================================
#      Number of cases calculation
#       Municipality level
#======================================================
# Julia ledien 21/09/2020

rm(list=ls(all.names=TRUE))
library(xtable); library(Hmisc); require(ggplot2); library(reshape); require(grid);library(Rcpp); library(readxl)

source('funs/ADM2_FormatingData.R')
source('funs/ADM2_GetParameterDistributions.R')
sourceCpp('funs/Rcpp_BurdenModel2.cpp')

#======== To be defined ==============
setting_type <- "rural"
Nb_iter=100
# ====================================

dico<- read_xlsx("data/dem/dictionnaire ADM2 Names.xlsx")
municipalities<- unique(dico$GID_2_GDAM)
municipalities <- municipalities[is.na(municipalities)==F]
Med_CasesTotal<- matrix(NA, length(municipalities),36)
QuantLo_CasesTotal <- matrix (NA, length(municipalities),36)
QuantUp_CasesTotal<- matrix(NA, length(municipalities),36)
CV_CasesTotal <-  matrix(NA,length(municipalities),36)

Med_SevCasesTotal<- matrix(NA,length(municipalities), 36)
QuantLo_SevCasesTotal <- matrix(NA,length(municipalities),36)
QuantUp_SevCasesTotal<- matrix(NA,length(municipalities),36)
CV_SevCasesTotal <-  matrix(NA,length(municipalities),36)

rownames(Med_CasesTotal)<- municipalities
rownames(QuantLo_CasesTotal)<- municipalities
rownames(QuantUp_CasesTotal)<- municipalities
rownames(Med_SevCasesTotal)<- municipalities
rownames(QuantLo_SevCasesTotal)<- municipalities
rownames(QuantUp_SevCasesTotal)<-municipalities
rownames(CV_CasesTotal)<- municipalities
rownames(CV_SevCasesTotal)<- municipalities

for (place_name in municipalities){
  

lambdaNy <- ADM2_FOI_import_and_format(place_name, setting_type, Nb_iter)
params   <- ADM2_get_parameter_distribution(lambdaNy, place_name, setting_type)

# params$alphaD <-params$alphaD *(1+5/100)
output <- Rcpp_BurdenModel2(params)
#saveRDS(output, paste0('res/out/', place_name, "_",setting_type, '_output.RDS'))


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

NySim    <- as.numeric(min(names(lambdaNy))): as.numeric(max(names(lambdaNy))) # Years of Symulation
lambdaD<- lambdaNy
iterdis<-params$iter
DeathsObs<-params$Deaths
Obs.Pop<- params$Pop

Na<- params$Na


#========================================
# Total number of Cases by year from 1985 to 2020

TotPop = (S_Array + Sm_Array + Ss_Array + Am_Array + As_Array + I_Array + Cm_Array + Cs_Array) # Total Population per year
TotIx <-  Am_Array + As_Array + I_Array + Cm_Array + Cs_Array
Nb_AgeClass<- nrow(Obs.Pop)

Prev.Chx5 <-array(NA, dim=c(Nb_AgeClass, iterdis, 160))
A<- Prev.Chx5;  A5<- TotIx/TotPop
for (l in 1:160){
  for(n in 1:iterdis){
    for(k in 1:85){
      A[k,n,l]<-  A5[k,n,l]
    }
    for (k in 85: Nb_AgeClass){
      A[k,n,l]<-  A5[85,n,l]
    }}}

Prev.Chx5<-A[,,]
rm(A, A5)
Prev.Chx5.36y<- Prev.Chx5[,,(115:150)] # from 1985 to 2020
CasesCha <- array(NA, dim=c(Nb_AgeClass,iterdis,36)) # TOTAL number of cases

for (i in 1:iterdis){
  for (j in 1:Nb_AgeClass){
    for (k in 1:36){
      CasesCha[j,i,k]<- round(Prev.Chx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}

CasesTotal<- matrix(NA, iterdis,36)
for(i in 1:iterdis){
  for(k in 1:36){
    CasesTotal[i,k]<- sum(CasesCha[,i,k])
  }
}
 
Med_CasesTotal[ place_name,]<- round(apply(CasesTotal,2,median))
QuantLo_CasesTotal[place_name,]<- round(apply(CasesTotal,2,quantile,c(0.05)))
QuantUp_CasesTotal[place_name,]<-round(apply(CasesTotal,2,quantile,c(0.95)))
CV_CasesTotal[place_name,] <-   apply(CasesTotal,2,sd) / apply(CasesTotal,2,mean)


#=============================================================
#Total number of severe cases by year from 1985 to 2020
Prev.CA.Sev <-array(NA, dim=c(Nb_AgeClass, iterdis, 160))
A<- Prev.CA.Sev;  A5<-  (As_Array )/TotPop
for (l in 1:160){
  for(n in 1:iterdis){
    for(k in 1:85){
      A[k,n,l]<-  A5[k,n,l]
    }
    for (k in 85: Nb_AgeClass){
      A[k,n,l]<-  A5[85,n,l]
    }}}

Prev.CA.Sev<-A[,,]
rm(A, A5)
Prev.CA.Sev.36y<- Prev.CA.Sev[,,(115:150)] # from 1985 to 2020
CasesChaSev <- array(NA, dim=c(Nb_AgeClass,iterdis,36)) # TOTAL number of cases

for (i in 1:iterdis){
  for (j in 1:Nb_AgeClass){
    for (k in 1:36){
      CasesChaSev[j,i,k]<- round(Prev.CA.Sev.36y[j,i,k] * Obs.Pop[j,k] )
    }}}

SevCasesTotal<- matrix(NA, iterdis,36)
for(i in 1:iterdis){
  for(k in 1:36){
    SevCasesTotal[i,k]<- sum(CasesChaSev[,i,k])
    
  }
}

Med_SevCasesTotal[ place_name,]<- round(apply(SevCasesTotal,2,median))
QuantLo_SevCasesTotal[place_name,]<- round(apply(SevCasesTotal,2,quantile,c(0.05)))
QuantUp_SevCasesTotal[place_name,]<-round(apply(SevCasesTotal,2,quantile,c(0.95)))
CV_SevCasesTotal[place_name,] <-   apply(SevCasesTotal,2,sd) / apply(SevCasesTotal,2,mean)



}

saveRDS(Med_CasesTotal, paste("res/summaries/ADM2_Median_CasesTotal", setting_type, iterdis, "it",sep="_"))
saveRDS(QuantLo_CasesTotal, paste("res/summaries/ADM2_QuantLo_CasesTotal", setting_type, iterdis, "it",sep="_"))
saveRDS(QuantUp_CasesTotal, paste("res/summaries/ADM2_QuantUp_CasesTotal", setting_type, iterdis, "it",sep="_"))
saveRDS(CV_CasesTotal, paste("res/summaries/ADM2_CV_CasesTotal", setting_type, iterdis, "it",sep="_"))

saveRDS(Med_SevCasesTotal, paste("res/summaries/ADM2_Median_SevereCasesTotal", setting_type,iterdis, "it", sep="_"))
saveRDS(QuantLo_SevCasesTotal, paste("res/summaries/ADM2_QuantLo_SevereCasesTotal", setting_type,iterdis,"it", sep="_"))
saveRDS(QuantUp_SevCasesTotal, paste("res/summaries/ADM2_QuantUp_SevereCasesTotal", setting_type, iterdis, "it", sep="_"))
saveRDS(CV_SevCasesTotal, paste("res/summaries/ADM2_CV_SevereCasesTotal", setting_type, iterdis, "it", sep="_"))


############################################################
# Creating shapefiles

library(sp)
library(rgdal)

ColombiaADM2<-readRDS("C:/Users/Julia L/Documents/GitHub/chagas-ML/data/Colombia BackgroundMaps/format sp/gadm36_COL_2_sp.rds")

for (shap in c("Median_CasesTotal", "CV_CasesTotal", "Median_SevereCasesTotal", "CV_SevereCasesTotal")){
  dat<- readRDS(paste("res/summaries/ADM2",shap, setting_type, iterdis, "it", sep="_"))
  dat<- as.data.frame(dat)
  colnames(dat)<- paste0("FoI",1985:2020)
  dat[c("COL.32.4_1","COL.32.5_1"),]<- dat["COL.32.1_1",]
  dat$ADM2<- rownames(dat)
  COL_t<- ColombiaADM2
  COL_t@data<- data.frame(COL_t@data,dat[match(COL_t@data[,"GID_2"], dat[,"ADM2"]),])
  colnames(COL_t@data)
  spplot(COL_t[,"FoI1990"])
  writeOGR(COL_t, paste("res/shapefiles/geo",shap, setting_type,iterdis,"it", sep="_"), layer=paste(shap,setting_type,iterdis,"it", sep="_"), driver="ESRI Shapefile")
}