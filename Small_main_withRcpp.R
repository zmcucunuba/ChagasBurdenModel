#========================================================
#      Number of cases calculation
#       Municipality level
#======================================================
# Julia ledien 21/09/2020

rm(list=ls(all.names=TRUE))
library(xtable); library(Hmisc); require(ggplot2); library(reshape); require(grid);library(Rcpp); library(readxl)

source('funs/FormatingData.R')
source('funs/GetParameterDistributions.R')
#source('funs/BoD_model2.R')

sourceCpp('funs/Rcpp_BurdenModel2.cpp')

#======== To be defined ==============
setting_type <- 'total'
# ====================================

dico<- read_xlsx("data/dem/dictionnaire ADM2 Names.xlsx")
municipalities<- unique(dico$GID_2_GDAM)


for (place_name in municipalities){
  
place <- place_name


lambdaNy <- FOI_import_and_format(place_name)
params   <- get_parameter_distribution(lambdaNy, place_name, setting_type)

# params$alphaD <-params$alphaD *(1+5/100)
output <- Rcpp_BurdenModel2(params)
saveRDS(output, paste0('res/out/', place_name, "_",setting_type, '_output.RDS'))


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
DeathsObs<-Deaths_import_and_format(place)
Obs.Pop<- Pop_import_and_format(place, setting_type )

Na<- params$Na


#========================================
# Total number of Cases in 2010

TotPop = (S_Array + Sm_Array + Ss_Array + Am_Array + As_Array + I_Array + Cm_Array + Cs_Array) # Total Population per year
TotIx <-  Am_Array + As_Array + I_Array + Cm_Array + Cs_Array

Prev.Chx5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Chx5;  A5<- TotIx/TotPop
for (l in 1:160){
  for(n in 1:iterdis){
    A[1,n,l]<-  mean(A5[(1:4),n,l], na.rm=T)  
    A[2,n,l]<-  mean(A5[(5:9),n,l])
    A[3,n,l]<-  mean(A5[(10:14),n,l])
    A[4,n,l]<-  mean(A5[(15:19),n,l])
    A[5,n,l]<-  mean(A5[(20:24),n,l])
    A[6,n,l]<-  mean(A5[(25:29),n,l])
    A[7,n,l]<-  mean(A5[(30:34),n,l])
    A[8,n,l]<-  mean(A5[(35:39),n,l])
    A[9,n,l]<-  mean(A5[(40:44),n,l])
    A[10,n,l]<- mean(A5[(45:49),n,l])
    A[11,n,l]<- mean(A5[(50:54),n,l])
    A[12,n,l]<- mean(A5[(55:59),n,l])
    A[13,n,l]<- mean(A5[(60:64),n,l])
    A[14,n,l]<- mean(A5[(65:69),n,l])
    A[15,n,l]<- mean(A5[(70:74),n,l])
    A[16,n,l]<- mean(A5[(75:79),n,l])
    A[17,n,l]<- mean(A5[(80:85),n,l])
  }}#}
Prev.Chx5<-A[,,]
rm(A, A5)
Prev.Chx5.36y<- Prev.Chx5[,,(115:150)]
CasesCha <- array(NA, dim=c(17,iterdis,36)) # TOTAL number of cases

for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesCha[j,i,k]<- round(Prev.Chx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}

CasesChaCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    CasesChaCI[p,,pp]=round(quantile(CasesCha[p,,pp], c(.05,.5,.95)))
  }}

plot(c(CasesChaCI[,3,36]), type ="l", col=3,
     main=c("Total Chagas Cases", paste(place, "2020"),
            paste(sum(CasesChaCI[,2,36]), "; 95%CI:", sum(CasesChaCI[,1,36]), "-", sum(CasesChaCI[,3,36]))),
     xlab="ageclass", ylab = "Absolute Number")
lines(c(CasesChaCI[,2,36]), col=2)
lines(c(CasesChaCI[,1,36]), col=3)

}