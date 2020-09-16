#####################################################
#====================================================
#             MODEL CHAGAS BURDEN version 11.0               
#====================================================
#####################################################
rm(list=ls(all.names=TRUE))
library(xtable); library(Hmisc); require(ggplot2); library(reshape); require(grid);library(Rcpp)

source('funs/FormatingData.R')
source('funs/GetParameterDistributions.R')
source('funs/BoD_model2.R')

sourceCpp('funs/Rcpp_BurdenModel2.cpp')


place_name <-  'Santander'
setting_type <- 'total'

lambdaNy <- FOI_import_and_format(place_name)
params   <- get_parameter_distribution(lambdaNy, place_name, setting_type)

# params$alphaD <-params$alphaD *(1+5/100)
output <- Rcpp_BurdenModel2(params)


output2   <- BurdenModel2(params)
# saveRDS(output, paste0('res/', place_name, '_output.RDS'))
if(all.equal(output2,output)==FALSE){
  warning('kelharjkd')
}

#output <- readRDS(paste0('../ChagasBurdenModel_res/', place_name, '_output.RDS'))
# I'm here!!!----------------------------------------



par(mar=c(2, 4, 4, 2), xpd=F)
(par(mfrow=c(2,2)))


boxplot(lambdaNy, main=c("Lambda over 160 years", place_name),col= "red",
        ylab="FOI x 1000", xlab="Simulated years", outline=FALSE)


I_Array<- output$I
Cm_Array<- output$Cm
Cs_Array<- output$Cs
Am_Array<- output$Am
As_Array<- output$As
S_Array<- output$S
Sm_Array<- output$Sm
Ss_Array<- output$Ss
###############################################################
#            Chagas Infection and Disease Prevalence          #
###############################################################

for (k in c(120, 140, 150)){
 
Year<- k

Total.Inf.YearX<- (I_Array[,,Year] + Cm_Array[,,Year] + Cs_Array[,,Year] + 
                  Am_Array[,,Year] + As_Array[,,Year])
Total.Diseased.YearX<- (Cm_Array[,,Year] + Cs_Array[,,Year] + 
                         Am_Array[,,Year] + As_Array[,,Year])
Total.Sus.YearX<- (S_Array[,,Year] + Sm_Array[,,Year] + Ss_Array[,,Year])
Total.Pop.YearX<- Total.Inf.YearX + Total.Sus.YearX

Prev.Inf.YearX<- Total.Inf.YearX/Total.Pop.YearX
Prev.Diseased.YearX<- Total.Diseased.YearX/Total.Pop.YearX

MAXP<- max(Prev.Inf.YearX)
boxplot(t(Prev.Inf.YearX), ylim=c(0,MAXP), col="green",
        main=c("Chagas Prevalence", paste(place, NySim[Year])), outline=FALSE)
par(new=T)
boxplot(t(Prev.Diseased.YearX), ylim=c(0,MAXP), col="red", xlab="age", outline=FALSE)
legend("topleft",c("Infection", "Disease"), lty=1,lwd=3, col=c("green", "red"), bty="n")
}



##############################################
#################    CARDIOMYOPATHY PREVALENCE   
##############################################
par(mar=c(6, 4, 4, 2), xpd=F)
(par(mfrow=c(1,2)))

C<- 1:17 # Children
A<- 18:85 # Adults


# Susceptibles
TotalSus=  (S_Array[,,Year] + Sm_Array[,,Year]+Ss_Array[,,Year])
PrevHDSus= (Sm_Array[,,Year]+Ss_Array[,,Year])/TotalSus
PrevSmSus= (Sm_Array[,,Year]/TotalSus)
PrevSsSus= (Ss_Array[,,Year]/TotalSus)


# Infected
TotalInf= (I_Array[,,Year] + Cm_Array[,,Year] + Cs_Array[,,Year] + Am_Array[,,Year] + As_Array[,,Year])
PrevHDInf= (Cm_Array[,,Year] + Cs_Array[,,Year] + As_Array[,,Year]) / TotalInf            
PrevCmInf= Cm_Array[,,Year]/ TotalInf
PrevCsInf= Cs_Array[,,Year]/ TotalInf

# TotalPop
PrevHDTotal= (Cm_Array[,,Year] + Cs_Array[,,Year] + As_Array[,,Year]+ 
              Sm_Array[,,Year] + Ss_Array[,,Year]) /
              (TotalSus + TotalInf)

MAXP<- max(PrevHDInf, na.rm=T)
boxplot(t(PrevHDInf), col= rgb(0.8,0.1,0.3,0.6), main=c("Heart Disease Prevalence", paste(place, NySim[Year])), boxlty = 0,  whisklty = 1, medcol = "red",whiskcol = rgb(0.8,0.1,0.3,0.6), staplecol=F,
        ylim=c(0,MAXP), outline=FALSE, xaxt="n",  ylab="Estimated prevalence" , xlab="Age")
par(new=T)
boxplot(t(PrevHDTotal), col= rgb(0.5,0.1,0.1,0.5), ylim=c(0,MAXP), outline=FALSE, xaxt="n", whisklty = 1, medcol = "green")
par(new=T)
boxplot(t(PrevHDSus), col= rgb(0.1,0.1,0.7,0.5), ylim=c(0,MAXP), outline=FALSE, xaxt="n", boxlty = 0,  whisklty = 1, medcol = "blue", whiskcol = rgb(0.1,0.1,0.7,0.5), staplecol=F)
legend("topleft", legend = c("Infected", "Susceptible", "Total") , 
       col = c(rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5), rgb(0.5,0.1,0.1,0.5) ) , bty = "n", pch=20 , 
       pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
axis(1,at=c(10, 30, 50, 70))#.5,3.5), c("Adults", "Children"))



###### TEMPORAL_PLOT
M.PrevHDTotal<- matrix(NA, 85, 5)
M.PrevHDInf  <- matrix(NA, 85, 5)
M.PrevHDSus  <- matrix(NA, 85, 5)
 for (l in 1:85){
   M.PrevHDTotal[l,]<- quantile(PrevHDTotal[l,],na.rm=T)
     M.PrevHDInf[l,]<- quantile(PrevHDInf[l,], na.rm=T)
     M.PrevHDSus[l,]<- quantile(PrevHDSus[l,],na.rm=T)
 }

par(mfrow=c(1,1))
MAXP<- max(PrevHDInf, na.rm=T)
plot((M.PrevHDInf[,3]), col= rgb(0.8,0.1,0.3,0.6), main= lambdaD[1,1], boxlty = 0,  whisklty = 1, medcol = "red",whiskcol = rgb(0.8,0.1,0.3,0.6), staplecol=F,
        ylim=c(0,MAXP), type = "l", lwd= 3, xaxt="n",  ylab="Estimated prevalence" , xlab="Age")
par(new=T)
plot((M.PrevHDTotal[,3]), type = "l", lwd= 3, col= "aquamarine3", ylim=c(0,MAXP), outline=FALSE, xaxt="n", ylab="", xlab="", whisklty = 1, medcol = "green")
par(new=T)
plot((M.PrevHDSus[,3]), type = "l", lwd= 3, col= rgb(0.1,0.1,0.7,0.5), ylim=c(0,MAXP), outline=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", boxlty = 0,  whisklty = 1, medcol = "blue", whiskcol = rgb(0.1,0.1,0.7,0.5), staplecol=F)
legend("topleft", legend = c("Infected", "Susceptible", "Total") , 
       col = c(rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5), "aquamarine3" ) , bty = "n", pch=20 , 
       pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
axis(1,at=c(10, 30, 50, 70))

########################################################### STOOOOOOPPPPPPPPPPPPPPPPPPPPPPPP ===========================

# Children 
TotChildSus<- TotalSus[C,]
HDChildSus= (Sm_Array[C,,Year]+ Ss_Array[C,,Year])
PrevHDChildSus <- round(colSums(HDChildSus)/ colSums(TotChildSus), digits=2)
PrevHDChildSus

TotChildInf<- TotalInf[C,]
HDChildInf= (Cm_Array[C,,Year] + Cs_Array[C,,Year])
PrevHDChildInf <- round(colSums(HDChildInf)/ colSums(TotChildInf), digits=2)
PrevChagasChild<- round(colSums(TotChildInf)/colSums(TotChildInf + TotChildSus), digits=4)

# Adults
TotAdultSus <- TotalSus[A,]
HDAdultSus= (Sm_Array[A,,Year]+Ss_Array[A,,Year])
PrevHDAdultSus <- round(colSums(HDAdultSus)/ colSums(TotAdultSus), digits=2)
PrevHDAdultSus

TotAdultInf<- TotalInf[A,]
HDAdultInf= (Cm_Array[A,,Year] +Cs_Array[A,,Year])
PrevHDAdultInf <- round(colSums(HDAdultInf)/ colSums(TotAdultInf), digits=2)
PrevHDAdultInf
PrevChagasAdult<- round(colSums(TotAdultInf)/colSums(TotAdultInf + TotAdultSus), digits=4)

df<- (cbind(PrevHDAdultInf, PrevHDAdultSus, PrevHDChildInf, PrevHDChildSus))
colnames(df)<- c("AdultsI", "AdultsS", "ChildrenI", "ChildrenS")
boxplot(df, main=(c("Heart Disease Prevalence", paste(place, NySim[Year]))),
        col=c(rgb(0.8,0.1,0.3,0.6), rgb(0.1,0.1,0.7,0.5), rgb(0.8,0.1,0.3,0.6), rgb(0.1,0.1,0.7,0.5)), 
        las=0, outline=T, horiz=F, xaxt='n', boxlty = 0,  whisklty = 1, medcol = c("red", "blue"), whiskcol = c("red", "blue"), staplecol=F,
        bty="n",
        ylab="Estimated prevalence" , xlab="Age group")
legend("topright", legend = c("Infected", "Susceptible") , 
       col = c(rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5) ) , bty = "n", pch=20 , 
       pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
abline(v=2.5, col="gray")
axis(1,at=c(1.5,3.5), c("Adults", "Children"))


df2<- (cbind(PrevChagasChild, PrevChagasAdult))
colnames(df2)<- c("Children", "Adults")
boxplot(df2*100, main=(c("T.cruzi Infection Prevalence (%)", paste(place, NySim[Year]))),
        col=c("pink", "red"), las=1, outline=FALSE)



###################################################
###################################################
###################################################
#                                                 #
#                   DEATHS                        #
#                                                 #
###################################################
###################################################
###################################################

par(mar=c(2, 4, 4, 2), xpd=F)
(par(mfrow=c(3,2)))

# Total deaths at Year X
DInd_Array<- DmI_Array + DsI_Array
DSus_Array<- DmS_Array + DsS_Array
totaldeaths=    (Du_Array[,,Year] + Da_Array[,,Year] + #Di_Array[,,Year]+ 
                   Dm_Array[,,Year] + Ds_Array[,,Year] + DInd_Array[,,Year]+
                   DSus_Array[,,Year])# 
totaldeathsCha<- Da_Array[,,Year] + #Di_Array[,,Year]+ 
  Dm_Array[,,Year] + Ds_Array[,,Year] 

prop.age.Du <-   Du_Array[,,Year]/totaldeaths
prop.age.DSus<-(DSus_Array[,,Year] + DInd_Array[,,Year])/totaldeaths
prop.age.DmS <-(DmS_Array[,,Year] + DmI_Array[,,Year])/totaldeaths
prop.age.DsS <-(DsS_Array[,,Year] + DsI_Array[,,Year])/totaldeaths
#prop.age.Di <-   Di_Array[,,Year]/totaldeaths
prop.age.Da <-   Da_Array[,,Year]/totaldeaths
prop.age.Dm <-   Dm_Array[,,Year]/totaldeaths
prop.age.Ds <-   Ds_Array[,,Year]/totaldeaths
prop.age.DCha<-  totaldeathsCha/totaldeaths


# Calculating Deaths and population for all years
TotD   = (Du_Array + Da_Array + Di_Array + Dm_Array + Ds_Array + DInd_Array + DSus_Array) # Total Deaths per year
TotDIx  = (Da_Array + Di_Array + Dm_Array + Ds_Array) # Total Deaths in Infected per year
TotD_NCHD = (DInd_Array + DSus_Array)
TotPop = (S_Array + Sm_Array + Ss_Array + Am_Array + As_Array + I_Array + Cm_Array + Cs_Array) # Total Population per year
TotIx <-  Am_Array + As_Array + I_Array + Cm_Array + Cs_Array


Prop.Ddue2Chagas<- TotDIx/TotD # Proportion of Deaths Chagas
Prop.D.NonChagasHD<- TotD_NCHD/TotD # Proportion of Deaths Chagas

# For deaths in infected (Da + Di + Dm + Ds)
Prop.Ddue2Cha.5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prop.Ddue2Cha.5;  A5<- Prop.Ddue2Chagas
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
Prop.D.dueCha.5<-A[,,]
rm(A, A5)

Prop.D.dueCha.36y<- Prop.D.dueCha.5[,,(115:150)]
Deaths.Chagas <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      Deaths.Chagas[j,i,k]<- round(Prop.D.dueCha.36y[j,i,k] * DeathsObs[j,k] )
    }}}

DeathsIxCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    DeathsIxCI[p,,pp]=round(quantile(Deaths.Chagas[p,,pp], c(.05,.5,.95)))
  }}

plot(c(DeathsIxCI[,3,36]), type ="l", col=1, xlab="ageclass", ylab="Absoulte number", 
     main=c("Chagas Deaths", paste(place,"2020"), 
            paste(sum(DeathsIxCI[,2,36]), "; 95%CI:", sum(DeathsIxCI[,1,36]), "-", sum(DeathsIxCI[,3,36]))))
points(c(DeathsIxCI[,2,36]), col=2)
lines(c(DeathsIxCI[,1,36]), col=1)
axis(side=1, at=(1:17),labels =agemod5 )


# For deaths in infected Da (Deaths in Acute Phase)
Prop.Da.5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prop.Da.5;  A5<- Da_Array/TotD
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
Prop.Da.5<-A[,,]
rm(A, A5)


Prop.Da.36y<- Prop.Da.5[,,(115:150)]
Deaths.A <- array(NA, dim=c(17,iterdis,36))

for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      Deaths.A[j,i,k]<- round(Prop.Da.36y[j,i,k] * DeathsObs[j,k] )
    }}}

DeathsACI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    DeathsACI[p,,pp]=round(quantile(Deaths.A[p,,pp], c(.05,.5,.95)))
  }}

plot(c(DeathsACI[,3,36]), type ="l", col=1, xlab="ageclass", ylab="Absolute number",
     main=c("Deaths Acute Chagas", paste(place, "2020"),
            paste(sum(DeathsACI[,2,36]), "; 95%CI:", sum(DeathsACI[,1,36]), "-", sum(DeathsACI[,3,36]))))
points(c(DeathsACI[,2,36]), col=2)
 lines(c(DeathsACI[,1,36]), col=1)

# For deaths in infected Di (Deaths in place 2020tomatic)
Prop.DCi.5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prop.DCi.5;  A5<- Di_Array/TotD
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
Prop.DCi.5<-A[,,]
rm(A, A5)

Prop.DCi.36y<- Prop.DCi.5[,,(115:150)]
Deaths.Ci <- array(NA, dim=c(17,iterdis,36))

for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      Deaths.Ci[j,i,k]<- round(Prop.DCi.36y[j,i,k] * DeathsObs[j,k] )
    }}}

DeathsCiCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    DeathsCiCI[p,,pp]=round(quantile(Deaths.Ci[p,,pp], c(.05,.5,.95)))
  }}

plot(c(DeathsCiCI[,3,36]), type ="l", col=1, xlab="ageclass", ylab="Absolute number",
     main=c("Deaths Asymptomatic Infection", paste(place, "2020"),
            paste(sum(DeathsCiCI[,2,36]), "; 95%CI:", sum(DeathsCiCI[,1,36]), "-", sum(DeathsCiCI[,3,36]))))
points(c(DeathsCiCI[,2,36]), col=2)
lines(c(DeathsCiCI[,1,36]), col=1)



#### For deaths in infected Ds and Dm (Deaths Symptomatic)
Prop.DCms.5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prop.DCms.5;  A5<- (Dm_Array + Ds_Array)/ TotD
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
Prop.DCms.5<-A[,,]
rm(A, A5)

Prop.DCMS.36y<- Prop.DCms.5[,,(115:150)]
Deaths.CMS <- array(NA, dim=c(17,iterdis,36))

for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      Deaths.CMS[j,i,k]<- round(Prop.DCMS.36y[j,i,k] * DeathsObs[j,k] )
    }}}

DeathsCMSCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    DeathsCMSCI[p,,pp]=round(quantile(Deaths.CMS[p,,pp], c(.05,.5,.95)))
  }}

plot(c(DeathsCMSCI[,3,36]), type ="l", col=1, xlab="ageclass", ylab="Absolute number",
     main=c("Deaths Symptomatic Chagas", paste(place, "2020"),
            paste(sum(DeathsCMSCI[,2,36]), "; 95%CI:", sum(DeathsCMSCI[,1,36]), "-", sum(DeathsCMSCI[,3,36]))))
points(c(DeathsCMSCI[,2,36]), col=2)
 lines(c(DeathsCMSCI[,1,36]), col=1)

 
# Deaths due to Non Chagasic Heart Conditions 
Prop.D.NonChagasHD.5 <- array(NA, dim=c(17, iterdis, 160))
A<- Prop.D.NonChagasHD.5;  A5<- Prop.D.NonChagasHD
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
Prop.D.NonChagasHD.5<- A[,,]
rm(A, A5)
boxplot(t(Prop.D.NonChagasHD.5[,,150]),
        xlab="ageclass", ylab ="Proportion",
        main= c("% Deaths Other Heart Conditions", paste(place, "2020")), outline=FALSE)

Prop.D.NonCha.36y<- Prop.D.NonChagasHD.5[,,(115:150)]
Deaths.NonCha <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      Deaths.NonCha[j,i,k]<- round(Prop.D.NonCha.36y[j,i,k] * DeathsObs[j,k] )
    }}}

DeathsNonChaCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    DeathsNonChaCI[p,,pp]=round(quantile(Deaths.NonCha[p,,pp], c(.05,.5,.95)))
  }}

plot(c(DeathsNonChaCI[,3,36]), type ="l", col=1, xlab="ageclass", ylab="Absolute number",
     main=c("Deaths Other Heart Conditions", paste(place, "2020"),
     paste(sum(DeathsNonChaCI[,2,36]), "; 95%CI:", sum(DeathsNonChaCI[,1,36]), "-", sum(DeathsNonChaCI[,3,36]))))
points(c(DeathsNonChaCI[,2,36]), col=2)
lines(c(DeathsNonChaCI[,1,36]), col=1)
sum(DeathsObs[,36])

PropDeathsChagas<- sum(Deaths.Chagas[,mean(iterdis),36])/ sum(DeathsObs[,36])
PropDeathsChagas

PropDeathsNonChHD<- sum(Deaths.NonCha[,mean(iterdis),36])/ sum(DeathsObs[,36])
PropDeathsNonChHD

PropDeathsHD<- (sum(Deaths.NonCha[,mean(iterdis),36]) + sum(Deaths.Chagas[,mean(iterdis),36])) / sum(DeathsObs[,36])
PropDeathsHD

########################################################
########################################################
########################################################
#                         -------                      #
#                          CASES                       #
#                         -------                      #
########################################################
########################################################
########################################################
par(mar=c(2, 4, 4, 2), xpd=F)
(par(mfrow=c(3,2)))

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

CasesChaChildren<- colSums(CasesChaCI[1:4,1:3,36])
CasesChaAdults<- colSums(CasesChaCI[5:17,1:3,36])
plot(1:10, 1:10, type="n", axes=FALSE)#, ann=FALSE)
text(5, 9, "Estimated number of T. cruzi infection cases", cex=1, col="red")
text(5, 8, paste("Subnational level:", place, "/Colombia"), cex=1, col="purple")
text(5, 7, "Year 2020", cex=1.5)
text(5, 5, col="darkorange", paste("Children =", CasesChaChildren[2], "cases, 95% CI =", CasesChaChildren[1],"-", CasesChaChildren[3]), cex=1)
text(5, 4, col="darkgreen", paste("Adults =", CasesChaAdults[2], "cases, 95% CI =", CasesChaAdults[1],"-", CasesChaAdults[3]), cex=1)

####################################
############  Asymptomatic Cases
######################################

Prev.Cix5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Cix5;  A5<- I_Array/TotPop
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
Prev.Cix5<-A[,,]
rm(A, A5)

Prev.Cix5.36y<- Prev.Cix5[,,(115:150)]
CasesCi <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesCi[j,i,k]<- round(Prev.Cix5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}
CasesCiCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    CasesCiCI[p,,pp]=round(quantile(CasesCi[p,,pp], c(.05,.5,.95)))
  }}

plot(c(CasesCiCI[,3,36]), type ="l", col=3,
     main=c("Asymptomatic Cases", paste(place,"2020"),
          paste(sum(CasesCiCI[,2,36]), "; 95%CI:", sum(CasesCiCI[,1,36]), "-", sum(CasesCiCI[,3,36]))),
          xlab="ageclass", ylab = "Absolute Number")
lines(c(CasesCiCI[,2,36]), col=2)
lines(c(CasesCiCI[,1,36]), col=3)


####################################
##      Acute Chagas Cases
######################################

# ============  Mild cases ==========

Prev.Amx5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Amx5;  A5<- (Am_Array )/TotPop
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
Prev.Amx5<-A[,,]
rm(A, A5)


Prev.Amx5.36y<- Prev.Amx5[,,(115:150)]
CasesAm <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesAm[j,i,k]<- round(Prev.Amx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}


# ============  Severe cases ==========

Prev.Asx5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Asx5;  A5<- (As_Array )/TotPop
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
Prev.Asx5<-A[,,]
rm(A, A5)

Prev.Asx5.36y<- Prev.Asx5[,,(115:150)]
CasesAs <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesAs[j,i,k]<- round(Prev.Asx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}


CasesA<- CasesAs+ CasesAm

CasesACI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    CasesACI[p,,pp]=round(quantile(CasesA[p,,pp], c(.05,.5,.95)))
  }}

plot(c(CasesACI[,3,36]), type ="l", col=3,
     main=c("Acute cases",  paste(place,"2020"),
             paste(sum(CasesACI[,2,36]), "; 95%CI:", sum(CasesACI[,1,36]), "-", sum(CasesACI[,3,36]))),
         xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(CasesACI[,3,36])))
lines(c(CasesACI[,2,36]), col=2)
lines(c(CasesACI[,1,36]), col=3)


####################################
##    Mild/Moderate Cardiomyopathy
#####################################

Prev.Cmx5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Cmx5;  A5<- (Cm_Array)/TotPop
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
Prev.Cmx5<-A[,,]
rm(A, A5)

Prev.Cmx5.36y<- Prev.Cmx5[,,(115:150)]
CasesCm <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesCm[j,i,k]<- round(Prev.Cmx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}
CasesCmCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    CasesCmCI[p,,pp]=round(quantile(CasesCm[p,,pp], c(.05,.5,.95)))
  }}

plot(c(CasesCmCI[,3,36]), type ="l", col=3,
     main=c("Mild/Moderate cases", paste(place,"2020"),
             paste(sum(CasesCmCI[,2,36]), "; 95%CI:", sum(CasesCmCI[,1,36]), "-", sum(CasesCmCI[,3,36]))),
     xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(CasesCmCI[,3,36])))
lines(c(CasesCmCI[,2,36]), col=2)
lines(c(CasesCmCI[,1,36]), col=3)

####################################
############ Severe  cardiomyopathy
######################################

Prev.Csx5 <-array(NA, dim=c(17, iterdis, 160))
A<- Prev.Csx5;  A5<- (Cs_Array)/TotPop
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
Prev.Csx5<-A[,,]
rm(A, A5)

Prev.Csx5.36y<- Prev.Csx5[,,(115:150)]
CasesCs <- array(NA, dim=c(17,iterdis,36))
for (i in 1:iterdis){
  for (j in 1:17){
    for (k in 1:36){
      CasesCs[j,i,k]<- round(Prev.Csx5.36y[j,i,k] * Obs.Pop[j,k] )
    }}}


CasesCsCI=array(NA,dim=c(17,3,36))
for (p in 1:17){
  for (pp in 1:36){
    CasesCsCI[p,,pp]=round(quantile(CasesCs[p,,pp], c(.05,.5,.95)))
  }}

plot(c(CasesCsCI[,3,36]), type ="l", col=3,
     main=c("Severe Cases", paste(place, "2020"),
             paste(sum(CasesCsCI[,2,36]), "; 95%CI:", sum(CasesCsCI[,1,36]), "-", sum(CasesCsCI[,3,36]))),
     xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(CasesCsCI[,3,36])))
lines(c(CasesCsCI[,2,36]), col=2)
lines(c(CasesCsCI[,1,36]), col=3)


TotalDeathsHD<- Deaths.NonCha + Deaths.Chagas
sum(TotalDeathsHD[,5,36])/sum(DeathsObs[,36])  
sum(Deaths.NonCha[,5,36])/sum(DeathsObs[,36])

########################################################
########################################################
########################################################
#                         -------                      #
#                         BURDEN                       #
#                         -------                      #
########################################################
########################################################
########################################################

par(mar=c(2, 4, 4, 2), xpd=F)
(par(mfrow=c(3,2)))


#================================== 
#                YLL 
#==================================

YLL <- array(NA, dim=c((Na/5),iterdis,36))
for (i in 1:36){  
  for (j in 1:iterdis){  
    for (k in 1:17){
      YLL[k,j,i]<- Deaths.Chagas[k,j,i] * LEx5[k]}}}
# boxplot(t(YLL[,,36]),
#         main=c(place, "2020", paste("YLL=", round(sum(YLL[,1,36])))),
#         xlab="ageclass", col="orange")
sum(YLL[,1,36])


#================================== 
#                YLD 
#==================================

YLD.Am <- array(NA, dim=c((Na/5),iterdis,36))
YLD.As <- array(NA, dim=c((Na/5),iterdis,36))
YLD.Cm <- array(NA, dim=c((Na/5),iterdis,36))
YLD.Cs <- array(NA, dim=c((Na/5),iterdis,36))
for (i in 1:36){  
  for (j in 1:iterdis){  
    for (k in 1: 85){
      YLD.Am[,j,i] <- CasesAm[,j,i] * wamD[j]
      YLD.As[,j,i] <- CasesAs[,j,i] * wasD[j]
      YLD.Cm[,j,i] <- CasesCm[,j,i] * wcmD[j]
      YLD.Cs[,j,i] <- CasesCs[,j,i] * wcsD[j]
    }}}

YLD<- (YLD.Am + YLD.As + YLD.Cs + YLD.Cm)
# boxplot(t(YLD[,,36]),
#         main=c(place, "2020", paste("YLD=", round(sum(YLD[,5,36])))),
#         xlab="ageclass", col="purple")

#================================== 
#                DALYs 
#==================================

DALY<- YLD + YLL
# boxplot(t(DALY[,,36]),
#         main=c(place, "2020", paste("DALY=", round(sum(DALY[,5,36])))),
#         xlab="ageclass", col="Green")

DALY_CI=array(NA,dim=c(17,3,36))
YLL_CI=array(NA,dim=c(17,3,36))
YLD_CI=array(NA,dim=c(17,3,36))

for (p in 1:17){
  for (pp in 1:36){
    DALY_CI[p,,pp]=round(quantile(DALY[p,,pp], c(.05,.5,.95)))
    YLL_CI[p,,pp]=round(quantile(YLL[p,,pp], c(.05,.5,.95)))
    YLD_CI[p,,pp]=round(quantile(YLD[p,,pp], c(.05,.5,.95)))
  }}
agec<- 1:17

# Plot YLL
plot(c(YLL_CI[,3,36]), type ="l", col=4,lwd=1,
     main=c("YLLs", paste(place, "2020"),
            paste(sum(YLL_CI[,2,36]), "; 95%CI:", sum(YLL_CI[,1,36]), "-", sum(YLL_CI[,3,36]))),               
     xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(YLL_CI[,3,36])))
polygon(c(agec,rev(agec)),c(YLL_CI[,1,36],rev(YLL_CI[,3,36])),
        border = FALSE, col="lightskyblue1", main=NA)
lines(c(YLL_CI[,2,36]), col=4, lwd=3)

#Plot YLD
plot(c(YLD_CI[,3,36]), type ="l", col=2,lwd=1,
     main=c("YLDs", paste(place, "2020"),
            paste(sum(YLD_CI[,2,36]), "; 95%CI:", sum(YLD_CI[,1,36]), "-", sum(YLD_CI[,3,36]))),               
     xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(YLD_CI[,3,36])))
polygon(c(agec,rev(agec)),c(YLD_CI[,1,36],rev(YLD_CI[,3,36])),
        border = FALSE, col="lightpink", main=NA)
lines(c(YLD_CI[,2,36]), col=2, lwd=3)


#Plot DALYs
plot(c(DALY_CI[,3,36]), type ="l", col=3,lwd=1,
     main=c("DALYs", paste(place, "2020"),
            paste(sum(DALY_CI[,2,36]), "; 95%CI:", sum(DALY_CI[,1,36]), "-", sum(DALY_CI[,3,36]))),               
     xlab="ageclass", ylab = "Absolute Number", ylim = c(0,max(DALY_CI[,3,36])))
polygon(c(agec,rev(agec)),c(DALY_CI[,1,36],rev(DALY_CI[,3,36])),
        border = FALSE, col="darkseagreen1", main=NA)
lines(c(DALY_CI[,2,36]), col="mediumseagreen", lwd=3)

# Total DALYs over time
totDALY<- matrix(NA, iterdis,36)
for (i in 1:36){
  for (j in 1:iterdis){
    totDALY[j,i]<- sum(DALY[,j, i])
  }
}
boxplot(totDALY, 
        main=c("DALYs over time", paste(place, "1985-2020")), 
        col="pink", ylab="Absolute number", outline=FALSE)

totDALYCI<-matrix(NA, 3, 36) 
  for (i in 1:36){
    totDALYCI[,i]<- quantile(totDALY[,i], c(0.05, .5, .95))}


# Total Cases over time
totCases<- matrix(NA, iterdis,36)
for (i in 1:36){
  for (j in 1:iterdis){
    totCases[j,i]<- sum(CasesCha[,j, i])
  }
}

boxplot(totCases, 
        main=c("Total Cases over time", paste(place, "1985-2020")), 
        col="darkseagreen1", ylab="Absolute number", outline=FALSE)

totCasesCI<-matrix(NA, 3, 36) 
for (i in 1:36){
  totCasesCI[,i]<- quantile(totCases[,i], c(0.05, .5, .95))}


########################################################
########################################################
########################################################
#                 --------------                       #
#        -------*  CURRENT EPI SITUATION *-------------#
#                 ---------------                      #
########################################################
########################################################
########################################################
YI<- 36# fro 2016
CASES<- colSums(CasesChaCI[,,YI])
ASYMP<- colSums(CasesCiCI[,,YI])
ACUTE<- colSums(CasesACI[,,YI])
CH.MOD<- colSums(CasesCmCI[,,YI])
CH.SEV<- colSums(CasesCsCI[,,YI])
DEATHS<- colSums(DeathsIxCI[,,YI])
DALYs<- colSums(DALY_CI[,,YI])

DA<- cbind(CASES, ASYMP, CH.MOD, CH.SEV, ACUTE, DEATHS, DALYs)
colnames(DA)<- c("Total Cases", "C. Asymptomatic", "C. Moderate", "C. Severe", "Acute", "Deaths", "DALYs")
library(Hmisc)
par(las=2) # make label text perpendicular to axis
par(mar=c(1,10,4,2)) 
bp<- barplot(DA[2,], ylim=c(0,max(DA[3,1])), main = c("Burden Chagas Disease", place,"2020"), col=c(2:8), cex.names=.5) 
errbar(x=bp[,1], DA[2,], DA[3,], DA[1,], add=T, xlab="")
legend("topright", colnames(DA), fill = c(2:8), bty = "n", cex = 1)



########################################################
########################################################
########################################################
#                  --------------                      #
#        -------* SENSITIVITY ANALYSIS *-------------- #
#                 ---------------                      #
########################################################
########################################################
########################################################

# Sentitivity based on Matrix Correlation
library(corrplot)
par(mar=c(6, 4, 2, 2), xpd=T)
(par(mfrow=c(1,1)))

M<- data.frame(colSums(DALY[,,36]), colSums(YLL[,,36]), colSums(YLD[,,36]), 
               colSums(CasesCha[,,36]), colSums(CasesCi[,,36]), colSums(CasesA[,,36]), 
               colSums(CasesCm[,,36]), colSums(CasesCs[,,36]), colSums(Deaths.Chagas[,,36]),
               lambdaD[,36], mu_1D, mu_aD,   
               mu_cmD, mu_csD, RRmD, gammaD, deltaD, alphaD,
               betaD, RRpD , wamD, wasD, 
               wcmD, wcsD)

colnames(M)<- c("DALY", "YLL", "YLD", 
                  "CasesCha", "Asymptomatic", "Acute", 
                  "Chronic.Mild", "Chronic.Severe", "Deaths",
                  "lambda", "mu_1", "mu_a",   
                  "mu_cm", "mu_cs", "RRm", "gamma", "delta", "alpha",
                  "beta", "RRp", "wam", "was", "wcm", "wcs")

library("sensitivity")
X<- M[,c("lambda", "mu_a", "mu_cm", "mu_cs", "RRm", "gamma", "delta", "alpha",  "beta",   "RRp")] #,    "wam",    "was",    "wcm",    "wcs")]
letters<-c(expression(paste(lambda)), expression(paste(mu,'a')), expression(paste(mu,'cm')), expression(paste(mu,'cs')),
           expression(RRm), expression(gamma), expression(delta), expression(alpha), expression(beta),
           expression(RRp))
names(X)<- letters
# , expression(wam), expression(was), expression(wcm), expression(wcs))

par(mar=c(2, 4, 4, 2), xpd=F)
(par(mfrow=c(3,2)))

## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = M$YLL))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="YLL", las=2, xlab="PRCC")

library(epiR)
dat<- data.frame(cbind(X, Y = M$DALY))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="DALY", las=2, xlab="PRCC")


library(epiR)
dat<- data.frame(cbind(X, Y = M$YLD))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="YLD", las=2, xlab="PRCC")

library(epiR)
dat<- data.frame(cbind(X, Y = M$CasesCha))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="Cases", las=2, xlab="PRCC")


dat<- data.frame(cbind(X, Y = M$Deaths))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="Deaths", las=2, xlab="PRCC")


dat<- data.frame(cbind(X, Y = M$Acute))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="Acute", las=2, xlab="PRCC")


########################################################
########################################################
########################################################
#                  --------------                      #
#        -------* EXTRACTING DATA *-------------- #
#                 ---------------                      #
########################################################
########################################################
########################################################

# Data by age for 4 years of interest 1990, 2000, 2010, 2020
YearsI<-c(6,16,26,36) # Years of Interest for Outputs
Data_YIa<-(rbind(data.frame(DeathsIxCI[,,YearsI]), # "Deaths.Total"
                 data.frame(DeathsACI[,,YearsI]),  
                data.frame(DeathsCiCI[,,YearsI]),
               data.frame(DeathsCMSCI[,,YearsI]),
            data.frame(DeathsNonChaCI[,,YearsI]),
                data.frame(CasesChaCI[,,YearsI]),
                 data.frame(CasesCiCI[,,YearsI]),
                  data.frame(CasesACI[,,YearsI]),
                 data.frame(CasesCmCI[,,YearsI]),
                 data.frame(CasesCsCI[,,YearsI]),
                   data.frame(DALY_CI[,,YearsI]),
                    data.frame(YLL_CI[,,YearsI]),
                    data.frame(YLD_CI[,,YearsI])))
namesD<- as.vector(c(rep("Deaths.Total",17),  # DeathsIxCI
           rep("Deaths.Acute",17),   # DeathsACI
           rep("Deaths.Asymptomatic",17), # DeathsCiCI
           rep("Deaths.ChrSymptoms",17),    # DeathsCMSCI
           rep("Deaths.OtherHD",17),    # DeathsNonChaCI
           rep("Cases.Total",17),    # CasesChaCI
           rep("Cases.Asymptomatic",17),   # CasesCiCI
           rep("Cases.Acute",17),   # CasesACI
           rep("Cases.ChronicMild",17),   # CasesCmCI
           rep("Cases.ChronicSevere",17),   # CasesCsCI
           rep("DALY",17), #DALY_CI
           rep("YLL",17),  #YLL_CI
           rep("YLD",17)))
Data_YIb<-data.frame(namesD,Data_YIa) #YLD_CI
Data_out<- data.frame(rep(place, length(Data_YIb)), Data_YIb)
colnames(Data_out)<- c("place", "metric","1990L", "1990M", "1990U", "2000L", "2000M", "2000U", "2010L", "2010M", "2010U", "2020L", "2020M", "2020U")


# Total cases and DALYS, by years x 36 years
Data36ya<- data.frame(c(rep(place,6)), rbind(totCasesCI, totDALYCI))
Data36y<- data.frame(c("CasesL", "CasesM", "CasesU", "DALY.L", "DALY.M", "DALY.U"), Data36ya)
colnames(Data36y)<-c("Data",'place',1985:2020)



#### ALL DATA 36 YEARS
# Data by age for 4 years of interest 1990, 2000, 2010, 2020
YearsI<-c(1:36) # Years of Interest for Outputs
INT<-1
Data_36.L<-(rbind(data.frame(DeathsIxCI[,INT,YearsI]), # "Deaths.Total"
                 data.frame(DeathsACI[,INT,YearsI]),  
                 data.frame(DeathsCiCI[,INT,YearsI]),
                 data.frame(DeathsCMSCI[,INT,YearsI]),
                 data.frame(DeathsNonChaCI[,INT,YearsI]),
                 data.frame(CasesChaCI[,INT,YearsI]),
                 data.frame(CasesCiCI[,INT,YearsI]),
                 data.frame(CasesACI[,INT,YearsI]),
                 data.frame(CasesCmCI[,INT,YearsI]),
                 data.frame(CasesCsCI[,INT,YearsI]),
                 data.frame(DALY_CI[,INT,YearsI]),
                 data.frame(YLL_CI[,INT,YearsI]),
                 data.frame(YLD_CI[,INT,YearsI])))
namesD<- as.vector(c(rep("Deaths.Total",17),  # DeathsIxCI
                     rep("Deaths.Acute",17),   # DeathsACI
                     rep("Deaths.Asymptomatic",17), # DeathsCiCI
                     rep("Deaths.ChrSymptoms",17),    # DeathsCMSCI
                     rep("Deaths.OtherHD",17),    # DeathsNonChaCI
                     rep("Cases.Total",17),    # CasesChaCI
                     rep("Cases.Asymptomatic",17),   # CasesCiCI
                     rep("Cases.Acute",17),   # CasesACI
                     rep("Cases.ChronicMild",17),   # CasesCmCI
                     rep("Cases.ChronicSevere",17),   # CasesCsCI
                     rep("DALY",17), #DALY_CI
                     rep("YLL",17),  #YLL_CI
                     rep("YLD",17)))
Data_36b.L<-data.frame(namesD,Data_36.L) #YLD_CI
Data_outL<- data.frame(rep(place, dim(Data_36b.L)[1]), rep("Low", dim(Data_36b.L)[1]), Data_36b.L)
colnames(Data_outL)<- c("place", "bound","metric", c(1985:2020))




#### ALL DATA 36 YEARS
# Data by age for 4 years of interest 1990, 2000, 2010, 2020
YearsI<-c(1:36) # Years of Interest for Outputs
INT<-2
Data_36.M<-(rbind(data.frame(DeathsIxCI[,INT,YearsI]), # "Deaths.Total"
                  data.frame(DeathsACI[,INT,YearsI]),  
                  data.frame(DeathsCiCI[,INT,YearsI]),
                  data.frame(DeathsCMSCI[,INT,YearsI]),
                  data.frame(DeathsNonChaCI[,INT,YearsI]),
                  data.frame(CasesChaCI[,INT,YearsI]),
                  data.frame(CasesCiCI[,INT,YearsI]),
                  data.frame(CasesACI[,INT,YearsI]),
                  data.frame(CasesCmCI[,INT,YearsI]),
                  data.frame(CasesCsCI[,INT,YearsI]),
                  data.frame(DALY_CI[,INT,YearsI]),
                  data.frame(YLL_CI[,INT,YearsI]),
                  data.frame(YLD_CI[,INT,YearsI])))
Data_36b.M<-data.frame(namesD,Data_36.M)
Data_outM<- data.frame(rep(place, dim(Data_36b.M)[1]), rep("Median", dim(Data_36b.M)[1]), Data_36b.M)
colnames(Data_outM)<- c("place", "bound","metric", c(1985:2020))




#### ALL DATA 36 YEARS

YearsI<-c(1:36) # Years of Interest for Outputs
INT<-3
Data_36.U<-(rbind(data.frame(DeathsIxCI[,INT,YearsI]), # "Deaths.Total"
                  data.frame(DeathsACI[,INT,YearsI]),  
                  data.frame(DeathsCiCI[,INT,YearsI]),
                  data.frame(DeathsCMSCI[,INT,YearsI]),
                  data.frame(DeathsNonChaCI[,INT,YearsI]),
                  data.frame(CasesChaCI[,INT,YearsI]),
                  data.frame(CasesCiCI[,INT,YearsI]),
                  data.frame(CasesACI[,INT,YearsI]),
                  data.frame(CasesCmCI[,INT,YearsI]),
                  data.frame(CasesCsCI[,INT,YearsI]),
                  data.frame(DALY_CI[,INT,YearsI]),
                  data.frame(YLL_CI[,INT,YearsI]),
                  data.frame(YLD_CI[,INT,YearsI])))

Data_36b.U<-data.frame(namesD,Data_36.U)
Data_outU<- data.frame(rep(place, dim(Data_36b.U)[1]), rep("Upper", dim(Data_36b.U)[1]),Data_36b.U)
colnames(Data_outU)<- c("place", "bound","metric", c(1985:2020))
Data_out36<- rbind(Data_outM,Data_outL, Data_outU)

#Saving files
write.csv(Data36y, paste("res/Data36yB", place,".csv"))
write.csv(Data_out, paste("res/Data_outB", place,".csv"))
write.csv(M, paste("res/SEN",place, ".csv"))
write.csv(Data_out36, paste("res/Data_out36B", place, ".csv"))





# Stop the clock
proc.time() - ptm
print(paste("END", place))
print(Sys.time()- ptm2)

#
save.image(paste(place, "Burden", iterdis,".RData"))
dev.off()
dev.off()




# 