#####################################################
#====================================================
#             MODEL CHAGAS BURDEN WITH ML INPUTS              
#====================================================
#####################################################

# 08/11/2021
# Julia Ledien, Zulma Cucunuba
# FoI inputs model : ML_A3_RFlog

## FUNCTIONS for main with RCCP ADM2 Nov2021

IClo<-function(x){
  mean(x)-qnorm(.975)*(sd(x)/sqrt(iterdis))
}
ICup<-function(x){
  mean(x)+qnorm(.975)*(sd(x)/sqrt(iterdis))
}

suma<- function (x){
  c(min(x), quantile(x, c(0.25, 0.5, 0.75)), max(x),mean(x), sd(x),IClo(x),ICup(x))
}

AC101to23<- function(x){
  A<-array(NA, dim=c(23, iterdis, 160))
  for (l in 1:160){
    for(n in 1:iterdis){
      A[1,n,l]<-  mean(x[(1),n,l], na.rm=T)    # [0-1[
      A[2,n,l]<- mean(x[(2),n,l], na.rm=T)     # [1-2[
      A[3,n,l]<-  mean(x[(3:5),n,l], na.rm=T)  # [2-5[
      A[4,n,l]<- mean(x[(6:10),n,l], na.rm=T)  # [6-10[
      A[5,n,l]<- mean(x[(11:15),n,l], na.rm=T) # [10-15[
      A[6,n,l]<- mean(x[(16:20),n,l], na.rm=T) # [15-20[
      A[7,n,l]<- mean(x[(21:25),n,l], na.rm=T) # [20-25[
      A[8,n,l]<- mean(x[(26:30),n,l], na.rm=T) # [25-30[
      A[9,n,l]<- mean(x[(31:35),n,l], na.rm=T) # [30-35[
      A[10,n,l]<- mean(x[(36:40),n,l], na.rm=T)# [35-40[
      A[11,n,l]<- mean(x[(41:45),n,l], na.rm=T)# [40-45[
      A[12,n,l]<- mean(x[(46:50),n,l], na.rm=T)# [45-50[
      A[13,n,l]<- mean(x[(51:55),n,l], na.rm=T)# [50-55[
      A[14,n,l]<- mean(x[(56:60),n,l], na.rm=T)# [55-60[
      A[15,n,l]<- mean(x[(61:65),n,l], na.rm=T)# [60-65[
      A[16,n,l]<- mean(x[(66:70),n,l], na.rm=T)# [65-70[
      A[17,n,l]<- mean(x[(71:75),n,l], na.rm=T)# [70-75[
      A[18,n,l]<- mean(x[(76:80),n,l], na.rm=T)# [75-80[
      A[19,n,l]<- mean(x[(81:85),n,l], na.rm=T)# [80-85[
      A[20,n,l]<- mean(x[(86:90),n,l], na.rm=T)# [85-90[
      A[21,n,l]<- mean(x[(91:95),n,l], na.rm=T)# [90-95[
      A[22,n,l]<- mean(x[(96:100),n,l],na.rm=T)# [95-100[
      A[23,n,l]<- mean(x[(101),n,l], na.rm=T)  # >100
    }}
  return(A)
}


###Life expectancy


LE   <-  read.csv("ML_inputs/Data/dem/life expectancy GBD85.csv")
LE85 <- LE$life.exp
LEx5 <-as.matrix(numeric(23))


LEx5[1]<-  LE85[1]
LEx5[2]<-  LE85[2]
LEx5[3]<-  mean(LE85[3:5])
LEx5[18:23]<- mean(LE85[81:85])
SEQ<- seq(6, 81, 5)
for (n in SEQ){
  LEx5[(n/5)+1]<- mean(LE85[n:(n+4)])
}
rm(LE85, LE)
