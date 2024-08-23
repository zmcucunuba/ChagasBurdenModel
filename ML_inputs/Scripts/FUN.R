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
  A<-array(NA, dim=c(23, iterdis, dim(x)[3]))
  
  for (l in 1:dim(x)[3]){
      A[1,,l]<-  (x[(1),,l])    # [0-1[
      A[2,,l]<- (x[(2),,l])     # [1-2[
      A[3,,l]<-  colMeans(x[(3:5),,l], na.rm=T)  # [2-5[
      A[4,,l]<- colMeans(x[(6:10),,l], na.rm=T)  # [5-10[
      A[5,,l]<- colMeans(x[(11:15),,l], na.rm=T) # [10-15[
      A[6,,l]<- colMeans(x[(16:20),,l], na.rm=T) # [15-20[
      A[7,,l]<- colMeans(x[(21:25),,l], na.rm=T) # [20-25[
      A[8,,l]<- colMeans(x[(26:30),,l], na.rm=T) # [25-30[
      A[9,,l]<- colMeans(x[(31:35),,l], na.rm=T) # [30-35[
      A[10,,l]<- colMeans(x[(36:40),,l], na.rm=T)# [35-40[
      A[11,,l]<- colMeans(x[(41:45),,l], na.rm=T)# [40-45[
      A[12,,l]<- colMeans(x[(46:50),,l], na.rm=T)# [45-50[
      A[13,,l]<- colMeans(x[(51:55),,l], na.rm=T)# [50-55[
      A[14,,l]<- colMeans(x[(56:60),,l], na.rm=T)# [55-60[
      A[15,,l]<- colMeans(x[(61:65),,l], na.rm=T)# [60-65[
      A[16,,l]<- colMeans(x[(66:70),,l], na.rm=T)# [65-70[
      A[17,,l]<- colMeans(x[(71:75),,l], na.rm=T)# [70-75[
      A[18,,l]<- colMeans(x[(76:80),,l], na.rm=T)# [75-80[
      A[19,,l]<- colMeans(x[(81:85),,l], na.rm=T)# [80-85[
      A[20,,l]<- colMeans(x[(86:90),,l], na.rm=T)# [85-90[
      A[21,,l]<- colMeans(x[(91:95),,l], na.rm=T)# [90-95[
      A[22,,l]<- colMeans(x[(96:100),,l],na.rm=T)# [95-100[
      A[23,,l]<- x[(101),,l]  # >100
    }
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
  LEx5[(n/5)+1]<- mean(LE85[n:(n+4)]) # problem with age class / check the indexing above, same as AC101to23 function
}
rm(LE85, LE)
