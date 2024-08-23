#====================================================================
#####################################################################
#               CHAGAS DISEASE BURDEN MODEL
#        INPUT DATA GENERATION FOR LINEAR MODELS
#####################################################################
#====================================================================
# Julia Ledien 17/09/2020

## Municipality data level
## Shapefile = GDAM
## Models: FullPost FoI R2Ov Model-averaging

Predictors <- readRDS( "C:/Users/Julia L/Documents/GitHub/chagas-ML/analysis/FoI Linear Regression/Continuous FoI/Median/ADM2/R-based selection/predictions/predictors_ADM2")
Municipalities <- Predictors[1:1065,1]
WeightTable<- readRDS("C:/Users/Julia L/Documents/GitHub/chagas-ML/analysis/FoI Linear Regression/Continuous FoI/Full_posterior/Model averaging/WeightTable2")

iter=100
years<- 1950:2014

################################################### too slow
for (j in c("urban", "rural", "indigenous")){
  modelT<-  WeightTable[which(WeightTable$zone==j),]
  models<- sub(".*XXLoutput_", "", modelT$model) 
  
  for (mun in 1:length(Municipalities)){
    mat <- matrix(NA, iter, length(years))
    colnames(mat)<- 1950:2014
    
    for (i in 1950:2014){
      v <- NA
      
      for (m in models){
        raw_pred<- readRDS(paste0("C:/Users/Julia L/Dropbox/Chagas modelling/Linear models/FullPosteriorFoI/predictions/ModelAveraging/PredictionsMatrix/",j,"/", i,"/",m,"_",i))
        w <- round(WeightTable$Weight[WeightTable$model== paste0("XXLoutput_", m)]*iter)
        v <- c(v,raw_pred[mun,1:w])
      }
      
      mat[,i-1949]<-v[1:iter+1]
    }
    
    saveRDS(mat,paste("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/LinearModels/Inputs/Post_FoIPred", j, Municipalities[mun],  sep="_" ))
  }
}

############################ New version
for (j in c("urban", "rural", "indigenous")){
  modelT<-  WeightTable[which(WeightTable$zone==j),]
  models<- sub(".*XXLoutput_", "", modelT$model) 
  for (i in 1950:2014){
    for (m in models){
      raw_pred<- readRDS(paste0("C:/Users/Julia L/Dropbox/Chagas modelling/Linear models/FullPosteriorFoI/predictions/ModelAveraging/PredictionsMatrix/",j,"/", i,"/",m,"_",i))
      for (mun in 1:length(Municipalities)){
        dt <- raw_pred[mun,]
        saveRDS(dt, paste("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/LinearModels/FoIPred/Pred",j,Municipalities[mun],i,m, sep="_"))
      }
    }
  }
}

for (j in c("rural")){
  modelT<-  WeightTable[which(WeightTable$zone==j),]
  models<- sub(".*XXLoutput_", "", modelT$model) 
  
  for (mun in Municipalities){
    mat <- matrix(NA, iter, length(years))
    colnames(mat)<- 1950:2014
    
    for (i in 1950:2014){
      v <- NA
      
      for (m in models){
        pred<- readRDS(paste("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/LinearModels/FoIPred/Pred",j,mun,i,m, sep="_"))
        w <- round(WeightTable$Weight[WeightTable$model== paste0("XXLoutput_", m)]*iter)
        v <- c(v,pred[1:w])
      }
      
      mat[,i-1949]<-v[1:iter+1]
    }
    
    saveRDS(mat,paste("C:/Users/Julia L/Dropbox/Chagas modelling/BurdenModel/LinearModels/Inputs/Post_FoIPred", j, mun,  sep="_" ))
  }
}

    