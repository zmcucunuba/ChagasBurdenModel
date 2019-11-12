

FOI_import_and_format <- function (place){
  
  # FOI Data 
  # We need the FOI for each location to be available from 1871 and until 2030
  
  lambdaNy  <-  readRDS(paste0('data/foi/FoIPred_Post_', place))
  min_year_wanted  <- 1871
  max_year_wanted  <- 2030
  min_year_data    <- as.numeric(min(names(lambdaNy)))
  max_year_data    <- as.numeric(max(names(lambdaNy)))
  
  lambdaNyBefore <- data.frame(matrix(lambdaNy[,1], nrow= NROW(lambdaNy), 
                                      ncol = min_year_data - min_year_wanted))
  colnames(lambdaNyBefore) <- as.character(min_year_wanted: (min_year_data -1))
  
  lambdaNyAfter <- data.frame(matrix(lambdaNy[,NCOL(lambdaNy)], nrow= NROW(lambdaNy), 
                                     ncol = max_year_wanted - max_year_data))
  colnames(lambdaNyAfter) <- as.character((max_year_data +1): (max_year_wanted))
  
  lambdaNyTotal <- cbind(lambdaNyBefore, lambdaNy, lambdaNyAfter)
  
  return(lambdaNyTotal)
}


Pop_import_and_format <- function (place, setting )
{

# Chosing Demography
Pop   <- read.csv("data/dem/Obs.Alive.Totalx5.csv")
Obs.Pop <- (Pop[which(Pop$dept==place),])[,-(c(1,38,39))]

PopR  <- read.csv("data/dem/Obs.Alive.Ruralx5.csv")
Obs.PopR <- (PopR[which(PopR$dept==place),])[,-(c(1,38,39))]

colnames(Obs.Pop) <-1985:2020
colnames(Obs.PopR) <-1985:2020

if (setting == 'rural') {
  return(Obs.PopR)
}

if (setting == 'total') {
  return(Obs.Pop)
}


}




Deaths_import_and_format <- function(place) {
  Deaths <- read.csv(("data/dem/Obs.Deaths.Col_Corrected.csv"))
  DeathsObs<- (Deaths[which(Deaths$depto==place),])[,-(c(1,38, 39))]
  colnames(DeathsObs)<-1985:2020
  
  return(Deaths)
  
  
}

