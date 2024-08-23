

ADM2_FOI_import_and_format <- function (place, setting, Nb_iter){
  
  # FOI Data 
  # We need the FOI for each location to be available from 1871 and until 2030
  
  lambdaNy  <-  readRDS(paste0("ML_inputs/Data/FoI/",Nb_iter,"it/Post_FoIPred_", setting ,"_",place))
  min_year_wanted  <- 1871
  max_year_wanted  <- 2030
  min_year_data    <- as.numeric(min(colnames(lambdaNy)))
  max_year_data    <- as.numeric(max(colnames(lambdaNy)))
  
  lambdaNyBefore <- data.frame(matrix(lambdaNy[,1], nrow= NROW(lambdaNy), 
                                      ncol = min_year_data - min_year_wanted))
  colnames(lambdaNyBefore) <- as.character(min_year_wanted: (min_year_data -1))
  
  lambdaNyAfter <- data.frame(matrix(lambdaNy[,NCOL(lambdaNy)], nrow= NROW(lambdaNy), 
                                     ncol = max_year_wanted - max_year_data))
  colnames(lambdaNyAfter) <- as.character((max_year_data +1): (max_year_wanted))
  
  lambdaNyTotal <- cbind(lambdaNyBefore, lambdaNy, lambdaNyAfter)
  
  return(lambdaNyTotal)
}


ADM2_Pop_import_and_format <- function (place, setting )
{
  Pop   <- readRDS(paste0("ML_inputs/Data/dem/ADM2_Obs_Alive_DPMP/",setting,"_", place))
  return(Pop)
  }



ADM2_Deaths_import_and_format <- function(place, setting) {
  Deaths <- readRDS(paste0("ML_inputs/Data/dem/ADM2_Obs_Deaths_DPMP/", setting, "_", place))
  return(Deaths)
  }

