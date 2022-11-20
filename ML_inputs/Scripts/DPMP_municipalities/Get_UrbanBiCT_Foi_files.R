###Get urbanBigCT files

library (readxl)
PopSize1985<- read_xlsx("C:/Users/ledie/Box/notes/PopSize1985.xlsx")
BigCities<- PopSize1985$DPMP[which(PopSize1985$`1985`>100000)]
Municipalities<- PopSize1985$DPMP[which(PopSize1985$`1985`<=100000)]

it=100

for (mun in Municipalities){
  foi  <-  readRDS(paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_urban_CO",mun))
  saveRDS(foi,paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_urbanBigCT_CO",mun))
}

for (mun in BigCities){
  foi  <-  readRDS(paste0("C:/Users/ledie/Box/Analyses/MachineLearning/MunLM/BurdenModel/BloobBanksPrevalence/FoI_Posteriors/Post_FoIPred_urbanBigCT_CO",mun))
  saveRDS(foi,paste0("ML_inputs/Data/FoI/",it,"it/Post_FoIPred_urbanBigCT_CO",mun))
}

