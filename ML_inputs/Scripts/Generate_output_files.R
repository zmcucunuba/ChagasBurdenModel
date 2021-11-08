#####################################################
#====================================================
#             MODEL CHAGAS BURDEN WITH ML INPUTS              
#====================================================
#####################################################

# 08/11/2021
# Julia Ledien, Zulma Cucunuba
# FoI inputs model : ML_A3_RFlog



# generation of outputs files

c.names<- c("min", "Q25", "med", "Q75", "max", "mean", "sd", "95CI_lo", "95CI_up")
r.names<- municipalities
m.names<- years
template<- array(NA,dim=c(length(municipalities),9,length(years)),dimnames = list(r.names, c.names, m.names))

### Burden output:
### Number of cases = NbCases
### Number of death attibutable to CD = NbDeath
### Years of Life lost = YLL
### Years of Life Disabled = YLD
### DALYs = DALYs

### Age Classes:
###   0 - 5 = AC1
###  5 - 20 = AC2
### 20 - 40 = AC3
### 40 - 60 = AC4
###    >60  = AC5
###   Total = ACt

### Stage of the disease:
### Acute = StAc
### Asymptomatic = StAs
### Chronic = StCh
### Severe = StChS
### All Infected = StI

NbCases_AC1_StAc <- template
NbCases_AC1_StAs <- template
NbCases_AC1_StCh <- template
NbCases_AC1_StChS <- template
NbCases_AC1_StI <- template

NbCases_AC2_StAc <- template
NbCases_AC2_StAs <- template
NbCases_AC2_StCh <- template
NbCases_AC2_StChS <- template
NbCases_AC2_StI <- template

NbCases_AC3_StAc <- template
NbCases_AC3_StAs <- template
NbCases_AC3_StCh <- template
NbCases_AC3_StChS <- template
NbCases_AC3_StI <- template

NbCases_AC4_StAc <- template
NbCases_AC4_StAs <- template
NbCases_AC4_StCh <- template
NbCases_AC4_StChS <- template
NbCases_AC4_StI <- template

NbCases_AC5_StAc <- template
NbCases_AC5_StAs <- template
NbCases_AC5_StCh <- template
NbCases_AC5_StChS <- template
NbCases_AC5_StI <- template

NbCases_ACt_StAc <- template
NbCases_ACt_StAs <- template
NbCases_ACt_StCh <- template
NbCases_ACt_StChS <- template
NbCases_ACt_StI <- template

###
NbDeath_AC1_StAc <- template
NbDeath_AC1_StAs <- template
NbDeath_AC1_StCh <- template
NbDeath_AC1_StChS <- template
NbDeath_AC1_StI <- template

NbDeath_AC2_StAc <- template
NbDeath_AC2_StAs <- template
NbDeath_AC2_StCh <- template
NbDeath_AC2_StChS <- template
NbDeath_AC2_StI <- template

NbDeath_AC3_StAc <- template
NbDeath_AC3_StAs <- template
NbDeath_AC3_StCh <- template
NbDeath_AC3_StChS <- template
NbDeath_AC3_StI <- template

NbDeath_AC4_StAc <- template
NbDeath_AC4_StAs <- template
NbDeath_AC4_StCh <- template
NbDeath_AC4_StChS <- template
NbDeath_AC4_StI <- template

NbDeath_AC5_StAc <- template
NbDeath_AC5_StAs <- template
NbDeath_AC5_StCh <- template
NbDeath_AC5_StChS <- template
NbDeath_AC5_StI <- template

NbDeath_ACt_StAc <- template
NbDeath_ACt_StAs <- template
NbDeath_ACt_StCh <- template
NbDeath_ACt_StChS <- template
NbDeath_ACt_StI <- template

##
YLL_AC1_StAc <- template
YLL_AC1_StAs <- template
YLL_AC1_StCh <- template
YLL_AC1_StChS <- template
YLL_AC1_StI <- template

YLL_AC2_StAc <- template
YLL_AC2_StAs <- template
YLL_AC2_StCh <- template
YLL_AC2_StChS <- template
YLL_AC2_StI <- template

YLL_AC3_StAc <- template
YLL_AC3_StAs <- template
YLL_AC3_StCh <- template
YLL_AC3_StChS <- template
YLL_AC3_StI <- template

YLL_AC4_StAc <- template
YLL_AC4_StAs <- template
YLL_AC4_StCh <- template
YLL_AC4_StChS <- template
YLL_AC4_StI <- template

YLL_AC5_StAc <- template
YLL_AC5_StAs <- template
YLL_AC5_StCh <- template
YLL_AC5_StChS <- template
YLL_AC5_StI <- template

YLL_ACt_StAc <- template
YLL_ACt_StAs <- template
YLL_ACt_StCh <- template
YLL_ACt_StChS <- template
YLL_ACt_StI <- template

##
YLD_AC1_StAc <- template
YLD_AC1_StAs <- template
YLD_AC1_StCh <- template
YLD_AC1_StChS <- template
YLD_AC1_StI <- template

YLD_AC2_StAc <- template
YLD_AC2_StAs <- template
YLD_AC2_StCh <- template
YLD_AC2_StChS <- template
YLD_AC2_StI <- template

YLD_AC3_StAc <- template
YLD_AC3_StAs <- template
YLD_AC3_StCh <- template
YLD_AC3_StChS <- template
YLD_AC3_StI <- template

YLD_AC4_StAc <- template
YLD_AC4_StAs <- template
YLD_AC4_StCh <- template
YLD_AC4_StChS <- template
YLD_AC4_StI <- template

YLD_AC5_StAc <- template
YLD_AC5_StAs <- template
YLD_AC5_StCh <- template
YLD_AC5_StChS <- template
YLD_AC5_StI <- template

YLD_ACt_StAc <- template
YLD_ACt_StAs <- template
YLD_ACt_StCh <- template
YLD_ACt_StChS <- template
YLD_ACt_StI <- template

##
DALYs_AC1_StAc <- template
DALYs_AC1_StAs <- template
DALYs_AC1_StCh <- template
DALYs_AC1_StChS <- template
DALYs_AC1_StI <- template

DALYs_AC2_StAc <- template
DALYs_AC2_StAs <- template
DALYs_AC2_StCh <- template
DALYs_AC2_StChS <- template
DALYs_AC2_StI <- template

DALYs_AC3_StAc <- template
DALYs_AC3_StAs <- template
DALYs_AC3_StCh <- template
DALYs_AC3_StChS <- template
DALYs_AC3_StI <- template

DALYs_AC4_StAc <- template
DALYs_AC4_StAs <- template
DALYs_AC4_StCh <- template
DALYs_AC4_StChS <- template
DALYs_AC4_StI <- template

DALYs_AC5_StAc <- template
DALYs_AC5_StAs <- template
DALYs_AC5_StCh <- template
DALYs_AC5_StChS <- template
DALYs_AC5_StI <- template

DALYs_ACt_StAc <- template
DALYs_ACt_StAs <- template
DALYs_ACt_StCh <- template
DALYs_ACt_StChS <- template
DALYs_ACt_StI <- template