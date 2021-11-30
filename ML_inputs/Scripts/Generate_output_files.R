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
### Chronic mild= StCh
### Chronic Severe = StChS
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
#YLD_AC1_StAs <- template
YLD_AC1_StCh <- template
YLD_AC1_StChS <- template
YLD_AC1_StI <- template

YLD_AC2_StAc <- template
#YLD_AC2_StAs <- template
YLD_AC2_StCh <- template
YLD_AC2_StChS <- template
YLD_AC2_StI <- template

YLD_AC3_StAc <- template
#YLD_AC3_StAs <- template
YLD_AC3_StCh <- template
YLD_AC3_StChS <- template
YLD_AC3_StI <- template

YLD_AC4_StAc <- template
#YLD_AC4_StAs <- template
YLD_AC4_StCh <- template
YLD_AC4_StChS <- template
YLD_AC4_StI <- template

YLD_AC5_StAc <- template
#YLD_AC5_StAs <- template
YLD_AC5_StCh <- template
YLD_AC5_StChS <- template
YLD_AC5_StI <- template

YLD_ACt_StAc <- template
#YLD_ACt_StAs <- template
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


##national level

National_template<- matrix(0,iterdis,length(years))
National_NbCases_AC1_StAc <- National_template
National_NbCases_AC1_StAs <- National_template
National_NbCases_AC1_StCh <- National_template
National_NbCases_AC1_StChS <- National_template
National_NbCases_AC1_StI <- National_template

National_NbCases_AC2_StAc <- National_template
National_NbCases_AC2_StAs <- National_template
National_NbCases_AC2_StCh <- National_template
National_NbCases_AC2_StChS <- National_template
National_NbCases_AC2_StI <- National_template

National_NbCases_AC3_StAc <- National_template
National_NbCases_AC3_StAs <- National_template
National_NbCases_AC3_StCh <- National_template
National_NbCases_AC3_StChS <- National_template
National_NbCases_AC3_StI <- National_template

National_NbCases_AC4_StAc <- National_template
National_NbCases_AC4_StAs <- National_template
National_NbCases_AC4_StCh <- National_template
National_NbCases_AC4_StChS <- National_template
National_NbCases_AC4_StI <- National_template

National_NbCases_AC5_StAc <- National_template
National_NbCases_AC5_StAs <- National_template
National_NbCases_AC5_StCh <- National_template
National_NbCases_AC5_StChS <- National_template
National_NbCases_AC5_StI <- National_template

National_NbCases_ACt_StAc <- National_template
National_NbCases_ACt_StAs <- National_template
National_NbCases_ACt_StCh <- National_template
National_NbCases_ACt_StChS <- National_template
National_NbCases_ACt_StI <- National_template

###
National_NbDeath_AC1_StAc <- National_template
National_NbDeath_AC1_StAs <- National_template
National_NbDeath_AC1_StCh <- National_template
National_NbDeath_AC1_StChS <- National_template
National_NbDeath_AC1_StI <- National_template

National_NbDeath_AC2_StAc <- National_template
National_NbDeath_AC2_StAs <- National_template
National_NbDeath_AC2_StCh <- National_template
National_NbDeath_AC2_StChS <- National_template
National_NbDeath_AC2_StI <- National_template

National_NbDeath_AC3_StAc <- National_template
National_NbDeath_AC3_StAs <- National_template
National_NbDeath_AC3_StCh <- National_template
National_NbDeath_AC3_StChS <- National_template
National_NbDeath_AC3_StI <- National_template

National_NbDeath_AC4_StAc <- National_template
National_NbDeath_AC4_StAs <- National_template
National_NbDeath_AC4_StCh <- National_template
National_NbDeath_AC4_StChS <- National_template
National_NbDeath_AC4_StI <- National_template

National_NbDeath_AC5_StAc <- National_template
National_NbDeath_AC5_StAs <- National_template
National_NbDeath_AC5_StCh <- National_template
National_NbDeath_AC5_StChS <- National_template
National_NbDeath_AC5_StI <- National_template

National_NbDeath_ACt_StAc <- National_template
National_NbDeath_ACt_StAs <- National_template
National_NbDeath_ACt_StCh <- National_template
National_NbDeath_ACt_StChS <- National_template
National_NbDeath_ACt_StI <- National_template

##
National_YLL_AC1_StAc <- National_template
National_YLL_AC1_StAs <- National_template
National_YLL_AC1_StCh <- National_template
National_YLL_AC1_StChS <- National_template
National_YLL_AC1_StI <- National_template

National_YLL_AC2_StAc <- National_template
National_YLL_AC2_StAs <- National_template
National_YLL_AC2_StCh <- National_template
National_YLL_AC2_StChS <- National_template
National_YLL_AC2_StI <- National_template

National_YLL_AC3_StAc <- National_template
National_YLL_AC3_StAs <- National_template
National_YLL_AC3_StCh <- National_template
National_YLL_AC3_StChS <- National_template
National_YLL_AC3_StI <- National_template

National_YLL_AC4_StAc <- National_template
National_YLL_AC4_StAs <- National_template
National_YLL_AC4_StCh <- National_template
National_YLL_AC4_StChS <- National_template
National_YLL_AC4_StI <- National_template

National_YLL_AC5_StAc <- National_template
National_YLL_AC5_StAs <- National_template
National_YLL_AC5_StCh <- National_template
National_YLL_AC5_StChS <- National_template
National_YLL_AC5_StI <- National_template

National_YLL_ACt_StAc <- National_template
National_YLL_ACt_StAs <- National_template
National_YLL_ACt_StCh <- National_template
National_YLL_ACt_StChS <- National_template
National_YLL_ACt_StI <- National_template

##
National_YLD_AC1_StAc <- National_template
#YLD_AC1_StAs <- National_template
National_YLD_AC1_StCh <- National_template
National_YLD_AC1_StChS <- National_template
National_YLD_AC1_StI <- National_template

National_YLD_AC2_StAc <- National_template
#YLD_AC2_StAs <- National_template
National_YLD_AC2_StCh <- National_template
National_YLD_AC2_StChS <- National_template
National_YLD_AC2_StI <- National_template

National_YLD_AC3_StAc <- National_template
#YLD_AC3_StAs <- National_template
National_YLD_AC3_StCh <- National_template
National_YLD_AC3_StChS <- National_template
National_YLD_AC3_StI <- National_template

National_YLD_AC4_StAc <- National_template
#YLD_AC4_StAs <- National_template
National_YLD_AC4_StCh <- National_template
National_YLD_AC4_StChS <- National_template
National_YLD_AC4_StI <- National_template

National_YLD_AC5_StAc <- National_template
#YLD_AC5_StAs <- National_template
National_YLD_AC5_StCh <- National_template
National_YLD_AC5_StChS <- National_template
National_YLD_AC5_StI <- National_template

National_YLD_ACt_StAc <- National_template
#YLD_ACt_StAs <- National_template
National_YLD_ACt_StCh <- National_template
National_YLD_ACt_StChS <- National_template
National_YLD_ACt_StI <- National_template

##
National_DALYs_AC1_StAc <- National_template
National_DALYs_AC1_StAs <- National_template
National_DALYs_AC1_StCh <- National_template
National_DALYs_AC1_StChS <- National_template
National_DALYs_AC1_StI <- National_template

National_DALYs_AC2_StAc <- National_template
National_DALYs_AC2_StAs <- National_template
National_DALYs_AC2_StCh <- National_template
National_DALYs_AC2_StChS <- National_template
National_DALYs_AC2_StI <- National_template

National_DALYs_AC3_StAc <- National_template
National_DALYs_AC3_StAs <- National_template
National_DALYs_AC3_StCh <- National_template
National_DALYs_AC3_StChS <- National_template
National_DALYs_AC3_StI <- National_template

National_DALYs_AC4_StAc <- National_template
National_DALYs_AC4_StAs <- National_template
National_DALYs_AC4_StCh <- National_template
National_DALYs_AC4_StChS <- National_template
National_DALYs_AC4_StI <- National_template

National_DALYs_AC5_StAc <- National_template
National_DALYs_AC5_StAs <- National_template
National_DALYs_AC5_StCh <- National_template
National_DALYs_AC5_StChS <- National_template
National_DALYs_AC5_StI <- National_template

National_DALYs_ACt_StAc <- National_template
National_DALYs_ACt_StAs <- National_template
National_DALYs_ACt_StCh <- National_template
National_DALYs_ACt_StChS <- National_template
National_DALYs_ACt_StI <- National_template

##Dep level
c.names<-1:iterdis
r.names<- unique(dico$NAME_1_GDAM)
m.names<- years
Dep_template<- array(0,dim=c(length(unique(dico$NAME_1_GDAM)),iterdis,length(years)),dimnames = list(r.names, c.names, m.names))

Dep_NbCases_AC1_StAc <- Dep_template
Dep_NbCases_AC1_StAs <- Dep_template
Dep_NbCases_AC1_StCh <- Dep_template
Dep_NbCases_AC1_StChS <- Dep_template
Dep_NbCases_AC1_StI <- Dep_template

Dep_NbCases_AC2_StAc <- Dep_template
Dep_NbCases_AC2_StAs <- Dep_template
Dep_NbCases_AC2_StCh <- Dep_template
Dep_NbCases_AC2_StChS <- Dep_template
Dep_NbCases_AC2_StI <- Dep_template

Dep_NbCases_AC3_StAc <- Dep_template
Dep_NbCases_AC3_StAs <- Dep_template
Dep_NbCases_AC3_StCh <- Dep_template
Dep_NbCases_AC3_StChS <- Dep_template
Dep_NbCases_AC3_StI <- Dep_template

Dep_NbCases_AC4_StAc <- Dep_template
Dep_NbCases_AC4_StAs <- Dep_template
Dep_NbCases_AC4_StCh <- Dep_template
Dep_NbCases_AC4_StChS <- Dep_template
Dep_NbCases_AC4_StI <- Dep_template

Dep_NbCases_AC5_StAc <- Dep_template
Dep_NbCases_AC5_StAs <- Dep_template
Dep_NbCases_AC5_StCh <- Dep_template
Dep_NbCases_AC5_StChS <- Dep_template
Dep_NbCases_AC5_StI <- Dep_template

Dep_NbCases_ACt_StAc <- Dep_template
Dep_NbCases_ACt_StAs <- Dep_template
Dep_NbCases_ACt_StCh <- Dep_template
Dep_NbCases_ACt_StChS <- Dep_template
Dep_NbCases_ACt_StI <- Dep_template

###
Dep_NbDeath_AC1_StAc <- Dep_template
Dep_NbDeath_AC1_StAs <- Dep_template
Dep_NbDeath_AC1_StCh <- Dep_template
Dep_NbDeath_AC1_StChS <- Dep_template
Dep_NbDeath_AC1_StI <- Dep_template

Dep_NbDeath_AC2_StAc <- Dep_template
Dep_NbDeath_AC2_StAs <- Dep_template
Dep_NbDeath_AC2_StCh <- Dep_template
Dep_NbDeath_AC2_StChS <- Dep_template
Dep_NbDeath_AC2_StI <- Dep_template

Dep_NbDeath_AC3_StAc <- Dep_template
Dep_NbDeath_AC3_StAs <- Dep_template
Dep_NbDeath_AC3_StCh <- Dep_template
Dep_NbDeath_AC3_StChS <- Dep_template
Dep_NbDeath_AC3_StI <- Dep_template

Dep_NbDeath_AC4_StAc <- Dep_template
Dep_NbDeath_AC4_StAs <- Dep_template
Dep_NbDeath_AC4_StCh <- Dep_template
Dep_NbDeath_AC4_StChS <- Dep_template
Dep_NbDeath_AC4_StI <- Dep_template

Dep_NbDeath_AC5_StAc <- Dep_template
Dep_NbDeath_AC5_StAs <- Dep_template
Dep_NbDeath_AC5_StCh <- Dep_template
Dep_NbDeath_AC5_StChS <- Dep_template
Dep_NbDeath_AC5_StI <- Dep_template

Dep_NbDeath_ACt_StAc <- Dep_template
Dep_NbDeath_ACt_StAs <- Dep_template
Dep_NbDeath_ACt_StCh <- Dep_template
Dep_NbDeath_ACt_StChS <- Dep_template
Dep_NbDeath_ACt_StI <- Dep_template

##
Dep_YLL_AC1_StAc <- Dep_template
Dep_YLL_AC1_StAs <- Dep_template
Dep_YLL_AC1_StCh <- Dep_template
Dep_YLL_AC1_StChS <- Dep_template
Dep_YLL_AC1_StI <- Dep_template

Dep_YLL_AC2_StAc <- Dep_template
Dep_YLL_AC2_StAs <- Dep_template
Dep_YLL_AC2_StCh <- Dep_template
Dep_YLL_AC2_StChS <- Dep_template
Dep_YLL_AC2_StI <- Dep_template

Dep_YLL_AC3_StAc <- Dep_template
Dep_YLL_AC3_StAs <- Dep_template
Dep_YLL_AC3_StCh <- Dep_template
Dep_YLL_AC3_StChS <- Dep_template
Dep_YLL_AC3_StI <- Dep_template

Dep_YLL_AC4_StAc <- Dep_template
Dep_YLL_AC4_StAs <- Dep_template
Dep_YLL_AC4_StCh <- Dep_template
Dep_YLL_AC4_StChS <- Dep_template
Dep_YLL_AC4_StI <- Dep_template

Dep_YLL_AC5_StAc <- Dep_template
Dep_YLL_AC5_StAs <- Dep_template
Dep_YLL_AC5_StCh <- Dep_template
Dep_YLL_AC5_StChS <- Dep_template
Dep_YLL_AC5_StI <- Dep_template

Dep_YLL_ACt_StAc <- Dep_template
Dep_YLL_ACt_StAs <- Dep_template
Dep_YLL_ACt_StCh <- Dep_template
Dep_YLL_ACt_StChS <- Dep_template
Dep_YLL_ACt_StI <- Dep_template

##
Dep_YLD_AC1_StAc <- Dep_template
#YLD_AC1_StAs <- Dep_template
Dep_YLD_AC1_StCh <- Dep_template
Dep_YLD_AC1_StChS <- Dep_template
Dep_YLD_AC1_StI <- Dep_template

Dep_YLD_AC2_StAc <- Dep_template
#YLD_AC2_StAs <- Dep_template
Dep_YLD_AC2_StCh <- Dep_template
Dep_YLD_AC2_StChS <- Dep_template
Dep_YLD_AC2_StI <- Dep_template

Dep_YLD_AC3_StAc <- Dep_template
#YLD_AC3_StAs <- Dep_template
Dep_YLD_AC3_StCh <- Dep_template
Dep_YLD_AC3_StChS <- Dep_template
Dep_YLD_AC3_StI <- Dep_template

Dep_YLD_AC4_StAc <- Dep_template
#YLD_AC4_StAs <- Dep_template
Dep_YLD_AC4_StCh <- Dep_template
Dep_YLD_AC4_StChS <- Dep_template
Dep_YLD_AC4_StI <- Dep_template

Dep_YLD_AC5_StAc <- Dep_template
#YLD_AC5_StAs <- Dep_template
Dep_YLD_AC5_StCh <- Dep_template
Dep_YLD_AC5_StChS <- Dep_template
Dep_YLD_AC5_StI <- Dep_template

Dep_YLD_ACt_StAc <- Dep_template
#YLD_ACt_StAs <- Dep_template
Dep_YLD_ACt_StCh <- Dep_template
Dep_YLD_ACt_StChS <- Dep_template
Dep_YLD_ACt_StI <- Dep_template

##
Dep_DALYs_AC1_StAc <- Dep_template
Dep_DALYs_AC1_StAs <- Dep_template
Dep_DALYs_AC1_StCh <- Dep_template
Dep_DALYs_AC1_StChS <- Dep_template
Dep_DALYs_AC1_StI <- Dep_template

Dep_DALYs_AC2_StAc <- Dep_template
Dep_DALYs_AC2_StAs <- Dep_template
Dep_DALYs_AC2_StCh <- Dep_template
Dep_DALYs_AC2_StChS <- Dep_template
Dep_DALYs_AC2_StI <- Dep_template

Dep_DALYs_AC3_StAc <- Dep_template
Dep_DALYs_AC3_StAs <- Dep_template
Dep_DALYs_AC3_StCh <- Dep_template
Dep_DALYs_AC3_StChS <- Dep_template
Dep_DALYs_AC3_StI <- Dep_template

Dep_DALYs_AC4_StAc <- Dep_template
Dep_DALYs_AC4_StAs <- Dep_template
Dep_DALYs_AC4_StCh <- Dep_template
Dep_DALYs_AC4_StChS <- Dep_template
Dep_DALYs_AC4_StI <- Dep_template

Dep_DALYs_AC5_StAc <- Dep_template
Dep_DALYs_AC5_StAs <- Dep_template
Dep_DALYs_AC5_StCh <- Dep_template
Dep_DALYs_AC5_StChS <- Dep_template
Dep_DALYs_AC5_StI <- Dep_template

Dep_DALYs_ACt_StAc <- Dep_template
Dep_DALYs_ACt_StAs <- Dep_template
Dep_DALYs_ACt_StCh <- Dep_template
Dep_DALYs_ACt_StChS <- Dep_template
Dep_DALYs_ACt_StI <- Dep_template