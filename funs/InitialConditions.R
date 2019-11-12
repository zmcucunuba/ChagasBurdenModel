

# susceptible
S <- array(NA, dim=c(Na,iters,Ny))    # matrix of number of suceptible per years and age
Sm <- S                  # matrix of number of mild suceptible
Ss <- S     


#infected
Am <- S                  # matrix of number of infected acute mild
As <- S                  # matrix of number of infected acute severe
I  <- S                   # matrix of number of infected asymptomatic
Cm1 <- S                  # matrix of number of infected chronic mild 1
Cm2 <- S                  # matrix of number of infected chronic mild 2
Cs1 <- S                  # matrix of number of infected chronic severe 1
Cs2 <- S                  # matrix of number of infected chronic severe 2
Cs3 <- S                  # matrix of number of infected chronic severe 3

# deaths
Du <- S                  # matrix of number of death uninfected
Da <- S                  # matrix of number of death acute
Di <- S                  # matrix of number of death asymptomatic
Dm <- S                  # matrix of number of death in mild/moderate
DmI <- S                  # matrix of number of Indirect death mild infected
Ds <- S                  # matrix of number of death severe infected
DsI <- S                  # matrix of number of Indirect deaths severe infected
DsS <- S                  # matrix of number of Indirect deaths severe Susceptible
DmS <- S                  # matrix of number of Indirect deaths severe Susceptible




#Susceptible
 S[,,1] <- 1
Sm[,,1] <- 0                  # matrix of number of mild suceptible
Ss[,,1] <- 0                  # matrix of number of mild suceptible


#Infected
 Am[,,1] <- 0                  # matrix of number of infected acute mild
 As[,,1] <- 0                  # matrix of number of infected acute severe
  I[,,1] <- 0                   # matrix of number of infected asymptomatic
Cm1[,,1] <- 0                  # matrix of number of infected chronic mild
Cm2[,,1] <- 0                  # matrix of number of infected chronic mild
Cs1[,,1] <- 0                  # matrix of number of infected chronic
Cs2[,,1] <- 0                  # matrix of number of infected chronic
Cs3[,,1] <- 0                  # matrix of number of infected chronic


#Deaths
 Du[,,1] <- 0                  # matrix of number of death uninfected
 Da[,,1] <- 0                 # matrix of number of death acute
 Di[,,1] <- 0                 # matrix of number of death asymptomatic
 Dm[,,1] <- 0                 # matrix of number of death mild infected
DmI[,,1] <- 0                 # matrix of number of death mild infected
 Ds[,,1] <- 0  
DsI[,,1] <- 0                 # matrix of number of death mild infected
DmS[,,1] <- 0                 # matrix of number of death mild in Susceptible
DsS[,,1] <- 0                 # matrix of number of death severe in Susceptible 

rm.S  <- numeric(iters)
rm.Sm <- numeric(iters)
rm.Ss <- numeric(iters)
rm.Am <- numeric(iters)
rm.As <- numeric(iters)
rm.I  <- numeric(iters)
rm.Cm1<- numeric(iters)
rm.Cm2<- numeric(iters)
rm.Cs1<- numeric(iters)
rm.Cs2<- numeric(iters)
rm.Cs3<- numeric(iters)
