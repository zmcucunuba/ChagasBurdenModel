# BOD Chagas Model

BurdenModel2<- function(params)
{
  
  alpha   <-  params$alphaD
  beta    <-  params$betaD
  delta   <-  params$deltaD
  du_A    <-  params$du_AD
  gamma   <-  params$gammaD
  mu_1    <-  params$mu_1D
  mu_a    <-  params$mu_aD
  mu_m    <-  params$mu_cmD
  mu_s    <-  params$mu_csD
  Pmu_a   <-  params$Pmu_aD
  RRm     <-  params$RRmD
  RRp     <-  params$RRpD
  wam     <-  params$wamD
  was     <-  params$wasD
  wcm     <-  params$wamD
  wcs     <-  params$wcsD
  Pop     <-  params$Pop
  Deaths  <-  params$Deaths
  Ny      <-  params$Ny
  Na      <-  params$Na
  iters   <-  params$iters
  lambdaT <-  params$lambdaNy

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
  
  
  for (i in 2:Ny){ 
    print(i)
    for (j in 1:iters){  
      #print(paste(c("Iter ", j)))
      
      ##############################################
      #              SUSCEPTIBLES          
      ##############################################
      
      
      # Susceptibles Asymptomatic
      rm.S[j] <- 1-exp(-(lambdaT[j,i] + mu_1[j] + alpha[j]) )
      S[,j,i] <- S[,j,(i-1)] - rm.S[j] * S[,j,(i-1)]
      
      # Susceptibles Mild
      rm.Sm[j] <- 1-exp( - (mu_m[j] + beta[j] + lambdaT[j,i]) )
      Sm[,j,i] <- Sm[,j,(i-1)] - rm.Sm[j] * Sm[,j,(i-1)] + 
        alpha[j]/(lambdaT[j,i] + mu_1[j] + alpha[j]) * 
        rm.S[j] * S[,j,(i-1)]
      
      # Susceptibles Severe
      rm.Ss[j] <- 1-exp(-(mu_s[j] + lambdaT[j,i]))
      Ss[,j,i] <- Ss[,j,(i-1)] - rm.Ss[j] * Ss[,j,(i-1)] + 
        (beta[j]/(mu_m[j] + beta[j] + lambdaT[j,i])) * rm.Sm[j] * Sm[,j,(i-1)]
      
      ##############################################
      #             INFECTED          
      ##############################################
      
      #==== Acute Mild =======
      rm.Am[j]<- 1-exp(-(mu_1[j] + delta[j]))
      Am[,j,i]<- Am[,j,(i-1)] - rm.Am[j] * Am[,j,(i-1)] + 
        (1-gamma[j])*(lambdaT[j,i]/(lambdaT[j,i]+ mu_1[j] + alpha[j]))*
        rm.S[j] * S[,j,(i-1)]
      
      #==== Acute Severe=======
      rm.As[j]<- 1-exp(-(mu_1[j] + mu_a[j] + delta[j]))
      As[,j,i] <- As[,j,(i-1)] - rm.As[j] * As[,j,(i-1)] + 
        gamma[j] * (lambdaT[j,i]/(lambdaT[j,i]+ mu_1[j] + alpha[j])) * 
        rm.S[j] * S[,j,(i-1)]
      
      #==== Asymptomatic=======
      rm.I[j]<- 1-exp(-( mu_1[j] * RRm[j] + alpha[j] *RRp[j]))
      I[,j,i] <- I[,j,(i-1)] - rm.I[j] * I[,j,(i-1)] + 
        (delta[j]/ ( mu_1[j] + delta[j]))* rm.Am[j] * Am[,j,(i-1)]
      
      #====== Chronic Moderate 1  
      rm.Cm1[j] <- 1-exp(-(mu_m[j] * RRm[j] + beta[j] * RRp[j])) 
      Cm1[,j,i] <- Cm1[,j,(i-1)] - rm.Cm1[j] * Cm1 [,j,(i-1)] + 
        (alpha[j])/( mu_1[j] * RRm[j] + alpha[j] *RRp[j])*rm.I[j]*I[,j,(i-1)] + 
        lambdaT[j,i]/(mu_m[j] + beta[j] + lambdaT[j,i])*   rm.Sm[j] *Sm[,j,(i-1)]
      
      #====== Chronic Moderate 2   
      rm.Cm2[j] <- 1-exp(-(mu_m[j] * RRm[j] + beta[j] *  RRp[j]))
      Cm2[,j,i] <- Cm2[,j,(i-1)] - rm.Cm2[j] * Cm2[,j,(i-1)] + 
        (delta[j])/(mu_1[j] + mu_a[j] + (delta[j]))*rm.As[j]*As[,j,(i-1)]+
        (alpha[j] * (RRp[j]-1))/(( mu_1[j] * RRm[j] + alpha[j] *RRp[j])) * rm.I[j] *  I[,j,(i-1)] 
      
      #====== Chronic Severe 1  
      rm.Cs1[j]<- 1- exp(-(mu_s[j]  * RRm[j])) 
      Cs1[,j,i] <- Cs1[,j,(i-1)] - rm.Cs1[j] * Cs1[,j,(i-1)] + 
        (beta[j])/(mu_m[j] * RRm[j] + beta[j] * RRp[j]) * rm.Cm1[j] * Cm1[,j,(i-1)]+
        lambdaT[j,i]/(mu_s[j] + lambdaT[j,i]) * rm.Ss[j]* Ss[,j,(i-1)]
      
      
      #====== Chronic Severe 2 
      rm.Cs2[j]<- 1- exp(-(mu_s[j] *RRm[j])) 
      Cs2[,j,i] <- Cs2[,j,(i-1)] - rm.Cs2[j] * Cs2[,j,(i-1)] + 
        (beta[j] * (RRp[j] -1))/(mu_m[j] * RRm[j] + beta[j] * RRp[j]) * rm.Cm1[j] * Cm1[,j,(i-1)]
      
      #====== Chronic Severe 3
      rm.Cs3[j]<- 1- exp(-(mu_s[j] *RRm[j])) 
      Cs3[,j,i] <- Cs3[,j,(i-1)] - rm.Cs3[j] * Cs3 [,j,(i-1)] + 
        (beta[j] * RRp[j])/(mu_m[j] * RRm[j] + beta[j] * RRp[j]) * rm.Cm2[j] * Cm2[,j,(i-1)]
      
      
      
      ###################################
      #           DEATHS         
      ###################################
      
      Da[,j,i] <- mu_a[j] / (mu_1[j] + mu_a[j]+(delta[j]))*rm.As[j]*As[,j,(i-1)]
      
      Di[,j,i] <- (mu_1[j]*(RRm[j]-1))/ ( mu_1[j] * RRm[j] + alpha[j] *RRp[j]) * 
        rm.I[j] * I[,j,(i-1)]
      
      Dm[,j,i] <- (mu_m[j]*(RRm[j]-1)) / (mu_m[j] * RRm[j] + beta[j] * RRp[j]) * rm.Cm1[j] * Cm1[,j,(i-1)] +
        (mu_m[j]*(RRm[j]-1)) / (mu_m[j] * RRm[j] + beta[j] * RRp[j]) *      rm.Cm2[j] * Cm2[,j,(i-1)]
      
      
      DmI[,j,i] <- (mu_m[j]*(1-(mu_1[j]/mu_m[j]))) / (mu_m[j] * RRm[j] + beta[j] * RRp[j]) *rm.Cm2[j] * Cm2[,j,(i-1)]
      
      
      Ds[,j,i] <- (mu_s[j]*(RRm[j]-1)) /(mu_s[j] *RRm[j]) * rm.Cs1[j] * Cs1[,j,(i-1)]  +
        (mu_s[j]*(RRm[j]-1)) /(mu_s[j] *RRm[j]) * rm.Cs2[j] * Cs2[,j,(i-1)]  +
        (mu_s[j]*(RRm[j]-1)) /(mu_s[j] *RRm[j]) * rm.Cs3[j] * Cs3[,j,(i-1)]  
      
      DsI[,j,i] <- (mu_s[j]*(1-(mu_m[j]/mu_s[j]))) /(mu_s[j] *RRm[j]) * rm.Cs2[j] *  
        Cs2[,j,(i-1)] + (mu_s[j]*(1-(mu_1[j]/mu_s[j]))) /(mu_s[j] *RRm[j]) * rm.Cs3[j] * Cs3[,j,(i-1)]
      
      Du[,j,i] <- (mu_1[j]/(lambdaT[j,i]+ mu_1[j] + alpha[j])* rm.S[j] * S[,j,(i-1)] ) +       
        mu_1[j]/(mu_1[j] + delta[j])* rm.Am[j] * Am[,j,(i-1)]   +                         
        mu_1[j]/(mu_1[j] + mu_a[j]+ (delta[j]))*rm.As[j] *As[,j,(i-1)] +                 
        mu_1[j]/( mu_1[j] * RRm[j] + alpha[j] *RRp[j])*rm.I[j] *I[,j,(i-1)] 
      
      
      DmS[,j,i] <-  mu_m[j]/(mu_m[j] * RRm[j] + beta[j]*RRp[j])*rm.Cm1[j] * Cm1[,j,(i-1)] +          
        (mu_m[j]*(mu_1[j]/mu_m[j]) )/(mu_m[j] * RRm[j] + beta[j] * RRp[j]) * rm.Cm2[j] * Cm2[,j,(i-1)] +   
        mu_m[j]/(mu_m[j]+beta[j]+lambdaT[j,i]) * rm.Sm[j] *Sm [,j,(i-1)]                              
      
      
      DsS[,j,i] <-  mu_s[j]/(mu_s[j]+lambdaT[j,i]) * rm.Ss[j] * Ss[,j,(i-1)] +                      
        mu_s[j]/(mu_s[j] *RRm[j]) * rm.Cs1[j] * Cs1[,j,(i-1)] +                                   
        (mu_s[j]*(mu_m[j]/mu_s[j]))/ (mu_s[j] *RRm[j]) * rm.Cs2[j] * Cs2[,j,(i-1)] +          
        (mu_s[j]*(mu_1[j]/mu_s[j]))/ (mu_s[j] *RRm[j]) * rm.Cs3[j] * Cs3[,j,(i-1)]   
      
      ###################################
      #           TOTAL POPULATION
      ###################################
      
      
      Tot <-   
        S[,j,i] +  Sm[,j,i] + Ss[,j,i] + 
        Am[,j,i] +   As[,j,i] +  I[,j,i] + 
        Cm1[,j,i] + Cm2[,j,i] + 
        Cs1[,j,i] + Cs2[,j,i] + Cs3[,j,i]+
        Du[,j,i] +  Da[,j,i] +  Di[,j,i] + 
        Dm[,j,i] +  Ds[,j,i] +  
        DmI[,j,i] + DsI[,j,i] + 
        DmS[,j,i] + DsS[,j,i]
      
      Tot_old <- S[,j,(i-1)] +  Sm[,j,(i-1)] + Ss[,j,(i-1)] + 
        Am[,j,(i-1)] +  As[,j,(i-1)] + I[,j,(i-1)]  +
        Cm1[,j,(i-1)] + Cm2[,j,(i-1)] +
        Cs1[,j,(i-1)] + Cs2[,j,(i-1)] + Cs3[,j,(i-1)] 
      
      
      ###################################
      #           FINAL CHECKING POINT
      ###################################
      
      
      #print(if(round(sum(Tot),digits = 10)!= round(sum(Tot_old),digits = 10)) {warning('MISSING INIIVUALS!!!')} else {"OK!"})
      #print(paste0(c("===== Year=======", names(lambdaT)[i])))
      
      
      ###################################
      #           AGEING
      ###################################
      
      for (k in 2:Na){
        # moving from age classes
        S[k,j,i] <- S[k-1,j,i]  # aging of susceptible 
        Sm[k,j,i] <- Sm[k-1,j,i]  # aging of susceptible 
        Ss[k,j,i] <- Ss[k-1,j,i]  # aging of susceptible 
        # infected
        Am[k,j,i] <- Am[k-1,j,i]  # aging of infected
        As[k,j,i] <- As[k-1,j,i]  # aging of infected
        I[k,j,i] <- I[k-1,j,i]  # aging of infected
        Cm1[k,j,i] <- Cm1[k-1,j,i]  # aging of infected
        Cm2[k,j,i] <- Cm2[k-1,j,i]  # aging of infected
        Cs1[k,j,i] <- Cs1[k-1,j,i]  # aging of infected
        Cs2[k,j,i] <- Cs2[k-1,j,i]  # aging of infected
        Cs3[k,j,i] <- Cs3[k-1,j,i]  # aging of infected
        
      }
      S[1,j,i] <- 1
      Sm[1,j,i] <- 0
      Ss[1,j,i] <- 0
      Am[1,j,i] <- 0
      As[1,j,i] <- 0
      I[1,j,i] <- 0
      Cm1[1,j,i] <- 0
      Cm2[1,j,i] <- 0
      Cs1[1,j,i] <- 0
      Cs2[1,j,i] <- 0
      Cs3[1,j,i] <- 0 
      
      
    }
    
    
  }
  
  Cs<- Cs1 + Cs2 + Cs3
  Cm<- Cm1 + Cm2 
  
  DSus<- DmS + DsS
  ###################################
  #           OUTPUT 
  ###################################
  
  output <- list(
    S   = S, 	# Susceptible Asymptomatic for Cardiomyopathy
    Sm  = Sm ,	# Susceptible Mild Cardiomyopathy
    Ss  = Ss,	# Susceptible Severe Cardiomyopathy
    Am  = Am,	# Acute Mild
    As  = As, 	# Acute Severe
    I   = I,	# Infected Asymptomatic
    Cm  = Cm, 	# Infected Chronic Mild
    Cs  = Cs,	# Infected Chronic Severe
    Da  = Da, 	# Deaths Acute
    Di  = Di,	# Deaths Asymptomatic Infected
    Dm  = Dm,	# Deaths Mild Infected (Direct)
    DmI = DmI,	# Deaths Mild Infected (Indirect)
    Ds  = Ds,	# Deaths Severe Infected (Direct)
    DsI = DsI,	# Deaths Severe Infected (Indirect)
    Du  = Du,	# Deaths Other causes (No heart disease related)
    DmS = DmS,	# Deaths mild du to other heart diseases
    DsS = DsS	# Deaths Severe due to other heart diseases
  )
  
  return(output)
}






