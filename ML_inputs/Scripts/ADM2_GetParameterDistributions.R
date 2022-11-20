
ADM2_get_parameter_distribution <- function(lambdaNy, place_name, setting)
{
  
  Pop      <- ADM2_Pop_import_and_format(place_name, setting)
  Deaths   <- ADM2_Deaths_import_and_format(place_name, setting)
  
  print('imported pop params')
  
  iters    <- NROW(lambdaNy)
  Ny       <- NCOL(lambdaNy)   # Number of years
  Na       <- 85      # Age classes

  # iters = 100
  param <- read.csv(file='ML_inputs/Data/pars/Parameters9k.csv') # No Normal
  param$compSD<- (param$compSE*2)
  agemod5  <- seq(5,85,5)
  agemod   <-1:85
  NySim    <- as.numeric(min(names(lambdaNy))): as.numeric(max(names(lambdaNy))) # Years of Symulation
  par(mar=c(2, 2, 2, 2), xpd=F)
  (par(mfrow=c(4,4)))
  
  #Life Expectancy
  LE   <-  read.csv("ML_inputs/Data/dem/life expectancy GBD85.csv")
  LE85 <- LE$life.exp
  LEx5 <-as.matrix(numeric(17))
  
  SEQ<- seq(5, 75, 5)
  LEx5[1]<-  mean(LE85[1:4])
  LEx5[17]<- mean(LE85[80:85])
  for (n in SEQ){
    LEx5[(n/5)+1]<- mean(LE85[n:n+4])
  }
  # plot(LE85)
  # plot(LEx5)
  
  
  ##====================
  
  ##############################################
  ################################################
  #
  #===================== FULL SIMPLE itersRIBUTIONS WITH 90%CI  ===================
  #
  ##########################################
  ###########################################
  
  # General mortality
  PAR<- paste(param$par[1])
  # print(PAR); 
  TITLE<- expression(mu)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  t<-parD
  t0<-median(t)
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL, (realM+(realM*0.1)), realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  min(t)
  realL
  max(t)
  realU
  mu_1D<-t
  rm(PAR, parD, parDi, t)
  
  #----Proportion of mortality among Acute severe cases
  
  PAR =paste(param$par[2])
  # print(PAR); 
  TITLE<- expression(paste('P', mu,'as'))
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  min(t)
  realL
  max(t)
  realU
  Pmu_aD<-t
  rm(PAR, TITLE, parD, parDi, t)
  
  
  #---- "Mortality in Chronic Mild cases
  PAR<- paste(param$par[3])
  # print(PAR); 
  TITLE<- expression(paste(mu,'cm'))
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL, (realM+(realM*0.1)), realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR)
  min(t)
  realL
  max(t)
  realU
  mu_cmD<-t
  rm(PAR, parD, parDi, t)
  
  
  #---- Moprtality in severe cases
  PAR<- paste(param$par[4])
  # print(PAR); 
  TITLE<- expression(paste(mu,'cs'))
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL, (realM+(realM*0.1)), realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR)
  min(t)
  realL
  max(t)
  realU
  mu_csD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  #----RR of mortality due to Chagas
  PAR<- paste(param$par[5])
  # print(PAR); 
  TITLE<- expression(RRm)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  RRmD<-t
  rm(PAR, parD, parDi, t, realL, realU, realM)
  
  
  #---- Gamma == Proportion of severe acute cases (recognisable)
  PAR<- paste(param$par[6])
  # print(PAR); 
  TITLE<- expression(paste(gamma))
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  t<-sapply(1:iters,function(x) median(sample(x=parD,size=5,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  gammaD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  #---- Alpha = Progression from Asymptomatic to Moderate
  PAR<- paste(param$par[8])
  # print(PAR); 
  TITLE<- expression(alpha)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR])))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  #parD<- (rbeta(n=iters,shape1= 6,shape2= 1))/100 # sqweed to the left if  shape1 > than shape 2
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<- parD
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL, realM, (realU+(realU*.02))),
  # col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  alphaD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  
  
  #---- Beta = Progression from mild to severe cardiomyopathy
  
  PAR<- paste(param$par[9])
  # print(PAR); 
  TITLE<- expression(beta)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  parD<- ((rbeta(n=iters,shape1= 6,shape2= 1)/100)*2) # sqweed to the left if  shape1 > than shape 2
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  d<-parD
  t0<-median(d)
  t<- parD
  #t<-sapply(1:iters,function(x) median(sample(x=d,size=10,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  betaD<-t
  rm(PAR, TITLE, parD, t, realL, realM, realU)
  
  
  
  #----RR excess of progression for Chagas disease
  PAR<- paste(param$par[10])
  # print(PAR); 
  TITLE<- expression(RRp)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]))*1)
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp((parDi))}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t<-parD
  t<-sapply(1:iters,function(x) median(sample(x=parD,size=10,replace=TRUE)))
  t0<-median(t)
  hist(t, xlab="", ylab="",col=2, main=TITLE )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  RRpD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  
  
  #---- Duration of Acute phase
  PAR<- paste(param$par[15])
  # print(PAR); 
  TITLE<- expression(epsilon)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR])))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  TITLE<- expression()
  min(t)
  realL
  max(t)
  realU
  du_AD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  ## delta
  deltaD<- (1/du_AD)*53
  TITLE<-expression(delta)
  hist(deltaD, xlab="", ylab="", col=2, main= TITLE) 
  
  #----
  mu_aD<- (Pmu_aD*(deltaD+mu_1D)-mu_1D)/(1-Pmu_aD)
  TITLE<-expression(paste(mu,'a'))
  hist(mu_aD, xlab="", ylab="", col=2, main=TITLE) 
  
  
  #####=====================================================
  #---- Disability weight of mild acute case
  
  PAR<- paste(param$par[11])
  # print(PAR); 
  TITLE<- expression(wam)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR])*.5 ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  wamD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  
  #----Was Disability weight of severe acute case
  PAR <- paste(param$par[12])
  # print(PAR); 
  TITLE<- expression(was)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR]) ))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #        col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  min(t)
  realL
  max(t)
  realU
  wasD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  #---- Disability weight of mild chronic heart dissease
  PAR<- paste(param$par[13])
  # print(PAR); 
  TITLE<- expression(wcm)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR])))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); TITLE<- expression()
  min(t)
  realL
  max(t)
  realU
  wcmD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  
  
  #---- Wcs Disability weight of severe chronic heart dissease
  PAR<- paste(param$par[14])
  # print(PAR); 
  TITLE<- expression(wcs)
  parDi<- (rnorm(iters, mean= ( param$compM[which=param$par==PAR] ) ,
                 sd=(param$compSD[which=param$par==PAR])))
  if (param$normal[which=param$par==PAR]==1) {
    parD<- parDi} else { parD<- exp(parDi)}
  realL<- param$lower[which=param$par==PAR]
  realM<- param$mean[which=param$par==PAR]
  realU<- param$upper[which=param$par==PAR]
  t0<-median(parD)
  t<-parD
  #t<-sapply(1:iters,function(x) median(sample(x=parD,size=5000,replace=TRUE)))
  hist(t, main=TITLE, xlab="", ylab="",col=2 )
  ##abline(v=c(realL,  (realM+(realM*0.1)),realU),
  #      col=c("green", "darkgreen","green"), lwd=3)
  #abline(v=t0,col="orange",lwd=3)
  #legend("topright", c("Median (real)","95%CI (real)", "Median (simulated)"),
  #fill=c("darkgreen", "green", "orange"), horiz=F, bty="n", border=F)
  # print(PAR); 
  TITLE<- expression()
  min(t)
  realL
  max(t)
  realU
  wcsD<-t
  rm(PAR, TITLE, parD, parDi, t, realL, realM, realU)
  
  
  quantile(mu_aD, c(0.025, 0.5, 0.975))
  
  params <- list (alphaD  = alphaD,
                  betaD   = betaD,
                  deltaD  = deltaD,
                  du_AD   = du_AD,
                  gammaD  = gammaD,
                  mu_1D   = mu_1D,
                  mu_aD   = mu_aD,
                  mu_cmD  = mu_cmD,
                  mu_csD  = mu_csD,
                  Pmu_aD  = Pmu_aD,
                  RRmD    = RRmD,
                  RRpD    = RRpD,
                  wamD    = wamD,
                  wasD    = wasD,
                  wcmD    = wamD,
                  wcsD    = wcsD,
                  Pop     = Pop,
                  Deaths  = Deaths,
                  Ny      = Ny,
                  Na      = Na,
                  iters   = iters,
                  lambdaNy = lambdaNy,
                  place_name = place_name,
                  setting = setting
                  
  )
  
  print(data.frame(names(params)))
  
  
  return(params)
  
  
}


