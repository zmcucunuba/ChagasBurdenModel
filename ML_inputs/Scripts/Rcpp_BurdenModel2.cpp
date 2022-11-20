//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
Rcpp::List Rcpp_BurdenModel2(Rcpp::List params){
  NumericVector alpha  = as<NumericVector>( params["alphaD"]);
  NumericVector beta  = as<NumericVector>( params["betaD"]);
  NumericVector delta  = as<NumericVector>( params["deltaD"]);
  NumericVector du_A  = as<NumericVector>( params["du_AD"]);
  NumericVector gamma  = as<NumericVector>( params["gammaD"]);
  NumericVector mu_1  = as<NumericVector>( params["mu_1D"]);
  NumericVector mu_a  = as<NumericVector>( params["mu_aD"]);
  NumericVector mu_m  = as<NumericVector>( params["mu_cmD"]);
  NumericVector mu_s  = as<NumericVector>( params["mu_csD"]);
  NumericVector Pmu_a  = as<NumericVector>( params["Pmu_aD"]);
  NumericVector RRm  = as<NumericVector>( params["RRmD"]);
  NumericVector RRp  = as<NumericVector>( params["RRpD"]);
  NumericVector wam  = as<NumericVector>( params["wamD"]);
  NumericVector was  = as<NumericVector>( params["wasD"]);
  NumericVector wcm  = as<NumericVector>( params["wamD"]);
  NumericVector wcs  = as<NumericVector>( params["wcsD"]);
  //NumericVector Pop  = as<NumericVector>( params["Pop"]);
  //NumericVector Deaths  = as<NumericVector>( params["Deaths"]);
  int Ny  = as<int>( params["Ny"]);
  int Na  = as<int>( params["Na"]);
  int iters  = as<int>( params["iters"]);
  List  lambdaT  = as<List >( params["lambdaNy"]);
  //NumericMatrix lambdaT  = as<NumericMatrix>( params["lambdaNy"]);
  
  // Initial matrices
  IntegerVector v = {Na,iters,Ny};
  
  //// susceptible
  arma::cube S(v[0],v[1],v[2]);    // matrix of number of suceptible per years and age
  arma::cube Sm(v[0],v[1],v[2]);                 // matrix of number of mild suceptible
  arma::cube Ss(v[0],v[1],v[2]);  
  
  ////infected
  arma::cube Am(v[0],v[1],v[2]);                 // matrix of number of infected acute mild
  arma::cube As(v[0],v[1],v[2]);                  // matrix of number of infected acute severe
  arma::cube I(v[0],v[1],v[2]);                  // matrix of number of infected asymptomatic
  arma::cube Cm1(v[0],v[1],v[2]);                 //matrix of number of infected chronic mild 1
  arma::cube Cm2(v[0],v[1],v[2]);                 // matrix of number of infected chronic mild 2
  arma::cube Cs1(v[0],v[1],v[2]);                 // matrix of number of infected chronic severe 1
  arma::cube Cs2(v[0],v[1],v[2]);                  // matrix of number of infected chronic severe 2
  arma::cube Cs3(v[0],v[1],v[2]);                 // matrix of number of infected chronic severe 3
  
  //deaths
  arma::cube Du(v[0],v[1],v[2]);                 // matrix of number of death uninfected
  arma::cube Da(v[0],v[1],v[2]);                 // matrix of number of death acute
  arma::cube Di(v[0],v[1],v[2]);                  // matrix of number of death asymptomatic
  arma::cube Dm(v[0],v[1],v[2]);                 // matrix of number of death in mild/moderate
  arma::cube DmI(v[0],v[1],v[2]);                  // matrix of number of Indirect death mild infected
  arma::cube Ds(v[0],v[1],v[2]);                  // matrix of number of death severe infected
  arma::cube DsI(v[0],v[1],v[2]);                  // matrix of number of Indirect deaths severe infected
  arma::cube DsS(v[0],v[1],v[2]);                  // matrix of number of Indirect deaths severe Susceptible
  arma::cube DmS(v[0],v[1],v[2]);                  // matrix of number of Indirect deaths severe Susceptible
  
  arma::cube xS(v[0],v[1],v[2]); 
  // set 1 values for  S[,,1]
  for (int i = 0; i < v[0]; ++i) {
    for (int j = 0; j < v[1]; ++j) {
      
      S(i,j,0) = 1;
    }
  }
  
  // create initial vectors
  NumericVector rm_S(iters);
  NumericVector rm_Sm(iters);
  NumericVector rm_Ss(iters);
  NumericVector rm_Am(iters);
  NumericVector rm_As(iters);
  NumericVector rm_I(iters);
  NumericVector rm_Cm1(iters);
  NumericVector rm_Cm2(iters);
  NumericVector rm_Cs1(iters);
  NumericVector rm_Cs2(iters);
  NumericVector rm_Cs3(iters);
  
  
  
  for (int i = 1; i < Ny; ++i) {
    for (int j = 0; j < iters; ++j) {
      for (int k = 0; k < Na; ++k) {
        
        //##############################################
        //#              SUSCEPTIBLES          
        //##############################################
        
        
        //# Susceptibles Asymptomatic
        NumericVector lambdaT_t = lambdaT[i];
        rm_S(j) = 1-exp(-(lambdaT_t(j) + mu_1(j) + alpha(j)) );
        S(k,j,i) = S(k,j,(i-1)) - rm_S(j) * S(k,j,(i-1));
        
        //# Susceptibles Mild
        rm_Sm(j) = 1-exp( - (mu_m(j) + beta(j) + lambdaT_t(j)) );
        Sm(k,j,i) = Sm(k,j,(i-1)) - rm_Sm(j) * Sm(k,j,(i-1)) + alpha(j)/(lambdaT_t(j) + mu_1(j) + alpha(j)) * rm_S(j) * S(k,j,(i-1));
        
        //# Susceptibles Severe
        rm_Ss(j) = 1-exp(-(mu_s(j) + lambdaT_t(j)));
        Ss(k,j,i) = Ss(k,j,(i-1)) - rm_Ss(j) * Ss(k,j,(i-1)) + (beta(j)/(mu_m(j) + beta(j) + lambdaT_t(j))) * rm_Sm(j) * Sm(k,j,(i-1));
        
        //##############################################
        //#             INFECTED          
        //##############################################
        
        //#==== Acute Mild =======
        rm_Am(j) = 1-exp(-(mu_1(j) + delta(j)));
        Am(k,j,i)= Am(k,j,(i-1)) - rm_Am(j) * Am(k,j,(i-1)) + (1-gamma(j))*(lambdaT_t(j)/(lambdaT_t(j)+ mu_1(j) + alpha(j)))* rm_S(j) * S(k,j,(i-1));
        
        //#==== Acute Severe=======
        rm_As(j) = 1-exp(-(mu_1(j) + mu_a(j) + delta(j)));
        As(k,j,i) = As(k,j,(i-1)) - rm_As(j) * As(k,j,(i-1)) + gamma(j) * (lambdaT_t(j)/(lambdaT_t(j)+ mu_1(j) + alpha(j))) * rm_S(j) * S(k,j,(i-1));
        
        //#==== Asymptomatic=======
        rm_I(j) = 1-exp(-( mu_1(j) * RRm(j) + alpha(j) *RRp(j)));
        I(k,j,i) = I(k,j,(i-1)) - rm_I(j) * I(k,j,(i-1)) + (delta(j)/ ( mu_1(j) + delta(j)))* rm_Am(j) * Am(k,j,(i-1));
        
        //#====== Chronic Moderate 1  
        rm_Cm1(j) = 1-exp(-(mu_m(j) * RRm(j) + beta(j) * RRp(j))); 
        Cm1(k,j,i) = Cm1(k,j,(i-1)) - rm_Cm1(j) * Cm1(k,j,(i-1)) + (alpha(j))/( mu_1(j) * RRm(j) + alpha(j) *RRp(j))*rm_I(j)*I(k,j,(i-1)) + 
          lambdaT_t(j)/(mu_m(j) + beta(j) + lambdaT_t(j))*   rm_Sm(j) *Sm(k,j,(i-1));
        
        //#====== Chronic Moderate 2   
        rm_Cm2(j) = 1-exp(-(mu_m(j) * RRm(j) + beta(j) *  RRp(j)));
        Cm2(k,j,i) = Cm2(k,j,(i-1)) - rm_Cm2(j) * Cm2(k,j,(i-1)) + (delta(j))/(mu_1(j) + mu_a(j) + (delta(j)))*rm_As(j)*As(k,j,(i-1))+
          (alpha(j) * (RRp(j)-1))/(( mu_1(j) * RRm(j) + alpha(j) *RRp(j))) * rm_I(j) *  I(k,j,(i-1)) ;
        
        //#====== Chronic Severe 1  
        rm_Cs1(j)= 1- exp(-(mu_s(j)  * RRm(j))) ;
        Cs1(k,j,i) = Cs1(k,j,(i-1)) - rm_Cs1(j) * Cs1(k,j,(i-1)) + (beta(j))/(mu_m(j) * RRm(j) + beta(j) * RRp(j)) * rm_Cm1(j) * Cm1(k,j,(i-1))+
          lambdaT_t(j)/(mu_s(j) + lambdaT_t(j)) * rm_Ss(j)* Ss(k,j,(i-1));
        
        
        //#====== Chronic Severe 2 
        rm_Cs2(j) = 1- exp(-(mu_s(j) *RRm(j))) ;
        Cs2(k,j,i) = Cs2(k,j,(i-1)) - rm_Cs2(j) * Cs2(k,j,(i-1)) + (beta(j) * (RRp(j) -1))/(mu_m(j) * 
          RRm(j) + beta(j) * RRp(j)) * rm_Cm1(j) * Cm1(k,j,(i-1));
        
        //#====== Chronic Severe 3
        rm_Cs3(j) = 1- exp(-(mu_s(j) *RRm(j))) ;
        Cs3(k,j,i) = Cs3(k,j,(i-1)) - rm_Cs3(j) * Cs3 (k,j,(i-1)) + (beta(j) * RRp(j))/(mu_m(j) * RRm(j) + beta(j) * RRp(j)) * 
          rm_Cm2(j) * Cm2(k,j,(i-1));
        
        //###################################
        //#           DEATHS         
        //###################################
        
        Da(k,j,i) = mu_a(j) / (mu_1(j) + mu_a(j)+(delta(j)))*rm_As(j)*As(k,j,(i-1));
        
        Di(k,j,i) = (mu_1(j)*(RRm(j)-1))/ ( mu_1(j) * RRm(j) + alpha(j) *RRp(j)) * rm_I(j) * I(k,j,(i-1));
        
        Dm(k,j,i) = (mu_m(j)*(RRm(j)-1)) / (mu_m(j) * RRm(j) + beta(j) * RRp(j)) * rm_Cm1(j) * Cm1(k,j,(i-1)) +
          (mu_m(j)*(RRm(j)-1)) / (mu_m(j) * RRm(j) + beta(j) * RRp(j)) *      rm_Cm2(j) * Cm2(k,j,(i-1));
        
        
        DmI(k,j,i) = (mu_m(j)*(1-(mu_1(j)/mu_m(j)))) / (mu_m(j) * RRm(j) + beta(j) * RRp(j)) *rm_Cm2(j) * Cm2(k,j,(i-1));
        
        
        Ds(k,j,i) = (mu_s(j)*(RRm(j)-1)) /(mu_s(j) *RRm(j)) * rm_Cs1(j) * Cs1(k,j,(i-1))  +
          (mu_s(j)*(RRm(j)-1)) /(mu_s(j) *RRm(j)) * rm_Cs2(j) * Cs2(k,j,(i-1))  +
          (mu_s(j)*(RRm(j)-1)) /(mu_s(j) *RRm(j)) * rm_Cs3(j) * Cs3(k,j,(i-1)) ; 
        
        DsI(k,j,i) = (mu_s(j)*(1-(mu_m(j)/mu_s(j)))) /(mu_s(j) *RRm(j)) * rm_Cs2(j) *  
          Cs2(k,j,(i-1)) + (mu_s(j)*(1-(mu_1(j)/mu_s(j)))) /(mu_s(j) *RRm(j)) * rm_Cs3(j) * Cs3(k,j,(i-1));
        
        Du(k,j,i) = (mu_1(j)/(lambdaT_t(j)+ mu_1(j) + alpha(j))* rm_S(j) * S(k,j,(i-1)) ) +       
          mu_1(j)/(mu_1(j) + delta(j))* rm_Am(j) * Am(k,j,(i-1))   +                         
          mu_1(j)/(mu_1(j) + mu_a(j)+ (delta(j)))*rm_As(j) *As(k,j,(i-1)) +                 
          mu_1(j)/( mu_1(j) * RRm(j) + alpha(j) *RRp(j))*rm_I(j) *I(k,j,(i-1)) ;
        
        
        DmS(k,j,i) =  mu_m(j)/(mu_m(j) * RRm(j) + beta(j)*RRp(j))*rm_Cm1(j) * Cm1(k,j,(i-1)) +          
          (mu_m(j)*(mu_1(j)/mu_m(j)) )/(mu_m(j) * RRm(j) + beta(j) * RRp(j)) * rm_Cm2(j) * Cm2(k,j,(i-1)) +   
          mu_m(j)/(mu_m(j)+beta(j)+lambdaT_t(j)) * rm_Sm(j) *Sm (k,j,(i-1)) ;                             
        
        
        DsS(k,j,i) =  mu_s(j)/(mu_s(j)+lambdaT_t(j)) * rm_Ss(j) * Ss(k,j,(i-1)) +                      
          mu_s(j)/(mu_s(j) *RRm(j)) * rm_Cs1(j) * Cs1(k,j,(i-1)) +                                   
          (mu_s(j)*(mu_m(j)/mu_s(j)))/ (mu_s(j) *RRm(j)) * rm_Cs2(j) * Cs2(k,j,(i-1)) +          
          (mu_s(j)*(mu_1(j)/mu_s(j)))/ (mu_s(j) *RRm(j)) * rm_Cs3(j) * Cs3(k,j,(i-1)) ;  
        
      } 
      
      //###################################
      //#           AGEING
      //###################################
      //arma::mat kS = S.slice(i);
      //arma::vec kS = S.rows(j,i);
      
      for (int k = 1; k < Na; ++k){
        //# moving from age classes
        
        // S(k,j,i) =  kS(k-1,j) ; //# aging of susceptible 
        S((Na-k),j,i) = S((Na-k)-1,j,i);  
        Sm((Na-k),j,i) = Sm((Na-k)-1,j,i);  //# aging of susceptible 
        Ss((Na-k),j,i) = Ss((Na-k)-1,j,i) ; //# aging of susceptible 
        //# infected
        Am((Na-k),j,i) = Am((Na-k)-1,j,i)  ;//# aging of infected
        As((Na-k),j,i) = As((Na-k)-1,j,i)  ;//# aging of infected
        I((Na-k),j,i) = I((Na-k)-1,j,i)  ;//# aging of infected
        Cm1((Na-k),j,i) = Cm1((Na-k)-1,j,i);  //# aging of infected
        Cm2((Na-k),j,i) = Cm2((Na-k)-1,j,i) ; //# aging of infected
        Cs1((Na-k),j,i) = Cs1((Na-k)-1,j,i)  ;//# aging of infected
        Cs2((Na-k),j,i) = Cs2((Na-k)-1,j,i)  ;//# aging of infected
        Cs3((Na-k),j,i) = Cs3((Na-k)-1,j,i)  ;//# aging of infected
        
      }
      S(0,j,i) = 1;
      Sm(0,j,i) = 0;
      Ss(0,j,i) = 0;
      Am(0,j,i) = 0;
      As(0,j,i) = 0;
      I(0,j,i) = 0;
      Cm1(0,j,i) = 0;
      Cm2(0,j,i) = 0;
      Cs1(0,j,i) = 0;
      Cs2(0,j,i) = 0;
      Cs3(0,j,i) = 0 ;
      
    }
  }
  
  arma::cube Cs = Cs1 + Cs2 + Cs3;
  arma::cube Cm = Cm1 + Cm2 ;
  
  arma::cube DSus = DmS + DsS; // not used after
  
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("S") = S, //# Susceptible Asymptomatic for Cardiomyopathy
                                      Rcpp::Named("Sm")  = Sm ,	//# Susceptible Mild Cardiomyopathy
                                      Rcpp::Named("Ss")  = Ss,	//# Susceptible Severe Cardiomyopathy
                                      Rcpp::Named("Am")  = Am,	//# Acute Mild
                                      Rcpp::Named("As")  = As, 	//# Acute Severe
                                      Rcpp::Named("I")   = I,	  //# Infected Asymptomatic
                                      Rcpp::Named("Cm")  = Cm, 	//# Infected Chronic Mild
                                      Rcpp::Named("Cs")  = Cs,	//# Infected Chronic Severe
                                      Rcpp::Named("Da")  = Da, 	//# Deaths Acute
                                      Rcpp::Named("Di")  = Di,	//# Deaths Asymptomatic Infected
                                      Rcpp::Named("Dm")  = Dm,	//# Deaths Mild Infected (Direct)
                                      Rcpp::Named("DmI")= DmI,	//# Deaths Mild Infected (Indirect)
                                      Rcpp::Named("Ds") = Ds,	  //# Deaths Severe Infected (Direct)
                                      Rcpp::Named("DsI") = DsI,	//# Deaths Severe Infected (Indirect)
                                      Rcpp::Named("Du")  = Du,	//# Deaths Other causes (No heart disease related)
                                      Rcpp::Named("DmS") = DmS,	//# Deaths mild du to other heart diseases
                                      Rcpp::Named("DsS") = DsS);	//# Deaths Severe due to other heart diseases);
  return out;
}

