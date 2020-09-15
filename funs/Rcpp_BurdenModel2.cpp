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
  NumericMatrix lambdaT  = as<NumericMatrix>( params["lambdaNy"]);
  
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
  
  
  // set 1 values for  S[,,1]
  for (int i = 0; i < v[0]; ++i) {
    for (int j = 0; j < v[1]; ++j) {
      
        S(i,j,1) = 1;
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
      //NumericVector lambdaT_t = as<NumericVector>(lambdaT.column(i)) ;
      rm_S(j) = 1-exp(-(lambdaT(j,i) + mu_1(j) + alpha(j)) );
      S(k,j,i) = S(k,j,(i-1)) - rm_S(j) * S(k,j,(i-1));
      
       
      }
    }
  }
  
  
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("postS") = S,
                                      Rcpp::Named("postI") = rm_S,
                                      Rcpp::Named ("test")= rm_Cs3,
                                      Rcpp::Named ("test2")= DmS);
  return out;
}

