//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
Rcpp::List Rcpp_BurdenModel2(Rcpp::List params){
  // exemple: NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector alpha  = as<NumericVector>( params["alphaD"]);
  // NumericVector beta  = as<NumericVector>( params["betaD"]);
  // NumericVector delta  = as<NumericVector>( params["deltaD"]);
  // NumericVector du_A  = as<NumericVector>( params["du_AD"]);
  // NumericVector gamma  = as<NumericVector>( params["gammaD"]);
  // NumericVector mu_1  = as<NumericVector>( params["mu_1D"]);
  // NumericVector mu_a  = as<NumericVector>( params["mu_aD"]);
  // NumericVector mu_m  = as<NumericVector>( params["mu_cmD"]);
  // NumericVector mu_s  = as<NumericVector>( params["mu_csD"]);
  // NumericVector Pmu_a  = as<NumericVector>( params["Pmu_aD"]);
  // NumericVector RRm  = as<NumericVector>( params["RRmD"]);
  // NumericVector RRp  = as<NumericVector>( params["RRpD"]);
  // NumericVector wam  = as<NumericVector>( params["wamD"]);
  // NumericVector was  = as<NumericVector>( params["wasD"]);
  // NumericVector wcm  = as<NumericVector>( params["wamD"]);
  // NumericVector wcs  = as<NumericVector>( params["wcsD"]);
  // NumericVector Pop  = as<NumericVector>( params["Pop"]);
  // NumericVector Deaths  = as<NumericVector>( params["Deaths"]);
  // NumericVector Ny  = as<NumericVector>( params["Ny"]);
  // NumericVector Na  = as<NumericVector>( params["Na"]);
  // NumericVector iters  = as<NumericVector>( params["iters"]);
  // NumericVector lambdaT  = as<NumericVector>( params["lambdaNy"]);
  
  //NumericMatrix 
  //S <- array(NA, dim=c(Na,iters,Ny))    # matrix of number of suceptible per years and age
  IntegerVector v = {10,20,3}; 
  arma::cube S(v[0],v[1],v[2]);
  arma::cube I(v[0],v[1],v[2]);
  // arma::cube S = randu<cube>(v[0],v[1],v[2]);
  
  for (int i = 0; i < v[0]; ++i) {
    for (int j = 0; j < v[1]; ++j) {
      for (int k = 0; k < v[2]; ++k) {
        S(i,j,k) = i+j*10;
        I(i,j,k) = i+j*10 + 1e3;
      }
    }
  }
  
  
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("postS") = S,
                                      Rcpp::Named("postI") = I);
  return out;
}

//#NumericMatrix m( 5, 5 );