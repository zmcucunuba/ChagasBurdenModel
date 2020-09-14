### BoD_model2 in C++ using Rcpp
### Julia Ledien 14/09/2020

library(Rcpp)

### to call ythe function created in the R script use cppFunction()
## then write alle the function in C++

## The model output is a list and the parameter input is also a list
## it is better to input vectors

List Rcpp_BurdenModel ( list params){
  # exemple: NumericVector resid = as<NumericVector>(mod["residuals"]);
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
  NumericVector Pop  = as<NumericVector>( params["Pop"]);
  NumericVector Deaths  = as<NumericVector>( params["Deaths"]);
  NumericVector Ny  = as<NumericVector>( params["Ny"]);
  NumericVector Na  = as<NumericVector>( params["Na"]);
  NumericVector iters  = as<NumericVector>( params["iters"]);
  NumericVector lambdaT  = as<NumericVector>( params["lambdaNy"]);
  
  #NumericMatrix 
  #S <- array(NA, dim=c(Na,iters,Ny))    # matrix of number of suceptible per years and age
  NumericVector s = {NA};
  s.attr("dim") = Dimension(Na,iters,Ny);
  NumericMatrix S = as<NumericMatrix>(s);
  
  return dim(S);
}

#NumericMatrix m( 5, 5 );