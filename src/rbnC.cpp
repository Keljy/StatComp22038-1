#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler to generate a bivariate normal chain using Rcpp
//' @description A Gibbs sampler to generate a bivariate normal chain using Rcpp
//' @param N the length of chain
//' @param mu1 Initial parameters
//' @param mu2 Initial parameters
//' @param sigma1 Initial parameters
//' @param sigma2 Initial parameters
//' @param rho Initial parameters
//' @return a a Gibbs sample chain
//' @examples
//' \dontrun{
//' rbnC(1000,0,0,1,1,0.9)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix rbnC(int N, double mu1, double mu2, double sigma1, double sigma2, double rho) {
  NumericMatrix mat(N, 2);
  double x1 = 0, x2 = 0; 
  double m1 = 0, m2 = 0;
  double s1 = sqrt(1-rho*rho)*sigma1;
  double s2 = sqrt(1-rho*rho)*sigma2;
  mat(0, 0) = mu1;
  mat(0, 1) = mu2;
  for(int i = 1; i < N; i++) {
    x2 = mat(i-1, 1);
    m1 = mu1 + rho*(x2-mu2)*sigma1/sigma2;
    mat(i,0) = rnorm(1,m1,s1)[0];
    x1 = mat(i-1,0);
    m2 = mu2 + rho*(x1-mu1)*sigma2/sigma1;
    mat(i,1) = rnorm(1,m2,s2)[0];
  }
  return(mat);
}
