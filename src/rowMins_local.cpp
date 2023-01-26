// Find the index of a minimum element per matrix row
//    returns integer vector                                                  
// The first function just replicates the function from package Rfast (changing numeric vector to integer).
// I add it here because Rfast is not compatible with some versions of C++
// The second is robust to missed elements in the matrix.

#include <Rcpp.h>
#include <RcppEigen.h>


// [[Rcpp::export]]
Eigen::VectorXi rowMins_local(Rcpp::NumericMatrix x){
  const int p = x.nrow();
  Eigen::MatrixXd X = Eigen::Map<Eigen::MatrixXd>(x.begin(), p, x.ncol());
  Eigen::VectorXi F(p);
  int index;
  for(int i = 0; i < p; i++){
    (X.row(i)).minCoeff(&index);
    F(i) = index + 1;
  }
  return F;
}

// #include <RcppArmadillo.h>
// 
// using namespace Rcpp;
// using namespace arma;
// 
// // [[Rcpp::export]]
// Rcpp::IntegerVector rowMins_local(Rcpp::NumericMatrix x){
//   const unsigned int p=x.nrow();
//   arma::mat X = arma::mat(x.begin(), p, x.ncol(), false); 
//   Rcpp::IntegerVector F(p);
//   Rcpp::IntegerVector::iterator FF=F.begin();
//   for(unsigned int i=0;i<p;++i,++FF)
//     *FF=(X.row(i)).index_min()+1;
//   return F;
// }

// // [[Rcpp::export]]
// Rcpp::IntegerVector rowMins_narobust(Rcpp::NumericMatrix x){
//   const unsigned int p=x.nrow();
//   arma::mat X = arma::mat(x.begin(), p, x.ncol(), false); 
//   Rcpp::IntegerVector F(p);
//   Rcpp::IntegerVector::iterator FF=F.begin();
//   for(unsigned int i=0;i<p;++i,++FF) {
//     arma::rowvec row = X.row(i);
//     arma::uvec na_idx = arma::find_nonfinite(row);
//     if(na_idx.n_elem == row.n_elem) {
//       *FF = NA_INTEGER;
//     } else {
//       row.elem(na_idx).fill(arma::datum::inf);
//       *FF=row.index_min()+1;
//     }
//   }
//   return F;
// }
