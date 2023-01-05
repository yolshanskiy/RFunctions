#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

// [[Rcpp::export]]
double checkCollinearity(Eigen::MatrixXd X) {
  Eigen::LDLT<Eigen::MatrixXd> ldlt(X.transpose() * X);
  double sum_X_transpose_X = ldlt.vectorD().array().square().sum();
  double avg_abs_diag = X.diagonal().array().abs().mean();
  double cond_num = sqrt(sum_X_transpose_X / avg_abs_diag);
  return cond_num;
}

/*** R
# create a diagonal matrix
X <- diag(1:30)

# compute the condition number
cond_num <- checkCollinearity(X)
print(cond_num)

***/
