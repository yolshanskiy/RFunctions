#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::export]]
Rcpp::List roll_reg_with_constant(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size) {
  // Convert y and x to Eigen objects
  int n = x.nrow();
  int p = x.ncol();
  Eigen::Map<Eigen::VectorXd> y_eig(y.begin(), n);
  Eigen::Map<Eigen::MatrixXd> x_eig(x.begin(), n, p);
  // Add a column of 1s to x_eig for the constant term
  Eigen::MatrixXd x_eig_const(n, p + 1);
  x_eig_const << Eigen::VectorXd::Ones(n), x_eig;
  // Allocate space for the rolling coefficients and residual variance
  Eigen::MatrixXd coefs(p + 1, n - window_size + 1);
  Eigen::VectorXd res_var(n - window_size + 1);
  // Perform the rolling regression
  for (int i = 0; i <= n - window_size; i++) {
    Eigen::VectorXd y_window = y_eig.segment(i, window_size);
    Eigen::MatrixXd x_window = x_eig_const.block(i, 0, window_size, p + 1);
    coefs.col(i) = (x_window.transpose() * x_window).ldlt().solve(x_window.transpose() * y_window);
    res_var(i) = (y_window - x_window * coefs.col(i)).squaredNorm() / (window_size - p - 1);
  }
  // Return the rolling coefficients and residual variance as a list
  return Rcpp::List::create(Rcpp::Named("coefs") = coefs.transpose(), Rcpp::Named("res_var") = res_var);
}

