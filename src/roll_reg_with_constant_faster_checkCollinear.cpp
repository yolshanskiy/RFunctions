#include <Rcpp.h>
#include <RcppEigen.h>

namespace {
  bool isInvertible(Eigen::LDLT<Eigen::MatrixXd> ldlt, double threshold = 1e-8) {
    // get the diagonal elements of the matrix
    Eigen::VectorXd diag = ldlt.vectorD();

    // check if all of the diagonal elements are above the threshold
    return (diag.array().abs() > threshold).all();
  }
}
// [[Rcpp::export]]
Rcpp::List roll_reg_with_constant_faster_checkCollinear(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size) {
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

  // Initialize the rolling sum of x_window.transpose() * x_window
  Eigen::MatrixXd sum_x_transpose_x = Eigen::MatrixXd::Zero(p+1, p+1);

  // Initialize the rolling sum of x_window_transpose * y_window
  Eigen::VectorXd sum_x_transpose_y = Eigen::VectorXd::Zero(p+1);

  // Perform the rolling regression
  for (int i = 0; i <= n - window_size; i++) {
    Eigen::VectorXd y_window = y_eig.segment(i, window_size);
    Eigen::MatrixXd x_window = x_eig_const.block(i, 0, window_size, p + 1);

    // Initialize the rolling sums for the first window
    if (i == 0) {
      sum_x_transpose_x = x_window.transpose() * x_window;
      sum_x_transpose_y = x_window.transpose() * y_window;
    }
    // Add the new row and remove the old row for subsequent windows
    else {
      sum_x_transpose_x += x_eig_const.block(i+window_size-1, 0, 1, p + 1).transpose() * x_eig_const.block(i+window_size-1, 0, 1, p + 1) - x_eig_const.block(i-1, 0, 1, p+1).transpose() * x_eig_const.block(i-1, 0, 1, p+1);
      sum_x_transpose_y += x_eig_const.block(i+window_size-1, 0, 1, p + 1).transpose() * y_eig.segment(i+window_size-1, 1) - x_eig_const.block(i-1, 0, 1, p+1).transpose() * y_eig.segment(i-1, 1);
    }

    // Check if x is collinear
    Eigen::LDLT<Eigen::MatrixXd> ldlt_sum_x_transpose_x(sum_x_transpose_x);

    if ( !isInvertible(ldlt_sum_x_transpose_x) ) {
      coefs.col(i).setConstant(NA_REAL);
      res_var(i) = NA_REAL;
    }
    // Eigen::MatrixXd x_window_rank_check = x_window.block(0, 0, window_size, p);
    // if (x_window_rank_check.fullPivLu().rank() < x_window_rank_check.cols()) {
    //   coefs.col(i).setConstant(NA_REAL);
    //   res_var(i) = NA_REAL;
    // }
    // Perform matrix multiplication using the rolling sum of x_window.transpose() * x_window
    else {
      coefs.col(i) = ldlt_sum_x_transpose_x.solve(sum_x_transpose_y);
      res_var(i) = (y_window - x_window * coefs.col(i)).squaredNorm() / (window_size - p - 1);
    }
  }

  // Return the rolling coefficients and residual variance as a list
  return Rcpp::List::create(Rcpp::Named("coefs") = coefs.transpose(),
                            Rcpp::Named("res_var") = res_var);
}

