#include <Rcpp.h>
#include <RcppEigen.h>

namespace {
  Rcpp::NumericMatrix create_x_lagged_local(Rcpp::NumericVector x, int p, bool flag_keep_x = true) {
    int n = x.size();
    if(flag_keep_x == true){
      Rcpp::NumericMatrix x_lagged(n, p+1);
      for (int i = 0; i <= p; i++) {
        for (int j = 0; j < n; j++) {
          if (j >= i) {
            x_lagged(j, i) = x(j - i);
          } else {
            x_lagged(j, i) = NA_REAL;
          }
        }
      }
      return x_lagged;
    }
    Rcpp::NumericMatrix x_lagged(n, p);
    for (int i = 1; i <= p; i++) {
      for (int j = 0; j < n; j++) {
        if (j >= i) {
          x_lagged(j, i-1) = x(j - i);
        } else {
          x_lagged(j, i-1) = NA_REAL;
        }
      }
    }
    return x_lagged;
  }

  bool isInvertible(Eigen::LDLT<Eigen::MatrixXd> ldlt, double threshold = 1e-8) {
    // get the diagonal elements of the matrix
    Eigen::VectorXd diag = ldlt.vectorD();

    // check if all of the diagonal elements are above the threshold
    return (diag.array().abs() > threshold).all();
  }
}
// [[Rcpp::export]]
Rcpp::List roll_AR_checkCollinear(Rcpp::NumericVector y, Rcpp::NumericVector x, int p, int window_size) {
  // Convert y and x to Eigen objects
  int n = y.size();

  // Use the create_x_lagged_local function to create the x_lagged matrix
  Rcpp::NumericMatrix x_lagged = create_x_lagged_local(x, p, false);

  // Convert x_lagged to an Eigen object
  Eigen::Map<Eigen::MatrixXd> x_lagged_eig(x_lagged.begin(), n, p);
  Eigen::Map<Eigen::VectorXd> y_eig(y.begin(), n);

  // Add a column of 1s to x_lagged_eig for the constant term
  Eigen::MatrixXd x_lagged_eig_const(n, p + 1);
  x_lagged_eig_const << Eigen::VectorXd::Ones(n), x_lagged_eig;
  // Allocate space for the rolling coefficients and residual variance
  Eigen::MatrixXd coefs(p + 1, n - window_size + 1);
  Eigen::VectorXd res_var(n - window_size + 1);

  // Initialize the rolling sum of x_window.transpose() * x_window
  Eigen::MatrixXd sum_x_transpose_x = Eigen::MatrixXd::Zero(p+1, p+1);

  // Initialize the rolling sum of x_window_transpose * y_window
  Eigen::VectorXd sum_x_transpose_y = Eigen::VectorXd::Zero(p+1);

  // Miss first p regressions to avoid cases when lags are not defined
  for (int i = 0; i <= p - 1; i++) {
    coefs.col(i).setConstant(NA_REAL);
    res_var(i) = NA_REAL;
  }

  // Perform the rolling regression
  for (int i = p - 1; i <= n - window_size; i++) {
    Eigen::VectorXd y_window = y_eig.segment(i, window_size);
    Eigen::MatrixXd x_window = x_lagged_eig_const.block(i, 0, window_size, p + 1);

    // Initialize the rolling sums for the first window
    if (i == p) {
      sum_x_transpose_x = x_window.transpose() * x_window;
      sum_x_transpose_y = x_window.transpose() * y_window;
    }
    // Add the new row and remove the old row for subsequent windows
    else {
      sum_x_transpose_x += x_lagged_eig_const.block(i+window_size-1, 0, 1, p + 1).transpose() * x_lagged_eig_const.block(i+window_size-1, 0, 1, p + 1) - x_lagged_eig_const.block(i-1, 0, 1, p+1).transpose() * x_lagged_eig_const.block(i-1, 0, 1, p+1);
      sum_x_transpose_y += x_lagged_eig_const.block(i+window_size-1, 0, 1, p + 1).transpose() * y_eig.segment(i+window_size-1, 1) - x_lagged_eig_const.block(i-1, 0, 1, p+1).transpose() * y_eig.segment(i-1, 1);
    }

    // Check if x is collinear
    Eigen::LDLT<Eigen::MatrixXd> ldlt_sum_x_transpose_x(sum_x_transpose_x);

    if ( !isInvertible(ldlt_sum_x_transpose_x) ) {
      coefs.col(i).setConstant(NA_REAL);
      res_var(i) = NA_REAL;
    }

    // Eigen::LDLT<Eigen::MatrixXd> ldlt_sum_x_transpose_x(sum_x_transpose_x);
    // double cond_num = ldlt_sum_x_transpose_x.vectorD().array().square().sum() / std::abs(x_window(0, 0));

    // check the condition number
    // if (cond_num > 30) {
    //   coefs.col(i).setConstant(NA_REAL);
    //   res_var(i) = NA_REAL;
    // }
    //Eigen::MatrixXd x_window_rank_check = x_window.block(0, 0, window_size, p);
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
