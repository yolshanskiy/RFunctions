#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix create_x_lagged(Rcpp::NumericVector x, int p, bool flag_keep_x = true) {
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



