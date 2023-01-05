#include <Rcpp.h>
using namespace Rcpp;

Rcpp::Function quantile_fn("quantile", R_BaseNamespace);

// [[Rcpp::export]]
Rcpp::NumericVector winsorize(Rcpp::NumericVector x, double lower_cutoff, double upper_cutoff) {
  int n = x.size();
  Rcpp::NumericVector y(n);

  for (int i = 0; i < n; i++) {
    if (x[i] < lower_cutoff) {
      y[i] = lower_cutoff;
    } else if (x[i] > upper_cutoff) {
      y[i] = upper_cutoff;
    } else {
      y[i] = x[i];
    }
  }

  return y;
}

// [[Rcpp::export]]
Rcpp::NumericVector winsorize_quantiles(Rcpp::NumericVector x, double lower_quantile, double upper_quantile, bool print_quantiles = false) {
  int n = x.size();
  Rcpp::NumericVector y(n);

  double lower_cutoff = Rcpp::as<double>(quantile_fn(x, lower_quantile / 100));
  double upper_cutoff = Rcpp::as<double>(quantile_fn(x, upper_quantile / 100));

  if (print_quantiles) {
    Rcpp::Rcout << "Lower cutoff: " << lower_cutoff << std::endl;
    Rcpp::Rcout << "Upper cutoff: " << upper_cutoff << std::endl;
  }

  for (int i = 0; i < n; i++) {
    if (x[i] < lower_cutoff) {
      y[i] = lower_cutoff;
    } else if (x[i] > upper_cutoff) {
      y[i] = upper_cutoff;
    } else {
      y[i] = x[i];
    }
  }

  return y;
}

// [[Rcpp::export]]
Rcpp::NumericVector winsorize_one_sided(Rcpp::NumericVector x, double cutoff, bool lower, bool print_cutoff = false) {
  int n = x.size();
  Rcpp::NumericVector y(n);

  if (print_cutoff) {
    Rcpp::Rcout << "Cutoff: " << cutoff << std::endl;
  }

  for (int i = 0; i < n; i++) {
    if (lower && x[i] < cutoff) {
      y[i] = cutoff;
    } else if (!lower && x[i] > cutoff) {
      y[i] = cutoff;
    } else {
      y[i] = x[i];
    }
  }

  return y;
}

// [[Rcpp::export]]
Rcpp::NumericVector winsorize_one_sided_quantile(Rcpp::NumericVector x, double quantile, bool lower, bool print_cutoff = false) {
  int n = x.size();
  Rcpp::NumericVector y(n);

  double q =  lower ? quantile / 100 : (1 - quantile / 100);
  double cutoff = Rcpp::as<double>(quantile_fn(x, q));

  if (print_cutoff) {
    Rcpp::Rcout << "Cutoff: " << cutoff << std::endl;
  }

  for (int i = 0; i < n; i++) {
    if (lower && x[i] < cutoff) {
      y[i] = cutoff;
    } else if (!lower && x[i] > cutoff) {
      y[i] = cutoff;
    } else {
      y[i] = x[i];
    }
  }

  return y;
}
