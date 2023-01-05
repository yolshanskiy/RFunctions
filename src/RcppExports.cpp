// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// splitVector
List splitVector(NumericVector x, int n);
RcppExport SEXP _RFunctions_splitVector(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(splitVector(x, n));
    return rcpp_result_gen;
END_RCPP
}
// SortXY
List SortXY(NumericVector x, NumericVector y);
RcppExport SEXP _RFunctions_SortXY(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SortXY(x, y));
    return rcpp_result_gen;
END_RCPP
}
// BinnedAverageX
List BinnedAverageX(NumericVector x, int n);
RcppExport SEXP _RFunctions_BinnedAverageX(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedAverageX(x, n));
    return rcpp_result_gen;
END_RCPP
}
// BinnedAverageY
List BinnedAverageY(NumericVector x, NumericVector y, int n);
RcppExport SEXP _RFunctions_BinnedAverageY(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedAverageY(x, y, n));
    return rcpp_result_gen;
END_RCPP
}
// BinnedAverage
List BinnedAverage(NumericVector x, NumericVector y, int n);
RcppExport SEXP _RFunctions_BinnedAverage(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedAverage(x, y, n));
    return rcpp_result_gen;
END_RCPP
}
// BinnedBootstrapX
List BinnedBootstrapX(NumericVector x, int n, int nboot);
RcppExport SEXP _RFunctions_BinnedBootstrapX(SEXP xSEXP, SEXP nSEXP, SEXP nbootSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type nboot(nbootSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedBootstrapX(x, n, nboot));
    return rcpp_result_gen;
END_RCPP
}
// BinnedBootstrapY
List BinnedBootstrapY(NumericVector x, NumericVector y, int n, int nboot);
RcppExport SEXP _RFunctions_BinnedBootstrapY(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP, SEXP nbootSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type nboot(nbootSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedBootstrapY(x, y, n, nboot));
    return rcpp_result_gen;
END_RCPP
}
// BinnedBootstrap
List BinnedBootstrap(NumericVector x, NumericVector y, int n, int nboot);
RcppExport SEXP _RFunctions_BinnedBootstrap(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP, SEXP nbootSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type nboot(nbootSEXP);
    rcpp_result_gen = Rcpp::wrap(BinnedBootstrap(x, y, n, nboot));
    return rcpp_result_gen;
END_RCPP
}
// checkCollinearity
double checkCollinearity(Eigen::MatrixXd X);
RcppExport SEXP _RFunctions_checkCollinearity(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(checkCollinearity(X));
    return rcpp_result_gen;
END_RCPP
}
// create_x_lagged
Rcpp::NumericMatrix create_x_lagged(Rcpp::NumericVector x, int p, bool flag_keep_x);
RcppExport SEXP _RFunctions_create_x_lagged(SEXP xSEXP, SEXP pSEXP, SEXP flag_keep_xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< bool >::type flag_keep_x(flag_keep_xSEXP);
    rcpp_result_gen = Rcpp::wrap(create_x_lagged(x, p, flag_keep_x));
    return rcpp_result_gen;
END_RCPP
}
// roll_AR_checkCollinear
Rcpp::List roll_AR_checkCollinear(Rcpp::NumericVector y, Rcpp::NumericVector x, int p, int window_size);
RcppExport SEXP _RFunctions_roll_AR_checkCollinear(SEXP ySEXP, SEXP xSEXP, SEXP pSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_AR_checkCollinear(y, x, p, window_size));
    return rcpp_result_gen;
END_RCPP
}
// roll_AR_with_constant_checkCollinear
Rcpp::List roll_AR_with_constant_checkCollinear(Rcpp::NumericVector y, Rcpp::NumericVector x, int p, int window_size);
RcppExport SEXP _RFunctions_roll_AR_with_constant_checkCollinear(SEXP ySEXP, SEXP xSEXP, SEXP pSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_AR_with_constant_checkCollinear(y, x, p, window_size));
    return rcpp_result_gen;
END_RCPP
}
// roll_reg_withNo_constant
Rcpp::List roll_reg_withNo_constant(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size);
RcppExport SEXP _RFunctions_roll_reg_withNo_constant(SEXP ySEXP, SEXP xSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_reg_withNo_constant(y, x, window_size));
    return rcpp_result_gen;
END_RCPP
}
// roll_reg_with_constant
Rcpp::List roll_reg_with_constant(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size);
RcppExport SEXP _RFunctions_roll_reg_with_constant(SEXP ySEXP, SEXP xSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_reg_with_constant(y, x, window_size));
    return rcpp_result_gen;
END_RCPP
}
// roll_reg_with_constant_faster
Rcpp::List roll_reg_with_constant_faster(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size);
RcppExport SEXP _RFunctions_roll_reg_with_constant_faster(SEXP ySEXP, SEXP xSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_reg_with_constant_faster(y, x, window_size));
    return rcpp_result_gen;
END_RCPP
}
// roll_reg_with_constant_faster_checkCollinear
Rcpp::List roll_reg_with_constant_faster_checkCollinear(Rcpp::NumericVector y, Rcpp::NumericMatrix x, int window_size);
RcppExport SEXP _RFunctions_roll_reg_with_constant_faster_checkCollinear(SEXP ySEXP, SEXP xSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_reg_with_constant_faster_checkCollinear(y, x, window_size));
    return rcpp_result_gen;
END_RCPP
}
// winsorize
Rcpp::NumericVector winsorize(Rcpp::NumericVector x, double lower_cutoff, double upper_cutoff);
RcppExport SEXP _RFunctions_winsorize(SEXP xSEXP, SEXP lower_cutoffSEXP, SEXP upper_cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lower_cutoff(lower_cutoffSEXP);
    Rcpp::traits::input_parameter< double >::type upper_cutoff(upper_cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(winsorize(x, lower_cutoff, upper_cutoff));
    return rcpp_result_gen;
END_RCPP
}
// winsorize_quantiles
Rcpp::NumericVector winsorize_quantiles(Rcpp::NumericVector x, double lower_quantile, double upper_quantile, bool print_quantiles);
RcppExport SEXP _RFunctions_winsorize_quantiles(SEXP xSEXP, SEXP lower_quantileSEXP, SEXP upper_quantileSEXP, SEXP print_quantilesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lower_quantile(lower_quantileSEXP);
    Rcpp::traits::input_parameter< double >::type upper_quantile(upper_quantileSEXP);
    Rcpp::traits::input_parameter< bool >::type print_quantiles(print_quantilesSEXP);
    rcpp_result_gen = Rcpp::wrap(winsorize_quantiles(x, lower_quantile, upper_quantile, print_quantiles));
    return rcpp_result_gen;
END_RCPP
}
// winsorize_one_sided
Rcpp::NumericVector winsorize_one_sided(Rcpp::NumericVector x, double cutoff, bool lower, bool print_cutoff);
RcppExport SEXP _RFunctions_winsorize_one_sided(SEXP xSEXP, SEXP cutoffSEXP, SEXP lowerSEXP, SEXP print_cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    Rcpp::traits::input_parameter< bool >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< bool >::type print_cutoff(print_cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(winsorize_one_sided(x, cutoff, lower, print_cutoff));
    return rcpp_result_gen;
END_RCPP
}
// winsorize_one_sided_quantile
Rcpp::NumericVector winsorize_one_sided_quantile(Rcpp::NumericVector x, double quantile, bool lower, bool print_cutoff);
RcppExport SEXP _RFunctions_winsorize_one_sided_quantile(SEXP xSEXP, SEXP quantileSEXP, SEXP lowerSEXP, SEXP print_cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type quantile(quantileSEXP);
    Rcpp::traits::input_parameter< bool >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< bool >::type print_cutoff(print_cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(winsorize_one_sided_quantile(x, quantile, lower, print_cutoff));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RFunctions_splitVector", (DL_FUNC) &_RFunctions_splitVector, 2},
    {"_RFunctions_SortXY", (DL_FUNC) &_RFunctions_SortXY, 2},
    {"_RFunctions_BinnedAverageX", (DL_FUNC) &_RFunctions_BinnedAverageX, 2},
    {"_RFunctions_BinnedAverageY", (DL_FUNC) &_RFunctions_BinnedAverageY, 3},
    {"_RFunctions_BinnedAverage", (DL_FUNC) &_RFunctions_BinnedAverage, 3},
    {"_RFunctions_BinnedBootstrapX", (DL_FUNC) &_RFunctions_BinnedBootstrapX, 3},
    {"_RFunctions_BinnedBootstrapY", (DL_FUNC) &_RFunctions_BinnedBootstrapY, 4},
    {"_RFunctions_BinnedBootstrap", (DL_FUNC) &_RFunctions_BinnedBootstrap, 4},
    {"_RFunctions_checkCollinearity", (DL_FUNC) &_RFunctions_checkCollinearity, 1},
    {"_RFunctions_create_x_lagged", (DL_FUNC) &_RFunctions_create_x_lagged, 3},
    {"_RFunctions_roll_AR_checkCollinear", (DL_FUNC) &_RFunctions_roll_AR_checkCollinear, 4},
    {"_RFunctions_roll_AR_with_constant_checkCollinear", (DL_FUNC) &_RFunctions_roll_AR_with_constant_checkCollinear, 4},
    {"_RFunctions_roll_reg_withNo_constant", (DL_FUNC) &_RFunctions_roll_reg_withNo_constant, 3},
    {"_RFunctions_roll_reg_with_constant", (DL_FUNC) &_RFunctions_roll_reg_with_constant, 3},
    {"_RFunctions_roll_reg_with_constant_faster", (DL_FUNC) &_RFunctions_roll_reg_with_constant_faster, 3},
    {"_RFunctions_roll_reg_with_constant_faster_checkCollinear", (DL_FUNC) &_RFunctions_roll_reg_with_constant_faster_checkCollinear, 3},
    {"_RFunctions_winsorize", (DL_FUNC) &_RFunctions_winsorize, 3},
    {"_RFunctions_winsorize_quantiles", (DL_FUNC) &_RFunctions_winsorize_quantiles, 4},
    {"_RFunctions_winsorize_one_sided", (DL_FUNC) &_RFunctions_winsorize_one_sided, 4},
    {"_RFunctions_winsorize_one_sided_quantile", (DL_FUNC) &_RFunctions_winsorize_one_sided_quantile, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_RFunctions(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}