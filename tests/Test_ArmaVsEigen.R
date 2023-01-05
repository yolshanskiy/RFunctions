# To justify using Eigen instead of Arma, use the code to compare speed of matrix multiplication
library(Rcpp)
library(RcppArmadillo)
library(RcppEigen)

set.seed(123)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

## large matrix multiplication:

Rcpp::sourceCpp("test/test_ArmaVsEigen.cpp")
test_ArmaVsEigen(N = 2000)


###### To get rid of warnings from RcppEigen #####

# The warning message "pragma diagnostic pop could not pop, no matching push" indicates that you have a #pragma clang diagnostic pop directive in your code without a corresponding #pragma clang diagnostic push directive.
#
# To get rid of this warning, you can either remove the #pragma clang diagnostic pop directive or add a matching #pragma clang diagnostic push directive before it.
#
# For example, to suppress the warning message, you can do the following:

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#pragma clang diagnostic pop
