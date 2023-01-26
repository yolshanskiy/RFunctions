library(dplyr)
library(data.table)
library(RFunctions)

##### Compare results for different simulations #####

## set seed
set.seed(1)
## simulate matrix
mm = matrix(runif(100), nrow = 25, ncol = 4)

## test functions
rowMins_local(mm)
## same by Rfast package:
# Rfast::rowMins(mm)

## introduce NAs to matrix
mm[25,1] = NA_real_       ## Rfast is robust to one NA
mm[24,] = rep(NA_real_,4) ## Rfast is not robust to multiple NAs
rowMins_local(mm)
class(rowMins_local(mm))

## unfortunately Eigen can do nothing around NA values:
## the src code has RcppArmadillo commented code that handles it
## but RcppArmadillo tends to creates problem when compilled withing the package

# rowMins_narobust(mm)

## check that class is integer
# class(rowMins_narobust(mm))

