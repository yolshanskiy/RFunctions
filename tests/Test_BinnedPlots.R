library(dplyr)
library(data.table)
library(RFunctions)
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
Rcpp::sourceCpp("src/BinnedAverages.cpp")
# Generate a vector with 100,000 observations
x <- rnorm(100000)
y <- x^2 + rnorm(100000, sd = 0.5)

as.data.table(SortXY(x,y))
as.data.table(SortXY(x,y)) %>%
  .[,mean(Y - X^2)]

# Define the number of splits
n <- 100

# Define the number of bootstrap samples
nboot <- 100

###### Binned X #####

X_Av = BinnedAverageX(x, n = n)

X_Av %>%
  as.data.table() %>%
  ggplot(aes(means, sds)) +
  geom_point() + ggtitle("SE within groups") + ylab("SE of X") + xlab("Mean of group")

## note that x, y are not affected by the function
as.data.table(SortXY(x,y))

###### Binned Y by X #####

Y_Av = BinnedAverageY(x, y, n = n)

data.table(Y = Y_Av$means, X = X_Av$means) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ggtitle("Y's average within groups") + ylab("Average Y") + xlab("Mean of group")


## same with different code:
XY_Av = BinnedAverage(x, y, n = n)
data.table(X = XY_Av$Xmeans, Y = XY_Av$Ymeans) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ggtitle("Y's average within groups") + ylab("Average Y") + xlab("Mean of group")

###### Binned SE of Y by X #####


data.table(X = XY_Av$Xmeans, Y = XY_Av$Ysds) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ggtitle("Y's sd within groups") + ylab("SD of Y") + xlab("Mean of group")


###### Bootstrapped standard errors  #####

X_AvBT = BinnedBootstrapX(x, n = n, nboot = 1000)
X_AvBT %>%
  as.data.table() %>%
  ggplot(aes(means, se)) +
  geom_point() + ggtitle("Bootstrapped SE within groups") + ylab("SE of X") + xlab("Mean of group")

as.data.table(SortXY(x,y))
Y_AvBT = BinnedBootstrapY(x, y, n = n, nboot = 1000)
names(Y_AvBT)

data.table(Y = Y_AvBT$means, X = X_AvBT$means) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ggtitle("Bootstrapped Means of Y vs X")

data.table(Y = Y_AvBT$se, X = X_AvBT$means) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ggtitle("Bootstrapped SEs for Y")

as.data.table(SortXY(x,y))
XY_AvBT = BinnedBootstrap(x, y, n = n, nboot = 1000)
names(XY_AvBT)

data.table(Y = XY_AvBT$Ymeans, X = XY_AvBT$Xmeans) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ylab("Bootstrapped Mean of Y per X")

data.table(Y = XY_AvBT$Yses, X = XY_AvBT$Xmeans) %>%
  ggplot(aes(X, Y)) +
  geom_point() + ylab("Bootstrapped SE of Y per X")




# Define the splitVector function
library(boot)
BinnedBootstrap_R
funEx<- function(x, n) {
  # Sort the vector
  x <- sort(x)

  # Calculate the size of each split
  splitSize <- length(x) / n

  # Initialize vectors to store the means and standard errors
  means <- rep(NA, n)
  ses <- rep(NA, n)

  # Iterate over the list and split the vector into n pieces
  for (i in 1:n) {
    # Calculate the starting and ending indices for this split
    start <- (i - 1) * splitSize + 1
    end <- i * splitSize

    # If this is the last split, make sure it includes any remaining elements
    if (i == n) {
      end <- length(x)
    }

    # Extract the split from the vector
    split <- x[start:end]

    # Compute the mean and standard error of this split using bootstrapping
    bootMeans <- boot::boot(split, mean, nboot, trim = 0)

    # Store the mean and standard error in the vectors
    means[i] <- mean(bootMeans)
    ses[i] <- sd(bootMeans)
  }

  # Return the vectors as a list
  return(list(means = means, se = se))
}

# Test the splitVector function
result <- BinnedBootstrap_R(x, n)

result <- BinnedBootstrap(x, n = 100, nboot = nboot )

# Print the means and standard errors
print(result$means)
print(result$se)
