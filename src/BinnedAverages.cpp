#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List splitVector(NumericVector x, int n) {
  // Make a copy of x
  NumericVector x_copy = clone(x);
  // Sort the vector
  std::sort(x_copy.begin(), x_copy.end());

  // Calculate the size of each split
  int splitSize = x_copy.size() / n;

  // Create a list to store the split vectors
  List splits(n);

  // Iterate over the list and split the vector into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x_copy.size();
    }

    // Extract the split from the vector and store it in the list
    NumericVector split = x_copy[Range(start, end - 1)];
    splits[i] = split;

    // Compute the mean and standard deviation of this split
    // double mean = Rcpp::mean(split);
    // double sd = Rcpp::sd(split);

    // Store the mean and standard deviation in the list
    splits[i] = List::create(Named("split") = split);
                             // Named("mean") = mean,
                             // Named("sd") = sd);
  }

  return splits;
}

// [[Rcpp::export]]
List SortXY(NumericVector x, NumericVector y) {
  // Check that x and y have the same length
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    stop("Vectors x and y must have the same length.");
  }

  // Sort the vectors by x
  std::vector<int> ind(nx);
  std::iota(ind.begin(), ind.end(), 0);
  std::sort(ind.begin(), ind.end(), [&](int i, int j) { return x[i] < x[j]; });
  NumericVector sortedX(nx);
  NumericVector sortedY(ny);
  for (int i = 0; i < nx; i++) {
    sortedX[i] = x[ind[i]];
    sortedY[i] = y[ind[i]];
  }
  // Return the vectors as a list
  return List::create(Named("X") = sortedX,
                      Named("Y") = sortedY);
}

// [[Rcpp::export]]
List BinnedAverageX(NumericVector x, int n) {
  // Make a copy of x
  NumericVector x_copy = clone(x);

  // Sort the vector
  std::sort(x_copy.begin(), x_copy.end());

  // Calculate the size of each split
  int splitSize = x_copy.size() / n;

  // Create vectors to store the means and standard deviations
  NumericVector means(n);
  NumericVector sds(n);

  // Iterate over the list and split the vector into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x_copy.size();
    }

    // Extract the split from the vector
    NumericVector split = x_copy[Range(start, end - 1)];

    // Compute the mean and standard deviation of this split
    double mean = Rcpp::mean(split);
    double sd = Rcpp::sd(split);

    // Store the mean and standard deviation in the vectors
    means[i] = mean;
    sds[i] = sd;
  }

  // Return the vectors as a list
  return List::create(Named("means") = means,
                      Named("sds") = sds);
}

// [[Rcpp::export]]
List BinnedAverageY(NumericVector x, NumericVector y, int n) {
  // Check that x and y have the same length
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    stop("Vectors x and y must have the same length.");
  }

  // Sort the vectors by x
  std::vector<int> ind(nx);
  std::iota(ind.begin(), ind.end(), 0);
  std::sort(ind.begin(), ind.end(), [&](int i, int j) { return x[i] < x[j]; });
  NumericVector sortedX(nx);
  NumericVector sortedY(ny);
  for (int i = 0; i < nx; i++) {
    sortedX[i] = x[ind[i]];
    sortedY[i] = y[ind[i]];
  }

  // Calculate the size of each split
  int splitSize = x.size() / n;

  // Initialize vectors to store the means and standard deviations
  NumericVector means(n);
  NumericVector sds(n);

  // Iterate over the list and split the vectors into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x.size();
    }

    // Extract the splits from the vectors
    NumericVector splitX = sortedX[Range(start, end - 1)];
    NumericVector splitY = sortedY[Range(start, end - 1)];

    // Compute the mean and standard deviation of this split using y
    double mean = Rcpp::mean(splitY);
    double sd = Rcpp::sd(splitY);

    // Store the mean and standard deviation in the vectors
    means[i] = mean;
    sds[i] = sd;
  }

  // Return the vectors as a list
  return List::create(Named("means") = means,
                      Named("sds") = sds);
}

// [[Rcpp::export]]
List BinnedAverage(NumericVector x, NumericVector y, int n) {
  // Check that x and y have the same length
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    stop("Vectors x and y must have the same length.");
  }

  // Sort the vectors by x
  std::vector<int> ind(nx);
  std::iota(ind.begin(), ind.end(), 0);
  std::sort(ind.begin(), ind.end(), [&](int i, int j) { return x[i] < x[j]; });
  NumericVector sortedX(nx);
  NumericVector sortedY(ny);
  for (int i = 0; i < nx; i++) {
    sortedX[i] = x[ind[i]];
    sortedY[i] = y[ind[i]];
  }

  // Calculate the size of each split
  int splitSize = x.size() / n;

  // Initialize vectors to store the means and standard deviations
  NumericVector meansX(n), meansY(n);
  NumericVector sdsX(n), sdsY(n);

  // Iterate over the list and split the vectors into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x.size();
    }

    // Extract the splits from the vectors
    NumericVector splitX = sortedX[Range(start, end - 1)];
    NumericVector splitY = sortedY[Range(start, end - 1)];

    // Compute the mean and standard deviation of this split using x
    double meanX = Rcpp::mean(splitX);
    double sdX = Rcpp::sd(splitX);
    // Compute the mean and standard deviation of this split using y
    double meanY = Rcpp::mean(splitY);
    double sdY = Rcpp::sd(splitY);

    // Store the mean and standard deviation in the vectors
    meansX[i] = meanX;
    sdsX[i] = sdX;
    meansY[i] = meanY;
    sdsY[i] = sdY;
  }

  // Return the vectors as a list
  return List::create(Named("Xmeans") = meansX,
                      Named("Xsds") = sdsX,
                      Named("Ymeans") = meansY,
                      Named("Ysds") = sdsY);
}

NumericVector bootstrap(NumericVector x, int nboot) {
  // Create a vector to store the bootstrapped samples
  NumericVector samples(nboot);

  // Set the random seed for reproducibility
  Rcpp::RNGScope scope;

  // Perform bootstrapping
  for (int i = 0; i < nboot; i++) {
    // Sample with replacement from the data
    NumericVector sample = Rcpp::sample(x, x.size(), true);

    // Calculate the statistic of interest for this sample
    double statistic = Rcpp::mean(sample);

    // Store the statistic in the vector
    samples[i] = statistic;
  }

  return samples;
}

// [[Rcpp::export]]
List BinnedBootstrapX(NumericVector x, int n, int nboot) {

  // Make a copy of x
  NumericVector x_copy = clone(x);

  // Sort the vector
  std::sort(x_copy.begin(), x_copy.end());

  // Calculate the size of each split
  int splitSize = x_copy.size() / n;

  // Create vectors to store the means and standard errors
  NumericVector means(n);
  NumericVector ses(n);

  // Iterate over the list and split the vector into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x_copy.size();
    }

    // Extract the split from the vector
    NumericVector split = x_copy[Range(start, end - 1)];

    // Compute the mean and standard error of this split using bootstrapping
    NumericVector bootMeans = bootstrap(split, nboot);
    double mean = Rcpp::mean(bootMeans);
    double se = Rcpp::sd(bootMeans);

    // Store the mean and standard error in the vectors
    means[i] = mean;
    ses[i] = se;
  }

  // Return the vectors as a list
  return List::create(Named("means") = means,
                      Named("se") = ses);
}

// Define the funEx function
// [[Rcpp::export]]
List BinnedBootstrapY(NumericVector x, NumericVector y, int n, int nboot) {
  // Sort the vectors by x
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    stop("Vectors x and y must have the same length.");
  }

  // Create a vector of indices
  std::vector<int> ind(nx);
  std::iota(ind.begin(), ind.end(), 0);

  // Sort the indices according to the values in x
  std::sort(ind.begin(), ind.end(),
            [&](int i, int j)
    { return x[i] < x[j]; }
  );


  // Use the sorted indices to sort x and y
  NumericVector sortedX(nx);
  NumericVector sortedY(ny);
  for (int i = 0; i < nx; i++) {
    sortedX[i] = x[ind[i]];
    sortedY[i] = y[ind[i]];
  }

  // Calculate the size of each split
  int splitSize = x.size() / n;

  // Initialize vectors to store the means and standard errors
  NumericVector means(n);
  NumericVector ses(n);

  // Iterate over the list and split the vector into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x.size();
    }

    // Extract the splits from the vectors
    NumericVector splitX = sortedX[Range(start, end - 1)];
    NumericVector splitY = sortedY[Range(start, end - 1)];

    // Compute the mean and standard error of this split using bootstrapping
    NumericVector bootMeans = bootstrap(splitY, nboot);
    double mean = Rcpp::mean(bootMeans);
    double se = Rcpp::sd(bootMeans);

    // Store the mean and standard error in the vectors
    means[i] = mean;
    ses[i] = se;
  }

  // Return the vectors as a list
  return List::create(Named("means") = means,
                      Named("se") = ses);
}

// [[Rcpp::export]]
List BinnedBootstrap(NumericVector x, NumericVector y, int n, int nboot) {
  // Sort the vectors by x
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    stop("Vectors x and y must have the same length.");
  }

  // Create a vector of indices
  std::vector<int> ind(nx);
  std::iota(ind.begin(), ind.end(), 0);

  // Sort the indices according to the values in x
  std::sort(ind.begin(), ind.end(),
            [&](int i, int j)
            { return x[i] < x[j]; }
  );


  // Use the sorted indices to sort x and y
  NumericVector sortedX(nx);
  NumericVector sortedY(ny);
  for (int i = 0; i < nx; i++) {
    sortedX[i] = x[ind[i]];
    sortedY[i] = y[ind[i]];
  }

  // Calculate the size of each split
  int splitSize = x.size() / n;

  // Initialize vectors to store the means and standard errors
  NumericVector meansX(n), meansY(n);
  NumericVector sesX(n), sesY(n);

  // Iterate over the list and split the vector into n pieces
  for (int i = 0; i < n; i++) {
    // Calculate the starting and ending indices for this split
    int start = i * splitSize;
    int end = (i + 1) * splitSize;

    // If this is the last split, make sure it includes any remaining elements
    if (i == n - 1) {
      end = x.size();
    }

    // Extract the splits from the vectors
    NumericVector splitX = sortedX[Range(start, end - 1)];
    NumericVector splitY = sortedY[Range(start, end - 1)];

    // Compute the mean and standard error of this split using bootstrapping
    NumericVector bootMeans = bootstrap(splitY, nboot);

    // Compute the mean and standard deviation of this split using x
    double meanX = Rcpp::mean(splitX);
    double seX = Rcpp::sd(splitX);
    // Compute the mean and standard deviation of this split using y
    double meanY = Rcpp::mean(bootMeans);
    double seY = Rcpp::sd(bootMeans);

    // Store the mean and standard deviation in the vectors
    meansX[i] = meanX;
    sesX[i] = seX;
    meansY[i] = meanY;
    sesY[i] = seY;
  }

  // Return the vectors as a list
  return List::create(Named("Xmeans") = meansX,
                      Named("Xses") = sesX,
                      Named("Ymeans") = meansY,
                      Named("Yses") = sesY);
}

