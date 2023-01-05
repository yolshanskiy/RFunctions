#include <Eigen/Dense>
# include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

#include <chrono>

using namespace Eigen;
using namespace arma;
using namespace std;
using namespace std::chrono;

// [[Rcpp::export]]
void test_ArmaVsEigen(int N = 1000) {
  // Set the size of the matrix


  // Create a dense matrix with Eigen
  MatrixXd A = MatrixXd::Random(N, N);
  MatrixXd B = MatrixXd::Random(N, N);
  MatrixXd C = MatrixXd::Zero(N, N);

  // Record the start time
  high_resolution_clock::time_point t1 = high_resolution_clock::now();

  // Perform matrix multiplication with Eigen
  C = A * B;

  // Record the end time
  high_resolution_clock::time_point t2 = high_resolution_clock::now();

  // Calculate the elapsed time
  duration<double> time_span = duration_cast<duration<double>>(t2 - t1);

  cout << "Eigen elapsed time: " << time_span.count() << " seconds." << endl;

  // Create a dense matrix with Armadillo
  mat A_arma = randu<mat>(N, N);
  mat B_arma = randu<mat>(N, N);
  mat C_arma = zeros<mat>(N, N);

  // Record the start time
  t1 = high_resolution_clock::now();

  // Perform matrix multiplication with Armadillo
  C_arma = A_arma * B_arma;

  // Record the end time
  t2 = high_resolution_clock::now();

  // Calculate the elapsed time
  time_span = duration_cast<duration<double>>(t2 - t1);

  cout << "Armadillo elapsed time: " << time_span.count() << " seconds." << endl;

  return;
}

/*** R
# test_ArmaVsEigen(N = 2000)
*/

