// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
#include <Rcpp.h>
//' @export
// [[Rcpp::export]]
SEXP eigenTransMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B){
  Eigen::MatrixXd D=B.transpose();
  Eigen::MatrixXd C = A * D;

  return Rcpp::wrap(C);
}

//' @export
// [[Rcpp::export]]
SEXP eigenMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B){
  Eigen::MatrixXd C = A * B;

  return Rcpp::wrap(C);
}

//' @export
// [[Rcpp::export]]
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
  Eigen::MatrixXd C = A * B;

  return Rcpp::wrap(C);
}

//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix ScalarMult(Rcpp::NumericMatrix & X, Rcpp::NumericVector & y){
  unsigned int ncol = X.ncol();
  unsigned int nrow = X.nrow();
  int counter = 0;
  for (unsigned int j=0; j<ncol; j++) {
    for (unsigned int i=0; i<nrow; i++)  {
      X[counter++] *= y[i];
    }
  }
  return Rcpp::wrap(X);
}
