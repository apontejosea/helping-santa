#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame book_elf_c(DataFrame df) {

// access the columns
  Rcpp::IntegerVector a = df["a"];
  Rcpp::CharacterVector b = df["b"];
  
  // make some changes
  a[2] = 42;
  b[1] = "foo";       

  // return a new data frame
  return DataFrame::create(_["a"]= a, _["b"]= b);
}
