#include <Rcpp.h>
using namespace Rcpp;

// hansard_bom
// Strip out BOM from JSON data
//
// @param x The GET return to strip BOM out of

// [[Rcpp::export]]
std::string hansard_bom(std::string x) {
  if (x.size() < 3)
    return x;

  if (x[0] == '\xEF' && x[1] == '\xBB' && x[2] == '\xBF')
    return x.substr(3);

  return x;
}

/*** R
x <- "\uFEFFabcdef"
print(x)
  print(hansard_bom(x))
  identical(x, hansard_bom(x))
  utf8ToInt(x)
  utf8ToInt(hansard_bom(x))
  */
