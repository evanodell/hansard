#include <Rcpp.h>
using namespace Rcpp;

// tidy_bom
// Internal function for \code{\link{tidy_bom}}
//
// @param df The API response to remove BOMs from.
// @export
// [[Rcpp::export]]
std::string tidy_bom(std::string df) {
  if (df.size() < 3)
    return df;

  if (df[0] == '\xEF' && df[1] == '\xBB' && df[2] == '\xBF')
    return df.substr(3);

  return df;
}

/*** R
df <- "\uFEFFabcdef"
print(df)
  print(tidy_bom(df))
  identical(df, tidy_bom(df))
  utf8ToInt(df)
  utf8ToInt(tidy_bom(df))
  */
