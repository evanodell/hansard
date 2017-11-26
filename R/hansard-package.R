#' hansard: Provides Easy Downloading Capabilities for the UK Parliament API
#'
#' Provides functions to request data from the data.parliament.uk APIs.
#' Because of the structure of the data.parliament.uk API, there is a named
#' function for each type of available data for ease of use. Functions for
#' each new API will be added as and when they become available on
#' \url{data.parliament.uk}. The API is rate limited to returning 5500 rows
#'  per request in some instances, although in testing this has been
#'  inconsistent.
#'
#' The API itself is still in beta, and only about half of the planned datasets
#' are currently available. The package name is ironic and hopeful, as the
#' actual contents of the Hansard are not yet available through this API.
#'
#' In addition to the standard function names, each function in the
#' \code{hansard} package has a wrapper where the name is prefixed with
#' \code{'hansard_'}. For example, both \code{bills()} and
#' \code{hansard_bills()} will return the same result. This is because
#' function names are taken from the specific API on
#' \url{http://explore.data.parliament.uk/}, but they are often not very
#' informative and could easily clash with functions in other packages
#' (e.g. \code{bills()} is not a term unique to the British parliament).
#'
#' For more details please see the
#' \href{http://ropengov.github.io/hansard/articles/introduction.html}{vignette},
#' or the API documentation on \url{http://explore.data.parliament.uk/}.
#'
#'
#' @docType package
#' @name hansard
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import stringi
#' @importFrom lubridate parse_date_time
NULL
