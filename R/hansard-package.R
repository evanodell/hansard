#' hansard: Provides Easy Downloading Capabilities for the UK Parliament API
#'
#' Provides functions to request data from the data.parliament.uk APIs. Because of the structure of the data.parliament.uk API, there is a named function for each type of available data for ease of use. Functions for each new API will be added as and when they become available on data.parliament.uk. The package is intended to simplify pulling data from an API for users unfamiliar with APIs, and does not cover all possible functionality available through the API. The API is rate limited to returning 5500 rows per request in some instances.
#'
#' In addition to the more generic sounding function names, each function in hansard has a wrapper where the name is prefixed with \code{'hansard_'}. For example, both \code{bills()} and \code{hansard_bills()} will return the same result.
#'
#' For more details please see the vignette, or the API documentation on \url{http://explore.data.parliament.uk/}.
#'
#'
#' @docType package
#' @name hansard
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import stringr
#' @importFrom lubridate parse_date_time
NULL
