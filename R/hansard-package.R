#' hansard: Provides Easy Downloading Capabilities for the UK Parliament API
#'
#' Provides functions to request data from the data.parliament.uk APIs.
#' Because of the structure of the data.parliament.uk API, there is a named
#' function for each type of available data for ease of use. Functions for
#' each new API will be added as and when they become available on
#' \url{data.parliament.uk}. The API is rate limited to returning 5500 rows
#' per request in some instances, though this has been inconsistent in testing.
#'
#' The API itself is still in beta, and only about half of the planned datasets
#' are currently available. The package name is optimistic, as the
#' actual contents of the Hansard are not yet available through this API.
#'
#' In addition to the standard function names, each function in the
#' `hansard` package has a wrapper where the name is prefixed with
#' `'hansard_'`. For example, both `bills()` and
#' `hansard_bills()` will return the same result. This is because
#' function names are taken from the specific API on
#' \url{http://explore.data.parliament.uk/}, but they are often not very
#' informative and could clash with functions in other packages (e.g.
#' `bills()` is not a term unique to the British parliament).
#'
#' For more details please see the
#' \href{http://ropengov.github.io/hansard/articles/introduction.html}{vignette},
#' or the API documentation on \url{http://explore.data.parliament.uk/}.
#'
#' This package is in no way officially related to or endorsed by the UK
#' Parliamentary Data Service.
#'
#'
#' @docType package
#' @name hansard
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows case_when if_else left_join bind_rows grouped_df
#' summarise_all distinct
#' @importFrom tibble as_tibble as.tibble
#' @importFrom tidyr spread_ unnest_
#' @importFrom lubridate parse_date_time
NULL
