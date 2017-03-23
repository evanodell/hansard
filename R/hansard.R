#' hansard: Provides Easy Downloading Capabilities for the UK Parliament API
#'
#' Provides functions to request data from the data.parliament.uk APIs. Because of the structure of the data.parliament.uk API, there is a named function for each type of available data for ease of use. Functions for each new API will be added as and when they become available on data.parliament.uk. The package is intended to simplify pulling data from an API for users unfamiliar with APIs, and does not cover all possible functionality available through the API. The API is rate limited to returning 5500 rows per request.
#'
#' For more details please see the vignette.
#'
#' @section Hansard functions:
#'
#' \code{\link{all_answered_questions}}
#'
#' \code{\link{bills}}
#'
#' \code{\link{commons_answered_questions}}
#'
#' \code{\link{commons_divisions}}
#'
#' \code{\link{commons_oral_question_times}}
#'
#' \code{\link{commons_oral_questions}}
#'
#' \code{\link{commons_terms}}
#'
#' \code{\link{commons_written_questions}}
#'
#' \code{\link{constituencies}}
#'
#' \code{\link{early_day_motions}}
#'
#' \code{\link{election_results}}
#'
#' \code{\link{elections}}
#'
#' \code{\link{epetition}}
#'
#' \code{\link{hansard_generic}}
#'
#' \code{\link{lord_vote_record}}
#'
#' \code{\link{lords_amendments}}
#'
#' \code{\link{lords_attendance}}
#'
#' \code{\link{lords_divisions}}
#'
#' \code{\link{lords_written_questions}}
#'
#' \code{\link{members}}
#'
#' \code{\link{members_search}}
#'
#' \code{\link{mp_edms}}
#'
#' \code{\link{mp_questions}}
#'
#' \code{\link{mp_vote_record}}
#'
#' \code{\link{papers_laid}}
#'
#' \code{\link{publication_logs}}
#'
#' \code{\link{research_briefings}}
#'
#' \code{\link{sessions_info}}
#'
#' \code{\link{tv_programmes}}
#'
#' @docType package
#' @name hansard
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import readr
NULL
