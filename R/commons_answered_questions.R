

#' House of Commons answered questions
#'
#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a tibble.
#'
#' If \code{answering_department} and/or \code{answered_by} are given arrays with multiple deparments/IDs, all possible combination of that criteria is returned.
#'
#' @param answering_department Accepts the name of a department or an array of department names.  Returns a tibble with all answered questions in the House of Commons from the given department. Defaults to \code{NULL}.
#' @param answered_by Accepts the ID of an MP, or an array of IDs. Returns a tibble with all answered questions in the House of Commons by the given MP(s). Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on all answered questions in the House of Commons.
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
### @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(answering_department = c('health','education'),
#'                                 answered_by = c('4019', '1542', '111'), start_date = '2017-01-01')
#'
#' x <- commons_answered_questions(start_date = '2017-03-26', end_date='2017-04-01')
#'
#' }


commons_answered_questions <- function(answering_department = NULL, answered_by = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (length(answered_by) > 1 || length(answering_department) > 1) {

    df <- caq_multi(answering_department, answered_by, start_date, end_date, extra_args, verbose)

  } else {

    dates <- paste0("&max-dateOfAnswer=", as.Date(end_date), "&min-dateOfAnswer=", as.Date(start_date))

    if (is.null(answered_by) == FALSE && is.na(answered_by)==FALSE) {
      answered_by <- paste0("&answeringMember=http://data.parliament.uk/members/", answered_by)
    } else {
      answered_by <- NULL
    }

    if (is.null(answering_department) == FALSE && is.na(answering_department)==FALSE) {
      query <- "/answeringdepartment"
      answering_department <- paste0("q=", answering_department)
    } else {
      query <- NULL
      answering_department <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions"

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    answered <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, dates, extra_args), flatten = TRUE)

    jpage <- floor(answered$result$totalResults/500)

    pages <- list()

    if (answered$result$totalResults > 0) {

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500&_page=", i, dates, extra_args), flatten = TRUE)
        if (verbose == TRUE) {
          message("Retrieving page ", i + 1, " of ", jpage + 1)
        }
        pages[[i + 1]] <- mydata$result$items
      }

    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  }

  if (nrow(df) == 0 && verbose == TRUE) {
    message("The request did not return any data. Please check your search parameters.")
  }

  if (tidy == TRUE) {

    df <- caq_tidy(df, tidy_style)  ## in utils-caq.R

  }

  df

}



#' @rdname commons_answered_questions
#' @export

hansard_commons_answered_questions <- function(answering_department = NULL, answered_by = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- commons_answered_questions(answering_department = NULL, answered_by = NULL, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}

