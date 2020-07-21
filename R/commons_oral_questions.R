

#' House of Commons Oral Questions
#'
#' Imports data on House of Commons oral questions, based on the asking MP,
#' the answering department and the date. The `mp_id` and
#' `answering_department` parameters accept a single ID or department
#' names, or a list of IDs or department names, respectively.
#'
#' @param mp_id The ID of a given MP asking an oral question, or a list of
#' MP Ids. Defaults to `NULL`.
#'
#' @param answering_department The name of a department, or a list of
#' departments. Defaults to `NULL`.
#'
#' @param start_date Only includes questions answered on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything
#' else that can be coerced to a date with `as.Date()`. ]
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date Only includes questions answered on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of class
#' `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else
#' that can be coerced to a date with `as.Date()`. Defaults to the current
#' system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on all oral questions in the House of Commons.
#' @seealso [all_answered_questions()]
#' @seealso [commons_answered_questions()]
#' @seealso [commons_oral_question_times()]
#' @seealso [commons_written_questions()]
#' @seealso [lords_written_questions()]
#' @seealso [mp_questions()]
#' @export
#' @examples
#' \dontrun{
#'
#' # Oral questions from a single MP to a single department
#' x <- commons_oral_questions(mp_id = 4019, answering_department = "education")
#'
#'
#' ## Questions from multiple MPs and to multiple departments
#' y <- commons_oral_questions(
#'   mp_id = c(4019, 4051, 4588),
#'   answering_department = c("education", "health")
#' )
#' }
#'
commons_oral_questions <- function(mp_id = NULL, answering_department = NULL,
                                   start_date = "1900-01-01",
                                   end_date = Sys.Date(),
                                   extra_args = NULL, tidy = TRUE,
                                   tidy_style = "snake", verbose = TRUE) {
  if (length(mp_id) > 1 || length(answering_department) > 1) {
    df <- commons_oral_questions_multi(
      mp_id, answering_department,
      start_date, end_date,
      extra_args, verbose
    )
  } else {

    dates <- paste0(
      "&_properties=AnswerDate&max-AnswerDate=",
      as.Date(end_date),
      "&min-AnswerDate=",
      as.Date(start_date)
    )

    json_query <- question_query_construction(mp_id, answering_department)

    baseurl <- paste0(url_util, "commonsoralquestions")

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    query <- paste0(
      baseurl, json_query, dates,
      extra_args
    )

    oral <- jsonlite::fromJSON(paste0(query, "&_pageSize=1"), flatten = TRUE)

    jpage <- floor(oral$result$totalResults / 100)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- coq_tidy(df, tidy_style) ## in utils-commons.R
    }

    df
  }
}


#' @rdname commons_oral_questions
#' @export
hansard_commons_oral_questions <- commons_oral_questions
