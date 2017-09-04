
#' House of Commons Written Questions
#'
#' Imports data in a tibble on House of Commons written questions.
#'
#' @param mp_id Accepts a member ID or a list of member IDs and returns a tibble with all written questions asked by that MP or list of MPs. If \code{NULL}, mp_id is not included as a query parameter. Defaults to \code{NULL}.
#' @param answering_department Accepts a string with a department name or partial name, or a list of such strings. The query acts as a search, so passing \code{'health'} will return all questions answered by the Department of Health. If \code{NULL}, answering_department is not included as a query parameter. Defaults to \code{NULL}.
#' @param start_date Only includes questions tabled on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes questions tabled on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on written questions in the House of Commons.
#' @export
#' @examples \dontrun{
#' x <- commons_written_questions(mp_id=410, 'cabinet office')
#' #Returns a tibble with written questions from Jon Trickett, answered by the Cabinet Office.
#'
#' x <- commons_written_questions(mp_id=c(410,172), c('cabinet','home'))
#' # Returns a tibble with written questions from Jon Trickett or Diane Abbot,
#' # and answered by the Cabinet Office or the Home Office.
#' }

commons_written_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {


  if (length(mp_id) > 1 || length(answering_department) > 1) { ## For lists queries

    df <- commons_written_questions_multi(mp_id, answering_department, start_date, end_date, extra_args, verbose)

  } else {

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=", as.Date(start_date))

    if (is.null(mp_id) == FALSE && is.na(mp_id) == FALSE) {

        mp_id <- utils::URLencode(paste0("&tablingMember=http://data.parliament.uk/members/", mp_id))

    } else {

      mp_id <- NULL

    }

    if (is.null(answering_department) == FALSE && is.na(answering_department) == FALSE) {

        query <- utils::URLencode(paste0("/answeringdepartment.json?q=", answering_department))

    } else {

        query <- ".json?"

    }

    baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions"

    if(verbose==TRUE){message("Connecting to API")}

    writ <- jsonlite::fromJSON(paste0(baseurl, query, mp_id, dates, extra_args), flatten = TRUE)

    jpage <- floor(writ$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, mp_id, dates, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  }

    if (nrow(df) == 0 && verbose==TRUE) {

        message("The request did not return any data. Please check your search parameters.")

    } else {

        if (tidy == TRUE) {

          df <- cwq_tidy(df, tidy_style)## in utils-commons.R

        }

          df

    }
}


#' @rdname commons_written_questions
#' @export
hansard_commons_written_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(),  extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

    df <- commons_written_questions(mp_id = mp_id, answering_department = answering_department, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

    df

}
