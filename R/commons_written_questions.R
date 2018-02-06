
#' House of Commons Written Questions
#'
#' Imports data in a tibble on House of Commons written questions.
#'
#' @param mp_id Accepts a member ID or a list of member IDs and returns a
#' tibble with all written questions asked by that MP or list of MPs. If
#' \code{NULL}, mp_id is not included as a query parameter.
#' Defaults to \code{NULL}.
#' @param answering_department Accepts a string with a department name or
#' partial name, or a list of such strings. The query acts as a search, so
#' passing \code{'health'} will return all questions answered by the
#' Department of Health. If \code{NULL}, answering_department is not included
#' as a query parameter. Defaults to \code{NULL}.
#' @param start_date Only includes questions tabled on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes questions tabled on or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on written questions in the House of Commons.
#' @export
#' @examples \dontrun{
#' x <- commons_written_questions(mp_id=410, 'cabinet office')
#' # Returns a tibble with written questions from Jon Trickett,
#' # answered by the Cabinet Office.
#'
#' x <- commons_written_questions(mp_id=c(410,172), c('cabinet','home'))
#' # Returns a tibble with written questions from Jon Trickett or Diane Abbott,
#' # and answered by the Cabinet Office or the Home Office.
#' }

commons_written_questions <- function(mp_id = NULL, answering_department = NULL,
                                      start_date = "1900-01-01",
                                      end_date = Sys.Date(), extra_args = NULL,
                                      tidy = TRUE, tidy_style = "snake_case",
                                      verbose = TRUE) {

    if (length(mp_id) > 1 || length(answering_department) > 1) {
        ## For lists queries

        df <- commons_written_questions_multi(mp_id, answering_department,
                                              start_date, end_date,
                                              extra_args, verbose)

    } else {

        dates <- paste0("&_properties=dateTabled&max-dateTabled=",
                        as.Date(end_date),
                        "&min-dateTabled=",
                        as.Date(start_date))

        mp_id_query <- dplyr::if_else(is.null(mp_id) == FALSE && is.na(mp_id) == FALSE,
                                      utils::URLencode(paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)),
                                      "")

        json_query <- dplyr::if_else(is.null(answering_department) == FALSE &&
                                       is.na(answering_department) == FALSE,
                                     utils::URLencode(paste0("/answeringdepartment.json?q=", answering_department)),
                                     ".json?")

        baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions"

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        writ <- jsonlite::fromJSON(paste0(baseurl, json_query, mp_id_query,
                                          dates, extra_args, "&_pageSize=1"),
                                   flatten = TRUE)

        jpage <- floor(writ$result$totalResults/500)

        query <- paste0(baseurl, json_query, mp_id_query, dates,
                        extra_args, "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    }

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- cwq_tidy(df, tidy_style)  ## in utils-commons.R

        }

        df

    }

}


#' @rdname commons_written_questions
#' @export
hansard_commons_written_questions <- commons_written_questions
