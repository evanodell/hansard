

#' House of Commons Oral Questions
#'
#' Imports data on House of Commons oral questions, based on the asking MP, the answering department and the date. The \code{mp_id} and \code{answering_department} parameters accept a single ID or department names, or a list of IDs or department names, respectively.
#'
#' @param mp_id The ID of a given MP asking an oral question, or a list of MP Ids. Defaults to \code{NULL}.
#' @param answering_department The name of a department, or a list of departments. Defaults to \code{NULL}.
#' @param start_date Only includes questions answered on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes questions answered on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on all oral questions in the House of Commons.
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#' x <- commons_oral_questions(mp_id=4019, answering_department = 'education')
#'
#' x <- commons_oral_questions(mp_id=c(4019,4051,4588),
#'                             answering_department = c('education', 'health'))
#' }

commons_oral_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (length(mp_id) > 1 || length(answering_department) > 1) {

    df <- commons_oral_questions_multi(mp_id, answering_department, start_date, end_date, extra_args, verbose)

  } else {

    if (is.null(mp_id) == FALSE && is.na(mp_id) == FALSE) {

        mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)

    } else {

        mp_id <- NULL

    }

    if (is.null(answering_department) == FALSE && is.na(answering_department) == FALSE) {

        query <- utils::URLencode(paste0("/answeringdepartment.json?q=", answering_department))

    } else {

        query <- ".json?"


    }

    dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", as.Date(end_date), "&min-AnswerDate=", as.Date(start_date))

    baseurl <- "http://lda.data.parliament.uk/commonsoralquestions"

    if(verbose==TRUE){message("Connecting to API")}

    oral <- jsonlite::fromJSON(paste0(baseurl, query, mp_id, dates, extra_args), flatten = TRUE)

    jpage <- floor(oral$result$totalResults/500)

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

            df <- coq_tidy(df, tidy_style)## in utils-commons.R

        }

            df

    }

}


#' @rdname commons_oral_questions
#' @export
hansard_commons_oral_questions <- commons_oral_questions
