

#' Imports data on House of Commons oral questions
#' @param mp_id The ID of a given MP. Defaults to NULL.
#' @param answering_department The department that answers a question
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on all oral questions in the House of Commons.
#' @keywords questions
#' @seealso \code{\link{all_answered_questions}} \code{\link{commons_answered_questions}} \code{\link{commons_oral_question_times}} \code{\link{commons_written_questions}}  \code{\link{lords_written_questions}} \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#' x <- commons_oral_questions(mp_id=4019, answering_department = 'education')
#'
#' }

commons_oral_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    if (is.null(mp_id) == FALSE) {
        mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)
    }

    if (is.null(answering_department) == FALSE) {

        query <- "/answeringdepartment"

        answering_department <- paste0("q=", answering_department)

    } else {

        query <- NULL

    }

    dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", end_date, "&min-AnswerDate=", start_date)

    baseurl <- "http://lda.data.parliament.uk/commonsoralquestions"

    message("Connecting to API")

    oral <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500", extra_args),
        flatten = TRUE)

    jpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500&_page=",
            i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}
