

#' Imports data on House of Commons oral questions
#' @param mp_id The ID of a given MP. Defaults to NULL.
#' @param answering_department The department that answers a question
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with details on all oral questions in the House of Commons.
#' @keywords questions
#' @seealso \code{\link{all_answered_questions}} \code{\link{commons_answered_questions}} \code{\link{commons_oral_question_times}} \code{\link{commons_written_questions}}  \code{\link{lords_written_questions}} \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#' x <- commons_oral_questions(mp_id=4019, answering_department = 'education')
#'
#' }

commons_oral_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(mp_id) == FALSE) {
        mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)
    }

    if (is.null(answering_department) == FALSE) {

        query <- "/answeringdepartment"

        answering_department <- paste0("q=", answering_department)

    } else {

        query <- NULL

    }

    dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", as.Date(end_date), "&min-AnswerDate=",as.Date(start_date))

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

          df$AnswerDateTime._value <- gsub("T", " ", df$AnswerDateTime._value)

          df$AnswerDateTime._value <- lubridate::parse_date_time(df$AnswerDateTime._value, "Y-m-d H:M:S")

          df$AnswerDateTime._datatype <- "POSIXct"

          df$AnswerDate._value <- as.Date(df$AnswerDate._value)

          df$AnswerDate._datatype <- "Date"

          df$modified._value <- gsub("T", " ", df$modified._value)

          df$modified._value <- lubridate::parse_date_time(df$modified._value, "Y-m-d H:M:S")

          df$modified._datatype <- "POSIXct"

          df <- hansard_tidy(df, tidy_style)

          df

        } else {

          df

        }

    }
}
