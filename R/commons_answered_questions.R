

#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a tibble.
#' @param answering_department Returns a tibble with all answered questions in the House of Commons from the given department. Defaults to NULL.
#' @param answered_by Returns a tibble with all answered questions in the House of Commons by the given MP. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on all answered questions in the House of Commons.
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(answering_department = 'health', answered_by = '4019')
#'
#' x <- commons_answered_questions(start_date = '2017-03-26', end_date='2017-04-01')
#'
#' }


commons_answered_questions <- function(answering_department = NULL, answered_by = NULL, start_date = "1900-01-01", end_date = Sys.Date(),
    extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    dates <- paste0("&max-dateOfAnswer=", as.Date(end_date), "&min-dateOfAnswer=", as.Date(start_date))

    if (is.null(answered_by) == FALSE) {
        answered_by <- paste0("&answeringMember=http://data.parliament.uk/members/", answered_by)
    }

    if (is.null(answering_department) == FALSE) {
        query <- "/answeringdepartment"
        answering_department <- paste0("q=", answering_department)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions"

    message("Connecting to API")

    answered <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500", dates,
        extra_args), flatten = TRUE)

    jpage <- floor(answered$result$totalResults/answered$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500&_page=",
            i, dates, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$dateOfAnswer._value <- as.POSIXct(df$dateOfAnswer._value)

            df$dateOfAnswer._datatype <- "POSIXct"

            df$answeringMember._about <- gsub("http://data.parliament.uk/members/", "", df$answeringMember._about)

            df$AnsweringBody <- unlist(df$AnsweringBody)

            df <- hansard::hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }

}
