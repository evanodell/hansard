

#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a tibble.
#' @param answering_department Returns a tibble with all answered questions in the House of Commons from the given department. If NULL, the answering department is not included in the API query. Defaults to NULL.
#' @param answered_by Returns a tibble with all questions in the House of Commons answered by the given MP. If NULL, the answering MP is not included in the API query. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on all answered questions in the House of Commons, subject to function parameters.
#' @keywords bills
#' @seealso \code{\link{all_answered_questions}} \code{\link{commons_oral_questions}} \code{\link{commons_oral_question_times}} \code{\link{commons_written_questions}}  \code{\link{lords_written_questions}} \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(answering_department = 'health', answered_by = '4019')
#'
#' }

commons_answered_questions <- function(answering_department = NULL, answered_by = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    dates <- paste0("&max-dateOfAnswer=", end_date, "&min-dateOfAnswer=", start_date)

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

    answered <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500", dates, extra_args), flatten = TRUE)

    jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500&_page=",
            i, dates, extra_args), flatten = TRUE)
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
