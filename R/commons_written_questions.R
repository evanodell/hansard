
#' Imports data in a tibble on House of Commons written questions.
#' @param mp_id Requests a member ID and returns a tibble with all written questions asked by that member. If NULL, mp_id is not included as a query parameter. Defaults to NULL.
#' @param answering_department Accepts a string with a department name or partial name, and returns all written questions by that department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health. If NULL, answering_department is not included as a query parameter. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble, using the date the question was tabled. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble, using the date the question was tabled. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on written questions in the House of Commons.
#' @keywords House of Commons Written Questions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_written_questions(mp_id=410, 'cabinet office')
#' #Returns a tibble with written questions from Jon Trickett, answered by the Cabinet Office.
#'
#' }

commons_written_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", end_date, "&min-dateTabled=", start_date)

    if (is.null(mp_id) == FALSE) {
        mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)

        mp_id <- utils::URLencode(mp_id)
    }

    if (is.null(answering_department) == FALSE) {

        query <- "/answeringdepartment"

        answering_department <- paste0("q=", answering_department)

        answering_department <- utils::URLencode(answering_department)

    } else {

        query <- NULL

    }

    baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions"

    message("Connecting to API")

    writ <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500", extra_args),
        flatten = TRUE)

    jpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500&_page=",
            i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

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
