

#' Imports data on early day motions
#' @param edm_id Accepts the ID number of an early day motion, and returns data on that motion. Note that EDM numbers reset each parliamentary session, so using this as the only parameter will return multiple early day motions. Defaults to NULL.
#' @param session Accepts a parliamentary session, in 'yyyy/yy' format. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param signatures The minimum number of signatures required for inclusion in the tibble. Defaults to 1.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with details on the content, signatories and sponsors of all or a specified early day motions.
#' @keywords EDM early day motion
#' @seealso \code{\link{mp_edms}}
#' @export
#' @examples \dontrun{
#'
#' x <- early_day_motions(edm_id = 1073)
#'
#' x <- early_day_motions(edm_id = 1073, session='2015/16')
#'
#' }


early_day_motions <- function(edm_id = NULL, session = NULL, start_date = "1900-01-01", end_date = Sys.Date(), signatures = 1, extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(edm_id) == FALSE) {
        edm_query <- paste0("&edmNumber=", edm_id)
    } else {
        edm_query <- NULL
    }

    if (is.null(session) == FALSE) {
        session_query <- paste0("&session.=", session)
    } else {
        session_query <- NULL
    }

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=",as.Date(start_date))

    sig_min <- paste0("&min-numberOfSignatures=", signatures)

    baseurl <- "http://lda.data.parliament.uk/edms"

    message("Connecting to API")

    edms <- jsonlite::fromJSON(paste0(baseurl, ".json?", edm_query, dates, session_query, "&_pageSize=500", sig_min, extra_args),
        flatten = TRUE)

    jpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?", edm_query, dates, session_query, sig_min, "&_pageSize=500&_page=",
            i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$dateTabled._value <- as.Date(df$dateTabled._value)

            df$dateTabled._datatype <- "Date"

            df <- hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}
