

#' Imports data on House of Commons and House of Lords bills
#'
#' @param ID The ID of a given bill to return data on. If NULL, returns all bills, subject to other parameters. Defaults to NULL.
#' @param amendments If TRUE, returns all bills with amendments. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If TRUE, returns data to console on the progress of the API request. Defaults to FALSE.
#' @return  A tibble with details on bills before the House of Lords and the House of Commons.
#' @keywords bills
#' @seealso \code{\link{bill_stage_types}}
#' @export
#' @examples \dontrun{
#'
#' x <- bills()
#'
#' x <- bills(amendments=TRUE)
#'
#' x <- bills(1719)
#'
#' x <- bills(start_date ='2016-01-01')
#'
#' }

bills <- function(ID = NULL, amendments = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (is.null(ID) == FALSE) {
        id_query <- paste0("&identifier=", ID)
    } else {
        id_query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/bills"

    if (amendments == TRUE) {
        query <- "withamendments.json?_pageSize=500"
    } else {
        query <- ".json?_pageSize=500"
    }

    if(verbose==TRUE){message("Connecting to API")}

    bills <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args), flatten = TRUE)

    jpage <- floor(bills$result$totalResults/bills$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args, "&_page=", i), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$date._value <- as.POSIXct(df$date._value)

            df$date._datatype <- "POSIXct"

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}






#' @rdname bills
#' @export

hansard_bills <- function(ID = NULL, amendments = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- bills(ID = ID, amendments = amendments, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df


}
