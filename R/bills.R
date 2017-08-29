

#' Bill data
#'
#' Imports data on House of Commons and House of Lords bills.
#'
#' @param ID The ID of a given bill to return data on. If \code{NULL}, returns all bills, subject to other parameters. Defaults to \code{NULL}.
#' @param amendments If \code{TRUE}, returns all bills with amendments. Defaults to \code{FALSE}.
#' @param start_date Only includes bills introduced on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes bills introduced on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on bills before the House of Lords and the House of Commons.
### @keywords bills
#' @seealso \code{\link{bill_stage_types}}
#' @export
#' @examples \dontrun{
#' x <- bills()
#'
#' x <- bills(amendments=TRUE)
#'
#' x <- bills(1719)
#'
#' x <- bills(start_date ='2016-01-01')
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
        query <- "withamendments.json?"
    } else {
        query <- ".json?_pageSize=500"
    }

    if(verbose==TRUE){message("Connecting to API")}

    bills <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args), flatten = TRUE)

    jpage <- floor(bills$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args, "&_page=", i,"&_pageSize=500"), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- bills_tidy(df, tidy_style)### in utils-bills.R

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
