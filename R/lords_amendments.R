
#' House of Lords Amendments.
#'
#' Returns a tibble with all available House of Lords amendments, subject to parameters.
#' @param decision The decision on the amendments. Accepts one of 'Withdrawn', 'Agreed', 'Disagreed', 'Pending', 'NotMoved', 'Disposed'. This parameters is not case sensitive. Defaults to \code{NULL}.
#' @param start_date Only includes amendments to bills introduced on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes amendments to bills introduced on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on amendments proposed by the House of Lords.
#'
#' @export
#' @examples \dontrun{
#' x <- lords_amendments()
#'
#' x <- lords_amendments(decision='Withdrawn')
#' }

lords_amendments <- function(decision = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE,  tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&min-bill.date=", as.Date(start_date), "&max-bill.date=", as.Date(end_date))

    if (is.null(decision) == FALSE) {

        decision_query <- paste0("&decision=", stringr::str_to_title(decision))

    } else {

        decision_query <- NULL

    }

    baseurl <- "http://lda.data.parliament.uk/lordsbillamendments.json?"

    if(verbose==TRUE){message("Connecting to API")}

    ammend <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, extra_args), flatten = TRUE)

    jpage <- floor(ammend$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

          df <- lords_amendments_tidy(df, tidy_style)

        }

          df

    }
}


#' @rdname lords_amendments
#' @export
hansard_lords_amendments <- function(decision = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lords_amendments(decision = decision, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
