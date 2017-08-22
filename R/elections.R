

#' General and By-Elections
#'
#' Imports data on general and by-elections from the 2010 General Election onwards.
#' @param ID Accepts an ID for a general or by-election from the 2010 General Election onwards, and returns the date and type of the elction. If \code{NULL}, returns the date and type of all available elections. Defaults to \code{NULL}.
#' @param type Accepts \code{'General Election'} or \code{'By-election'} as arguments if ID is \code{NULL}, and returns all General Elections or all By-elections, as specified.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @param label Label of the election. By-elections are in \code{'dd-mmm-yyyy By-election'} format; e.g. \code{'23-Feb-2017 By-election'}, and general elections use \code{'YYYY General Election'} format. The parameter cannot search, so check your format, spelling and make sure there were actually elections with the label specified. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return  A tibble with details on all elections from the 2010 general election onwards, subject to function parameters. Includes the election ID, the date, and the type of election(s).
#'
### @keywords Elections
#' @seealso \code{\link{election_results}}
#' @seealso \code{\link{election_candidates}}
#' @export
#' @examples \dontrun{
#'
#' x <- elections(517994)
#' }


elections <- function(ID = NULL, type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), label = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (is.null(label) == FALSE) {
        label <- utils::URLencode(paste0("&label=", label))
    }

    if (is.null(ID) == FALSE) {

        ID <- paste0("/", ID, ".json?")

        baseurl <- "http://lda.data.parliament.uk/elections"

        if(verbose==TRUE){message("Connecting to API")}

        elect <- jsonlite::fromJSON(paste0(baseurl, ID, dates, label), flatten = TRUE)

        df <- elect$result$primaryTopic

        df <- tibble::as_tibble(as.data.frame(df))

    } else {

        if (is.null(type) == FALSE) {

            type_query <- paste0("&electionType=", type)
            type_query <- utils::URLencode(type_query)
        } else {
            type_query <- NULL
        }

        ID <- ".json?&_pageSize=500"

        baseurl <- "http://lda.data.parliament.uk/elections"

        if(verbose==TRUE){message("Connecting to API")}

        elect <- jsonlite::fromJSON(paste0(baseurl, ID, type_query, dates, label), flatten = TRUE)

        df <- tibble::as_tibble(elect$result$items)

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

          df <- elections_tidy(df, tidy_style)

        }

            df

    }

}

#' @rdname elections
#' @export
hansard_elections <- function(ID = NULL, type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), label = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- elections(ID = ID, type = type, start_date = start_date, end_date = end_date, label = label, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
