

#' House publications
#'
#' Imports data on House of Commons and House of Lords publications.
#' @param ID Publication ID. Defaults to \code{NULL}. If not NULL, requests a tibble with information on the given publication.
#' @param house The house that produced the particular publication. Accepts 'commons' and 'lords'. If NULL or not 'commons' or 'lords', returns publications from both House of Commons and House of Lords. This parameter is case-insensitive. Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#'
### @keywords Publication Logs
#' @export
#' @examples \dontrun{
#'
#' x <- publication_logs(house='commons')
#'
#' x <- publication_logs(683267)
#' }

publication_logs <- function(ID = NULL, house = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

    if (is.null(ID) == FALSE) {
        query <- paste0("/", ID, ".json?")
    } else {
        query <- ".json?&_pageSize=500"
    }

    if (is.null(house) == FALSE) {
        house <- tolower(house)
        if (house == "commons") {
            house_query <- "&legislature.prefLabel=House of Commons"
            house_query <- utils::URLencode(house_query)
        } else if (house == "lords") {
            house_query <- "&legislature.prefLabel=House of Lords"
            house_query <- utils::URLencode(house_query)
        } else {
            house_query <- NULL
        }
    } else {
        house_query <- NULL
    }

    dates <- paste0("&_properties=publicationDate&max-publicationDate=", as.Date(end_date), "&min-publicationDate=", as.Date(start_date))

    baseurl <- "http://lda.data.parliament.uk/publicationlogs"

    if(verbose==TRUE){message("Connecting to API")}

    logs <- jsonlite::fromJSON(paste0(baseurl, query, house_query, dates, extra_args), flatten = TRUE)

    if (is.null(ID) == FALSE) {

        df <- tibble::as_tibble(as.data.frame(logs$result$primaryTopic))

    } else {

        jpage <- floor(logs$result$totalResults/logs$result$itemsPerPage)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, house_query, dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- pub_tidy(df, tidy_style) ## in utils-publogs.R

        }

            df

    }
}


#' @rdname publication_logs
#' @export
hansard_publication_logs <- function(ID = NULL, house = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE,  tidy_style = "snake_case", verbose=FALSE) {

  df <- publication_logs(ID=ID, house=house, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
