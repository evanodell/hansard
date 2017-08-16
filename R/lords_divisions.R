
#'
#' House of Lords Votes
#'
#' Imports data on House of Lords divisions. Either a general query subject to parameters, or the results of a specific division.
#' @param division_id The id of a particular vote. If empty, returns a tibble with information on all lords divisions. Defaults to \code{NULL}.
#' @param summary If \code{TRUE}, returns a small tibble summarising a division outcome. Otherwise returns a tibble with details on how each peer voted. Has no effect if `division_id` is empty. Defaults to \code{FALSE}.
#' @param start_date The earliest date to include in the tibble, if calling all divisions. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble, if calling all divisions. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Removes extra URL data from voting type columns.  Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return  A tibble with the results of divisions in the House of Lords.
#'
### @keywords Lords Divisions
#' @export
#' @examples \dontrun{
#'
#' x <- lords_divisions(division_id = 705891, summary = TRUE)
#'
#' x <- lords_divisions(division_id = 705891, summary = FALSE)
#'
#' # Return all lords divisions in 2016
#' x <- lords_divisions(NULL, FALSE, start_date = '2016-01-01', end_date = '2016-12-31')
#'
#' }

lords_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL,  tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (is.null(division_id) == TRUE) {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions"

        if(verbose==TRUE){message("Connecting to API")}

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?", dates, extra_args))

        jpage <- floor(divis$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else {

        division_id <- as.character(division_id)

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/id/"

        if(verbose==TRUE){message("Connecting to API")}

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json?", dates, extra_args), flatten = TRUE)

        if (summary == TRUE) {

            y <- divis$result$primaryTopic

            df <- list()

            df$about <- y$`_about`
            df$title <- y$title
            df$description <- y$description
            df$officialContentsCount <- y$officialContentsCount
            df$officialNotContentsCount <- y$officialNotContentsCount
            df$divisionNumber <- y$divisionNumber
            df$divisionResult <- y$divisionResult
            df$date <- y$date
            df$session <- y$session
            df$uin <- y$uin

        } else {

            df <- divis$result$primaryTopic

        }

            df <- tibble::as_tibble(as.data.frame(df))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

          df <- lords_division_tidy(df, division_id, summary, tidy_style)

        }

            df

    }
}


#' @rdname lords_divisions
#' @export
hansard_lords_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL,  tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lords_divisions(division_id = division_id, summary = summary, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
