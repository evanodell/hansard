
#' TV broadcast data
#'
#' Imports data on TV broadcasts, clips of individual members and parliamentary TV channels.
#' @param legislature Accepts one of either \code{'commons'} or \code{'lords'}. If \code{NULL}, returns all TV programmes for all chambers.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on TV broadcasts.
#'
#' @section TV Programme functions:
#' \describe{
#' \item{tv_programmes}{TV programmse broadcast, per legislature and date}
#' \item{tv_clips}{Clips of a given MP or Peer}
#' \item{tv_channels}{Details on the different parliamentary TV channels}
#' }
#' @export
#' @examples \dontrun{
#' x <- tv_programmes('commons', start_date ='2016-11-01', end_date='2016-12-01')
#' }

tv_programmes <- function(legislature = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&max-endDate=", as.Date(end_date), "T23:59:59Z", "&min-startDate=", as.Date(start_date), "T00:00:00Z")

    if (is.null(legislature) == FALSE) {
        legislature <- tolower(legislature)
    }

    if (is.null(legislature) == TRUE) {
      query <- NULL
    } else if (legislature == "commons") {

        query <- utils::URLencode("&legislature.prefLabel=House of Commons")

    } else if (legislature == "lords") {

        query <- utils::URLencode("&legislature.prefLabel=House of Lords")

    } else {

        query <- NULL

    }

    baseurl <- "http://lda.data.parliament.uk/tvprogrammes.json?"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    if(verbose==TRUE){message("Connecting to API")}

    jpage <- floor(tv$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- tv_tidy(df, tidy_style) ### in utils-tv.R

        }

            df

    }
}

#' @export
#' @rdname tv_programmes
hansard_tv_programmes <- function(legislature = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- tv_programmes(legislature = legislature,start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}




#' @param mp_id Accepts the ID of an MP or peer, and returns all clips featuring that MP or peer. If \code{NULL}, returns data on all available clips. Defaults to \code{NULL}.
#' @return A tibble with details on TV broadcasts featuring the given MP, or all available clips.
#'
#' @export
#' @rdname tv_programmes
#' @examples \dontrun{
#' x <- tv_clips(4591)
#' }

tv_clips <- function(mp_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    dates <- paste0("&max-startDate=", as.Date(end_date), "T00:00:00Z", "&min-startDate=", as.Date(start_date), "T00:00:00Z")

    if (is.null(mp_id) == FALSE) {
        query <- paste0("&member=http://data.parliament.uk/members/", mp_id)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/tvclips.json?"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    jpage <- floor(tv$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- as.data.frame(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        if (tidy == TRUE) {

            df <- tv_tidy2(df, mp_id, tidy_style)

        }

            df <- tibble::as.tibble(df)

            df

        }

}


#' @export
#' @rdname tv_programmes
hansard_tv_clips <- function(mp_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- tv_clips(mp_id = mp_id,start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}


#' @rdname tv_programmes
#' @return A tibble with details on the different broadcasting channels.
#'
### @keywords TV
#' @export

tv_channels <- function(tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/tvchannels.json?_pageSize=50", flatten = TRUE)

    df <- tibble::as_tibble(x$result$items)

    if (tidy == TRUE) {

        df <- hansard_tidy(df, tidy_style)

    }

        df

}

#' @export
#' @rdname tv_programmes
hansard_tv_channels <- function(tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- tv_channels(tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df
}
