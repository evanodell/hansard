

#' tv_programmes
#'
#' Imports data on TV broadcasts. To import information on TV channel options,
#' @param legislature Accepts one of either 'commons' or 'lords'. If NULL, returns all TV programmes for all chambers.
#' @param start_date The earliest date to include in the data frame, if calling all divisions, using the date the question was tabled. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame, if calling all divisions, using the date the question was tabled. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords TV
#' @export
#' @examples \dontrun{
#'
#' x <- tv_programmes('commons', start_date="2016-11-01")
#'
#' }

tv_programmes <- function(legislature = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    dates <- paste0("&max-endDate=", end_date, "T23:59:59Z", "&min-startDate=", start_date, "T00:00:00Z")

    if (is.null(legislature) == FALSE) {
        legislature <- tolower(legislature)
    }

    if (legislature == "commons") {
        query <- "&legislature.prefLabel=House of Commons"
        query <- utils::URLencode(query)
    } else if (legislature == "lords") {
        query <- "&legislature.prefLabel=House of Lords"
        query <- utils::URLencode(query)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    message("Connecting to API")

    jpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

    if (tidy == TRUE) {

      df <- hansard_tidy(df)

    } else {

      df

    }

    }
}



#' tv_clips
#'
#' Imports data on TV broadcasts. To import information on TV channel options,
#' @param mp_id Accepts the ID of an MP or peer, and returns all clips featuring that MP or peer. If NULL, returns data on all available clips. Defaults to NULL.
#' @keywords TV
#' @export
#' @rdname tv_programmes
#' @examples \dontrun{
#' x <- tv_clips(4591)
#' }

tv_clips <- function(mp_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy=TRUE) {

    dates <- paste0("&max-startDate=", end_date, "T00:00:00Z", "&min-startDate=", start_date, "T00:00:00Z")

    if (is.null(mp_id) == FALSE) {
        query <- paste0("&member=http://data.parliament.uk/members/", mp_id)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    jpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
      if (tidy == TRUE) {

        df <- hansard_tidy(df)

      } else {

        df

      }
    }
}


#' tv_channels
#'
#' Imports data on TV broadcasts
#' @rdname tv_programmes
#' @keywords TV
#' @export

tv_channels <- function(tidy=TRUE) {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/tvchannels.json?_pageSize=500", flatten = TRUE)

    df <- x$result$items

    if (tidy == TRUE) {

      df <- hansard_tidy(df)

    } else {

      df

    }


}



