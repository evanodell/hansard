

#' tv_programmes
#'
#' Imports data on TV broadcasts. To import information on TV channel options,
#' @param legislature Accepts one of either 'commons' or 'lords'. If NULL, returns all TV programmes for all chambers.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords TV
#' @export
#' @examples \dontrun{
#'
#' x <- tv_programmes('commons')
#'
#' }

tv_programmes <- function(legislature = NULL, extra_args = NULL) {

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

    tv <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

    message("Connecting to API")

    jpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
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

tv_clips <- function(mp_id = NULL, extra_args = NULL) {

    if (is.null(mp_id) == FALSE) {
        query <- paste0("&member=http://data.parliament.uk/members/", mp_id)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

    jpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


#' tv_channels
#'
#' Imports data on TV broadcasts
#' @rdname tv_programmes
#' @keywords TV
#' @export

tv_channels <- function() {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/tvchannels.json?_pageSize=500", flatten = TRUE)

    df <- x$result$items

    df

}



