
#' Imports data on TV broadcasts. To import information on TV channel options,
#' @param legislature Accepts one of either 'commons' or 'lords'. If NULL, returns all TV programmes for all chambers.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on TV broadcasts.
#' @keywords TV
#' @export
#' @examples \dontrun{
#'
#' x <- tv_programmes('commons', start_date ='2016-11-01')
#'
#' }

tv_programmes <- function(legislature = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    dates <- paste0("&max-endDate=", as.Date(end_date), "T23:59:59Z", "&min-startDate=", as.Date(start_date), "T00:00:00Z")

    if (is.null(legislature) == FALSE) {
        legislature <- tolower(legislature)
    }

    if (is.null(legislature) == TRUE) {
        query <- NULL
    } else if (legislature == "commons") {
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

    jpage <- floor(tv$result$totalResults/tv$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$startDate._value <- gsub("T", " ", df$startDate._value)

            df$startDate._value <- lubridate::parse_date_time(df$startDate._value, "Y-m-d H:M:Sz!*")

            df$startDate._datatype <- "POSIXct"

            df$endDate._value <- gsub("T", " ", df$endDate._value)

            df$endDate._value <- lubridate::parse_date_time(df$endDate._value, "Y-m-d H:M:Sz!*")

            df$endDate._datatype <- "POSIXct"

            df$legislature <- dplyr::bind_rows(df$legislature)

            df$legislature.prefLabel._value <- df$legislature$prefLabel._value

            df$legislature_about <- df$legislature$`_about`

            df$legislature_about <- gsub("http://data.parliament.uk/terms/", "", df$legislature_about)

            df$legislature <- NULL

            df <- hansard::hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}



#' tv_clips
#'
#' Imports data on TV broadcasts. To import information on TV channel options,
#' @param mp_id Accepts the ID of an MP or peer, and returns all clips featuring that MP or peer. If NULL, returns data on all available clips. Defaults to NULL.
#' @return A tibble with details on TV broadcasts featuring the given MP, or all available clips.
#' @keywords TV
#' @export
#' @rdname tv_programmes
#' @examples \dontrun{
#' x <- tv_clips(4591)
#' }

tv_clips <- function(mp_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    dates <- paste0("&max-startDate=", as.Date(end_date), "T00:00:00Z", "&min-startDate=", as.Date(start_date), "T00:00:00Z")

    if (is.null(mp_id) == FALSE) {
        query <- paste0("&member=http://data.parliament.uk/members/", mp_id)
    } else {
        query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

    tv <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    jpage <- floor(tv$result$totalResults/tv$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- as.data.frame(dplyr::bind_rows(pages))

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        if (tidy == TRUE) {

          for(i in 1:nrow(df)){

            if(is.null(df$member[[i]])==FALSE){

              df$member[[i]] <- hansard_tidy(df$member[[i]], tidy_style)

              df$member_about <- NA
              df$member_label_value <- NA

              df$member_about <- df$member[[i]]$about

              df$member_label_value <- df$member[[i]]$label_value

              df$member_about <- gsub("http://data.parliament.uk/terms/", "", df$member_about)

              df$member <- NULL

            }
          }

            df <- tibble::as.tibble(hansard::hansard_tidy(df, tidy_style))

            df

        } else {

            df <- tibble::as.tibble(df)

            df

        }
    }
}


#' tv_channels
#'
#' Imports data on TV broadcasts
#' @rdname tv_programmes
#' @return A tibble with details on the different broadcasting channels.
#' @keywords TV
#' @export

tv_channels <- function(tidy = TRUE, tidy_style = "snake_case") {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/tvchannels.json?_pageSize=500", flatten = TRUE)

    df <- tibble::as_tibble(x$result$items)

    if (tidy == TRUE) {

        df <- hansard::hansard_tidy(df, tidy_style)

        df

    } else {



        df

    }


}
