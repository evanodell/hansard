
#' Imports data on House of Commons divisions.
#'
#' @param division_id The id of a particular vote. If empty, returns a tibble with information on all commons divisions, subject to all other parameters. Defaults to NULL.
#' @param summary If TRUE, returns a small tibble summarising a division outcome. Otherwise returns a tibble with details on how each MP voted. Has no effect if `division_id` is empty. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to current system date. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Removes extra URL data from voting type columns. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with the results of divisions in the House of Commons.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_divisions()
#'
#' x <- commons_divisions(division_id = 694163, summary = FALSE)
#'
#' }

commons_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (is.null(division_id) == TRUE) {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, extra_args), flatten = TRUE)

        jpage <- floor(divis$result$totalResults/divis$result$itemsPerPage)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else if (is.null(division_id) == FALSE) {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json?", dates, extra_args), flatten = TRUE)

        if (summary == TRUE) {

            df <- divis$result$primaryTopic

            df$AbstainCount <- df$AbstainCount$`_value`
            df$AyesCount <- df$AyesCount$`_value`
            df$Didnotvotecount <- df$Didnotvotecount$`_value`
            df$Errorvotecount <- df$Errorvotecount$`_value`
            df$Noesvotecount <- df$Noesvotecount$`_value`
            df$Noneligiblecount <- df$Noneligiblecount$`_value`
            df$vote <- NULL
            df$Margin <- df$Margin$`_value`
            df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
            df$date <- df$date$`_value`

            df <- tibble::as_tibble(df)

        } else {

            df <- tibble::as_tibble(divis$result$primaryTopic$vote)

        }

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            if (is.null(division_id) == TRUE) {

                df$date._datatype <- "POSIXct"

                df$date._value <- as.POSIXct(df$date._value)

            } else {

                if (summary == TRUE) {

                  df$date <- as.POSIXct(df$date)

                } else {

                  df$`_about` <- gsub("http://data.parliament.uk/resources/", "", df$`_about`)

                  names(df)[names(df) == "_about"] <- "voteId"

                  df <- tidyr::unnest(df)

                  df$type <- gsub("http://data.parliament.uk/schema/parl#", "", df$type)

                  df$type <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", df$type)

                  if (tidy_style == "camelCase") {

                    df$type <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", df$type, perl = TRUE)

                    substr(df$type, 1, 1) <- tolower(substr(df$type, 1, 1))

                  } else if (tidy_style == "period.case") {

                    df$type <- gsub("_", ".", df$type)

                    df$type <- tolower(df$type)

                  } else {

                    df$type <- tolower(df$type)

                  }
                }
            }

            df <- hansard_tidy(df, tidy_style)

            df$about <- gsub("http://data.parliament.uk/members/", "", df$about)

            df


        } else {
            df
        }
    }
}



#' @rdname commons_divisions
#' @export

hansard_commons_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- commons_divisions(division_id = division_id, summary = summary, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}
