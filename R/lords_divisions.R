
#' Imports data on House of Lords divisions
#' @param division_id The id of a particular vote. If empty, returns a tibble with information on all lords divisions. Defaults to NULL.
#' @param summary If TRUE, returns a small tibble summarising a division outcome. Otherwise returns a tibble with details on how each peer voted. Has no effect if `division_id` is empty. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble, if calling all divisions. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble, if calling all divisions. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with the results of divisions in the House of Lords.
#' @keywords Lords Divisions
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

lords_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL,  tidy = TRUE) {

    dates <- paste0("&_properties=date&max-date=", end_date, "&min-date=", start_date)

    if (is.null(division_id)==TRUE) {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, extra_args))

        jpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else {

        division_id <- as.character(division_id)

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json?", dates, extra_args), flatten = TRUE)

        if (summary == TRUE) {

            y <- divis$result$primaryTopic

            df <- list()

            df$about <- y$`_about`
            df$title <- y$title
            df$description <- y$description
            df$contents_count <- y$officialContentsCount
            df$not_contents_ount <- y$officialNotContentsCount
            df$division_number <- y$divisionNumber
            df$division_result <- y$divisionResult
            df$date <- y$date
            df$session <- y$session
            df$uin <- y$uin

            df <- as.data.frame(df)

        } else {

            df <- divis$result$primaryTopic$vote

            df <- tibble::as_tibble(as.data.frame(df))

        }

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df <- tibble::as_tibble(df)

            df

        } else {

            df <- tibble::as_tibble(df)

            df

        }

    }
}
