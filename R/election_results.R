

#' Imports data on general election and by-election results from the 2010 general election onwards.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the results. If NULL, returns all available election results. Defaults to NULL.
#' @param calculate_percent If TRUE, calculates the turnout percentage for each constituency in the tibble and the majority of the winning candidate to one decimal place, and includes this information in the tibble in additional columns labelled 'turnout_percentage' and 'majority_percentage'. Defaults to FALSE.
#' @param constit_details If TRUE, returns additional details on each constituency, including its GSS (Government Statistical Service) code. Defaults to FALSE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#'
#' @return A tibble with the results of all general and by-elections, or of a specified general election or by-election.
#' @keywords Election Results
#' @export
#' @examples \dontrun{
#'
#' x <- election_results(ID=382037)
#'
#' y <- election_results()
#'
#' }

election_results <- function(ID = NULL, calculate_percent = FALSE, constit_details = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    baseurl <- "http://lda.data.parliament.uk/electionresults.json?electionId="

    message("Connecting to API")

    elect <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500", extra_args))

    if (elect$result$totalResults > elect$result$itemsPerPage) {

        jpage <- round(elect$result$totalResults/elect$result$itemsPerPage, digits = 0)-1
    } else {
        jpage <- 0
    }

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    if (constit_details == TRUE) {

        constits <- constituencies(current = FALSE)

        df <- dplyr::left_join(df, constits, by = c("constituency._about" = "about"))
    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (calculate_percent == TRUE) {

            df$turnout_percentage <- round((df$turnout/df$electorate) * 100, digits = 1)

            df$majority_percentage <- round((df$majority/df$turnout) * 100, digits = 1)

        }

        if (tidy == TRUE) {

            df$election._about <- gsub("http://data.parliament.uk/resources/", "", df$election._about)

            df$constituency._about <- gsub("http://data.parliament.uk/resources/", "", df$constituency._about)

            df <- tibble::as.tibble(hansard::hansard_tidy(df, tidy_style))

        } else {

            df <- tibble::as.tibble(df)

        }
    }
}
