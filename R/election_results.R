

#' Imports data on general election and by-election results
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the results. If NULL, returns all available election results. Defaults to NULL.
#' @param calculate_percent If TRUE, calculates the turnout percentage for each constituency in the tibble and the majority of the winning candidate to one decimal place, and includes this information in the tibble in additional columns labelled 'turnout_percentage' and 'majority_percentage'. Defaults to FALSE.
#' @param constit_details If TRUE, returns additional details on each constituency, including its GSS (Government Statistical Service) code. Defaults to FALSE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#'
#' @return A tibble with the results of all general and by-elections, or of a specified general election or by-election.
#' @keywords Election Results
#' @export
#' @examples \dontrun{
#'
#' x <- election_results(ID=382037)
#'
#' }

election_results <- function(ID = NULL, calculate_percent = FALSE, constit_details = FALSE, extra_args = NULL, tidy = TRUE) {

    baseurl <- "http://lda.data.parliament.uk/electionresults.json?electionId="

    message("Connecting to API")

    elect <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500", extra_args))

    if (elect$result$totalResults > elect$result$itemsPerPage) {

        jpage <- round(elect$result$totalResults/elect$result$itemsPerPage, digits = 0)
    } else {
        jpage <- 0
    }

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (constit_details == TRUE) {

        constits <- constituencies(current = FALSE)  ### combine with elect 2015 to get gss code

        df <- left_join(constits, df, by = c(about = "constituency_about"))  ## Join
    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

        }

        if (calculate_percent == TRUE) {

            df$turnout_percentage <- round((df$turnout/df$electorate) * 100, digits = 1)

            df$majority_percentage <- round((df$majority/df$turnout) * 100, digits = 1)

            df

        } else {

            df

        }
    }
}
