
#' Imports data on general election and by-election results from the 2010 general election onwards.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the results. If NULL, returns all available election results. Defaults to NULL.
#' @param all_data If TRUE, returns vote share for all parties standing in any constituency in the election/elections returned. Defaults to FALSE. Note that aside from shorthand for the Conservatives, Labour, Liberal Democrat and Independent (Con, Lab, Lib and Ind, respectively) being converted to their full names, party names are not tidied, so will contain spaces in the case of parties with multiple words in their name, such as the Liberal Democrats. If a party did not stand in a constituency its vote count is listed as NA. There is a drawback to using this parameter, as multiple candidates from the same party in a constituency, or multiple independent candidates, have their vote totals combined.
#' @param calculate_percent If TRUE, calculates the turnout percentage for each constituency in the tibble and the majority of the winning candidate to one decimal place, and includes this information in the tibble in additional columns labelled 'turnout_percentage' and 'majority_percentage'. Defaults to FALSE.
#' @param constit_details If TRUE, returns additional details on each constituency, including its GSS (Government Statistical Service) code. Defaults to FALSE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#'
#' @return A tibble with the results of all general and by-elections, or of a specified general election or by-election.
#' @keywords Election Results
#' @seealso \code{\link{elections}} \code{\link{election_candidates}}
#' @export
#' @examples \dontrun{
#'
#' x <- election_results(ID=382037)
#'
#' y <- election_results()
#'
#' z <- election_results(calculate_percent = TRUE, constit_details = TRUE)
#'
#' w <- election_results(ID=730039, all_data=TRUE)
#'
#' }

election_results <- function(ID = NULL, all_data = FALSE, calculate_percent = FALSE, constit_details = FALSE, extra_args = NULL,
    tidy = TRUE, tidy_style = "snake_case") {

    if (is.null(ID) == TRUE) {
        id_query <- NULL
    } else {
        id_query <- paste0("electionId=", ID)
    }

    baseurl <- "http://lda.data.parliament.uk/electionresults.json?"

    message("Connecting to API")

    elect <- jsonlite::fromJSON(paste0(baseurl, id_query, "&_pageSize=500", extra_args))

    jpage <- floor(elect$result$totalResults/elect$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, id_query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    if (constit_details == TRUE & (nrow(df) > 0)) {

        message("Retrieving constituency information")

        constits <- suppressMessages(constituencies(current = FALSE))

        df <- dplyr::left_join(df, constits, by = c(constituency._about = "about"))
    }

    if (all_data == TRUE) {

        names(df)[names(df) == "_about"] <- "about"

        dat <- vector("list", nrow(df))

        df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

        for (i in 1:nrow(df)) {

            x <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/electionresults/", df$about[[i]], ".json"), flatten = TRUE)

            dat[[i]] <- x$result$primaryTopic$candidate

            message("Retrieving ", i, " of ", nrow(df), ": ", df$constituency.label._value[[i]], ", ", df$election.label._value[[i]])

        }

        df2 <- dplyr::bind_rows(dat)

        df2$fullName._value <- NULL
        df2$order <- NULL

        names(df2)[names(df2) == "_about"] <- "about"

        df3 <- tidyr::spread(df2, df2$party._value, df2$numberOfVotes)

        df3$about <- gsub("http://data.parliament.uk/resources/", "", df3$about)
        df3$about <- gsub("/.*", "", df3$about)

        df3 <- dplyr::group_by(df3, df3$about)

        df4 <- dplyr::summarise_all(df3, sum, na.rm = TRUE)

        df4[df4 == 0] <- NA

        names(df4)[names(df4)=="Con"] <- "Conservative"
        names(df4)[names(df4)=="Lab"] <- "Labour"
        names(df4)[names(df4)=="Lib"] <- "Liberal Democrat"
        names(df4)[names(df4)=="Ind"] <- "Independent"

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

            if (all_data == TRUE) {

                df <- dplyr::left_join(df, df4, by = "about")

            }

            df

        } else {

            df <- tibble::as.tibble(df)

            if (all_data == TRUE) {

                df <- dplyr::left_join(df, df4, by = "about")

            }

            df

        }
    }
}
