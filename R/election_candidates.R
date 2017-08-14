


#' Access election candidate details
#'
#' Returns the name and party of all candidates standing in an election, by constituency.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the results. If NULL, returns all available election results. Defaults to NULL.
#' @param constit_details If TRUE, returns additional details on each constituency, including its GSS (Government Statistical Service) code. Defaults to FALSE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If TRUE, returns data to console on the progress of the API request. Defaults to FALSE.
#' @return A tibble with the names of each candidate standing in each constituency in an election or elections. If there are multiple candidates from the same party, or multiple independent candidates, their names are combined into a list.
#' @export
#' @seealso \code{\link{elections}}
#' @seealso \code{\link{election_results}}
#'
#' @examples \dontrun{
#'
#' x <- election_candidates(ID=382037)
#'
#' y <- election_candidates()
#'
#' z <- election_candidates(constit_details = TRUE)
#'
#'
#' }
#'

election_candidates <- function(ID = NULL, constit_details = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  if (is.null(ID) == TRUE) {
    id_query <- NULL
  } else {
    id_query <- paste0("electionId=", ID)
  }

  baseurl <- "http://lda.data.parliament.uk/electionresults.json?"

  if(verbose==TRUE){message("Connecting to API")}

  elect <- jsonlite::fromJSON(paste0(baseurl, id_query, "&_pageSize=500", extra_args))

  jpage <- floor(elect$result$totalResults/elect$result$itemsPerPage)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, id_query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
    if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
    pages[[i + 1]] <- mydata$result$items
  }

  df <- dplyr::bind_rows(pages)

  if (constit_details == TRUE & (nrow(df) > 0)) {

    message("Retrieving constituency information")

    constits <- suppressMessages(constituencies(current = FALSE))

    df <- dplyr::left_join(df, constits, by = c(constituency._about = "about"))
  }

  names(df)[names(df) == "_about"] <- "about"

  dat <- vector("list", nrow(df))

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  for (i in 1:nrow(df)) {

    x <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/electionresults/", df$about[[i]], ".json"), flatten = TRUE)

    df2 <- x$result$primaryTopic$candidate

    names(df2)[names(df2) == "_about"] <- "about"
    df2$about <- gsub("http://data.parliament.uk/resources/", "", df2$about)
    df2$about <- gsub("/.*", "", df2$about)

    df2 <- stats::aggregate(fullName._value ~ party._value + about, data = df2, c)
    df2$fullName._value <- as.list(df2$fullName._value)

    dat[[i]] <- tidyr::spread_(df2, key_col="party._value", value_col="fullName._value")

    message("Retrieving ", i, " of ", nrow(df), ": ", df$constituency.label._value[[i]], ", ", df$election.label._value[[i]])

  }

  df4 <- dplyr::bind_rows(dat)

  names(df4)[names(df4) == "Con"] <- "Conservative"
  names(df4)[names(df4) == "Lab"] <- "Labour"
  names(df4)[names(df4) == "Lib"] <- "Liberal Democrat"
  names(df4)[names(df4) == "Ind"] <- "Independent"

  df4 <- df4[,order(colnames(df4))]

  if (nrow(df) == 0 && verbose==TRUE) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      df$election._about <- gsub("http://data.parliament.uk/resources/", "", df$election._about)

      df$constituency._about <- gsub("http://data.parliament.uk/resources/", "", df$constituency._about)

      df <- hansard_tidy(df, tidy_style)

    }

    df <- tibble::as.tibble(df)

    df <- dplyr::left_join(df, df4, by = "about")

    df

  }
}


#' @rdname election_candidates
#' @export
hansard_election_candidates <- function(ID = NULL, constit_details = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- election_candidates(ID = ID, constit_details = constit_details, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
