
#' General and By-Election Results
#'
#' Imports results from general and by-elections from the
#' 2010 General Election onwards.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 General
#' Election onwards, and returns the results. If \code{NULL}, returns all
#' available election results. Defaults to \code{NULL}.
#' @param all_data If \code{TRUE}, returns vote share for all parties standing
#' in any constituency in the election/elections returned. Defaults to
#' \code{FALSE}. Note that aside from shorthand for the Conservatives, Labour,
#' Liberal Democrat and Independent (Con, Lab, Lib and Ind, respectively)
#' being converted to their full names, party names are not tidied, so will
#' contain spaces in the case of parties with multiple words in their name,
#' such as the Liberal Democrats. If a party did not stand in a constituency
#' its vote count is listed as NA. There is a drawback to using this parameter,
#' as multiple candidates from the same party in a constituency, or multiple
#' independent candidates, have their vote totals combined.
#' @param calculate_percent If \code{TRUE}, calculates the turnout percentage
#' for each constituency in the tibble and the majority of the winning
#' candidate to one decimal place, and includes this information in the
#' tibble in additional columns labelled 'turnout_percentage' and
#' 'majority_percentage'. Defaults to \code{FALSE}.
#' @param constit_details If \code{TRUE}, returns additional details on each
#' constituency, including its GSS (Government Statistical Service) code.
#' Defaults to \code{FALSE}.
#' @inheritParams all_answered_questions
#' @return A tibble with the results of all general and by-elections, or of
#' a specified general election or by-election.
#'
#' @seealso \code{\link{elections}}
#' @seealso \code{\link{election_candidates}}
#' @export
#' @examples \dontrun{
#' x <- election_results(ID = 382037)
#'
#' y <- election_results()
#'
#' z <- election_results(calculate_percent = TRUE, constit_details = TRUE)
#'
#' w <- election_results(ID = 730039, all_data = TRUE)
#' }

election_results <- function(ID = NULL, all_data = FALSE,
                             calculate_percent = FALSE,
                             constit_details = FALSE,
                             extra_args = NULL, tidy = TRUE,
                             tidy_style = "snake_case", verbose = TRUE) {
  id_query <- dplyr::if_else(is.null(ID), "", paste0("electionId=", ID))

  baseurl <- paste0(url_util, "electionresults.json?")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  elect <- jsonlite::fromJSON(paste0(
    baseurl, id_query,
    extra_args, "&_pageSize=1"
  ))

  jpage <- floor(elect$result$totalResults / 500)

  query <- paste0(baseurl, id_query, extra_args, "&_pageSize=500&_page=")

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (constit_details == TRUE & (nrow(df) > 0)) {
    message("Retrieving constituency information")

    constits <- suppressMessages(constituencies(current = FALSE))

    df <- dplyr::left_join(df, constits,
      by = c(constituency._about = "about")
    )
  }

  names(df)[names(df) == "_about"] <- "about"
  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  if (all_data == TRUE) {
    dat <- vector("list", nrow(df))

    for (i in 1:nrow(df)) {
      x <- jsonlite::fromJSON(
        paste0(
          "http://lda.data.parliament.uk/electionresults/",
          df$about[[i]], ".json"
        ),
        flatten = TRUE
      )

      dat[[i]] <- x$result$primaryTopic$candidate

      if (verbose == TRUE) {
        message(
          "Retrieving ", i, " of ", nrow(df), ": ",
          df$constituency.label._value[[i]], ", ",
          df$election.label._value[[i]]
        )
      }
    }

    df2 <- dplyr::bind_rows(dat)

    df2$fullName._value <- NULL
    df2$order <- NULL

    names(df2)[names(df2) == "_about"] <- "about"

    df3 <- tidyr::spread_(df2, "party._value", "numberOfVotes")

    df3$about <- gsub(
      "http://data.parliament.uk/resources/",
      "", df3$about
    )

    df3$about <- gsub("/.*", "", df3$about)

    df3 <- dplyr::grouped_df(df3, "about")

    df4 <- dplyr::summarise_all(df3, sum, na.rm = TRUE)

    df4[df4 == 0] <- NA

    names(df4)[names(df4) == "Con"] <- "Conservative"
    names(df4)[names(df4) == "Lab"] <- "Labour"
    names(df4)[names(df4) == "Lib"] <- "Liberal Democrat"
    names(df4)[names(df4) == "Ind"] <- "Independent"

    df4 <- df4[, order(colnames(df4))]
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (calculate_percent == TRUE) {
      df$turnout_percentage <- round(
        (df$turnout / df$electorate) * 100,
        digits = 2
      )

      df$majority_percentage <- round(
        (df$majority / df$turnout) * 100,
        digits = 2
      )
    }

    if (tidy == TRUE) {
      df$election._about <- stringi::stri_replace_all_fixed(
        df$election._about, "http://data.parliament.uk/resources/", "",
        vectorize_all = FALSE
      )

      df$constituency._about <- stringi::stri_replace_all_fixed(
        df$constituency._about,
        "http://data.parliament.uk/resources/", "",
        vectorize_all = FALSE
      )

      if (all_data == TRUE) {
        df <- dplyr::left_join(df, df4, by = "about")
      }

      df <- tibble::as.tibble(hansard_tidy(df, tidy_style))
    } else {
      df <- tibble::as.tibble(df)

      if (all_data == TRUE) {
        df <- dplyr::left_join(df, df4, by = "about")
      }
    }

    df
  }
}



#' @rdname election_results
#' @export
hansard_election_results <- election_results
