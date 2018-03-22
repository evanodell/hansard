
#' Election candidate details
#'
#' Returns the name and party of all candidates standing in an election, for
#' each constituency. Note that for general elections this can return a very
#' large tibble with hundreds of variables.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 General
#' Election onwards, and returns the results. If \code{NULL}, returns all
#' available election results. Defaults to \code{NULL}.
#' @param constit_details If \code{TRUE}, returns additional details on each
#' constituency, including its GSS (Government Statistical Service) code.
#' Defaults to \code{FALSE}.
#' @inheritParams all_answered_questions
#' @export
#' @seealso \code{\link{elections}}
#' @seealso \code{\link{election_results}}
#'
#' @examples \dontrun{
#' x <- election_candidates(ID = 382037)
#'
#' y <- election_candidates()
#'
#' z <- election_candidates(constit_details = TRUE)
#' }
#'

election_candidates <- function(ID = NULL, constit_details = FALSE,
                                extra_args = NULL, tidy = TRUE,
                                tidy_style = "snake_case", verbose = TRUE) {
  id_query <- dplyr::if_else(
    is.null(ID) == FALSE,
    paste0("electionId=", ID),
    ""
  )

  baseurl <- paste0(url_util, "electionresults.json?")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  elect <- jsonlite::fromJSON(paste0(
    baseurl, id_query,
    extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

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

  dat <- vector("list", nrow(df))

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  for (i in 1:nrow(df)) {
    x <- jsonlite::fromJSON(
      paste0(
        "http://lda.data.parliament.uk/electionresults/",
        df$about[[i]], ".json"
      ),
      flatten = TRUE
    )

    df2 <- x$result$primaryTopic$candidate

    names(df2)[names(df2) == "_about"] <- "about"
    df2$about <- gsub(
      "http://data.parliament.uk/resources/",
      "", df2$about
    )
    df2$about <- gsub("/.*", "", df2$about)

    df2 <- stats::aggregate(fullName._value ~ party._value + about,
      data = df2, c
    )

    df2$fullName._value <- as.list(df2$fullName._value)

    dat[[i]] <- tidyr::spread_(df2,
      key_col = "party._value",
      value_col = "fullName._value"
    )

    if (verbose == TRUE) {
      message(
        "Retrieving ", i, " of ", nrow(df), ": ",
        df$constituency.label._value[[i]], ", ",
        df$election.label._value[[i]]
      )
    }
  }

  df4 <- dplyr::bind_rows(dat)

  names(df4)[names(df4) == "Con"] <- "Conservative"
  names(df4)[names(df4) == "Lab"] <- "Labour"
  names(df4)[names(df4) == "Lib"] <- "Liberal Democrat"
  names(df4)[names(df4) == "Ind"] <- "Independent"

  df4 <- df4[, order(colnames(df4))]

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- elect_can_tidy(df, tidy_style)
    }

    df <- tibble::as.tibble(df)

    df <- dplyr::left_join(df, df4, by = "about")

    df
  }
}


#' @rdname election_candidates
#' @export
hansard_election_candidates <- election_candidates
