
#' Election candidate details
#'
#' Returns the name and party of all candidates standing in an election, for
#' each constituency. Note that for general elections this can return a very
#' large tibble with hundreds of variables.
#'
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 General
#' Election onwards, and returns the results. If `NULL`, returns all
#' available election results. Defaults to `NULL`.
#'
#' @param constit_details If `TRUE`, returns additional details on each
#' constituency, including its GSS (Government Statistical Service) code.
#' Defaults to `FALSE`.
#' @inheritParams all_answered_questions
#' @export
#' @seealso [elections()]
#' @seealso [election_results()]
#'
#' @examples
#' \dontrun{
#' x <- election_candidates(ID = 382037)
#'
#' y <- election_candidates()
#'
#' z <- election_candidates(constit_details = TRUE)
#' }
#'
election_candidates <- function(ID = NULL, constit_details = FALSE,
                                extra_args = NULL, tidy = TRUE,
                                tidy_style = "snake", verbose = TRUE) {
  id_query <- ifelse(
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

  jpage <- floor(elect$result$totalResults / 100)

  query <- paste0(baseurl, id_query, extra_args)

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  names(df)[names(df) == "_about"] <- "election_about"

  dat <- vector("list", nrow(df))

  df$election_about <- gsub(
    "http://data.parliament.uk/resources/", "",
    df$election_about
  )

  seq_list <- seq(from = 1, to = nrow(df), by = 1)

  for (i in seq_along(seq_list)) {
    x <- jsonlite::fromJSON(
      paste0(
        "http://lda.data.parliament.uk/electionresults/",
        df$election_about[[i]], ".json"
      ),
      flatten = TRUE
    )

    df2 <- x$result$primaryTopic$candidate

    names(df2)[names(df2) == "_about"] <- "election_about"

    df2$election_about <- gsub(
      "http://data.parliament.uk/resources/",
      "", df2$election_about
    )

    df2$election_about <- gsub("/.*", "", df2$election_about)

    dat[[i]] <- df2

    if (verbose == TRUE) {
      message(
        "Retrieving ", i, " of ", nrow(df), ": ",
        df$constituency.label._value[[i]], ", ",
        df$election.label._value[[i]]
      )
    }
  }

  df4 <- dplyr::bind_rows(dat)

  if (constit_details == TRUE & (nrow(df) > 0)) {
    message("Retrieving constituency information")

    constits <- suppressMessages(constituencies(current = FALSE))

    df <- dplyr::left_join(df, constits,
      by = c(constituency._about = "about")
    )
  }


  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    df <- tibble::as_tibble(df)

    df <- dplyr::left_join(df, df4, by = "election_about")

    if (tidy == TRUE) {
      df <- elect_can_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname election_candidates
#' @export
hansard_election_candidates <- election_candidates
