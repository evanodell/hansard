

#' General and By-Elections
#'
#' @description Imports data on general and by-elections from the
#' 2010 General Election onwards.
#'
#' @description If both `ID` and `type` are used to query the API,
#' `ID` takes precedence and `type` is ignored.
#'
#' @param ID Accepts an ID for a general or by-election from the 2010 General
#' Election onwards, and returns the date and type of the elction.
#' If `NULL`, returns the date and type of all available elections,
#' subject to other parameters. Defaults to `NULL`.
#'
#' @param type Accepts `'General Election'` or `'By-election'` as
#' arguments if ID is `NULL`, and returns all General Elections or all
#' By-elections, as specified, subject to other parameters.
#' Defaults to `NULL`.
#'
#' @param start_date Only includes elections held on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date Only includes elections held on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#'
#' @param label Label of the election. By-elections are in
#' `'dd-mmm-yyyy By-election'` format;
#' e.g. `'23-Feb-2017 By-election'`, and general elections use
#' `'YYYY General Election'` format. The parameter cannot search,
#' so check your formatting, spelling and make sure there were actually
#' elections with the label specified. If `NULL`, returns all
#' Elections/By-elections subject to other parameters.
#' Defaults to `NULL`.
#' @inheritParams all_answered_questions
#' @return A tibble with details on all elections from the 2010 General
#' Election onwards, subject to function parameters. Includes the election
#' ID, the date, and the type of election(s).
#'
#' @seealso [election_results()]
#' @seealso [election_candidates()]
#' @export
#' @examples
#' \dontrun{
#' x <- elections(517994)
#' }
#'
elections <- function(ID = NULL, type = NULL, start_date = "1900-01-01",
                      end_date = Sys.Date(), label = NULL, extra_args = NULL,
                      tidy = TRUE, tidy_style = "snake", verbose = TRUE) {
  dates <- paste0(
    "&max-date=", as.Date(end_date),
    "&min-date=", as.Date(start_date)
  )

  if (!is.null(label)) {
    label <- utils::URLencode(paste0("&label=", label))
  }

  baseurl <- paste0(url_util, "elections")

  veb(verbose)

  if (!is.null(ID)) {
    ID <- paste0("/", ID, ".json?")

    elect <- jsonlite::fromJSON(paste0(
      baseurl, ID, dates,
      label, extra_args
    ),
    flatten = TRUE
    )

    df <- elect$result$primaryTopic

    df <- tibble::as_tibble(as.data.frame(df))
  } else {
    type_query <- ifelse(
      !is.null(type),
      utils::URLencode(
        paste0(".json?&electionType=", type)
      ),
      ".json?"
    )

    query <- paste0(
      baseurl, type_query, dates, label,
      extra_args
    )



    df <- loop_query(query, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- elections_tidy(df, tidy_style) ## in utils-elections.R
    }

    df
  }
}

#' @rdname elections
#' @export
hansard_elections <- elections
