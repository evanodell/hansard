

#' Papers laid
#'
#' Imports data on papers laid before the House.
#' 
#' @param withdrawn If `TRUE`, only returns withdrawn papers.
#' Defaults to `FALSE`.
#' 
#' @param house The house the paper was laid in. Accepts `'commons'`
#' and `'lords'`. If `NULL`, returns both House of Commons and
#' House of Lords. This parameter is case-insensitive. Defaults to `NULL`.
#' 
#' @param start_date Only includes papers laid before the House on or after
#' this date. Accepts character values in `'YYYY-MM-DD'` format, and
#' objects of class `Date`, `POSIXt`, `POSIXct`, `POSIXlt`
#' or anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#' 
#' @param end_date Only includes papers laid before the House on or before
#' this date. Accepts character values in `'YYYY-MM-DD'` format, and
#' objects of class `Date`, `POSIXt`, `POSIXct`, `POSIXlt`
#' or anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on papers laid before the given House.
#' @export
#' @examples \dontrun{
#' x <- papers_laid(withdrawn = FALSE, house = 'commons')
#'
#' x <- papers_laid(withdrawn = TRUE, house = NULL)
#' }

papers_laid <- function(withdrawn = FALSE, house = NULL,
                        start_date = "1900-01-01", end_date = Sys.Date(),
                        extra_args = NULL, tidy = TRUE,
                        tidy_style = "snake_case", verbose = TRUE) {
  house <- tolower(house)

  house_query <- house_query_util(house) ## in utils-house.R

  withdrawn_query <- ifelse(
    withdrawn == TRUE,
    "&withdrawn=true",
    "&withdrawn=false"
  )

  dates <- paste0(
    "&max-ddpModified=", as.Date(end_date),
    "&min-ddpModified=", as.Date(start_date)
  )

  baseurl <- paste0(url_util, "paperslaid.json?")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  papers <- jsonlite::fromJSON(paste0(
    baseurl, withdrawn_query, house_query,
    dates, extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(papers$result$totalResults / 100)

  query <- paste0(
    baseurl, withdrawn_query, house_query, dates,
    extra_args, "&_pageSize=100&_page="
  )

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) { ### move this to external utils file?

      df$dateLaid._value <- as.POSIXct(df$dateLaid._value)

      df$dateLaid._datatype <- "POSIXct"

      if (withdrawn == TRUE) {
        df$dateWithdrawn._value <- gsub(
          "T", " ",
          df$dateWithdrawn._value
        )

        df$dateWithdrawn._value <- as.POSIXct(
          lubridate::parse_date_time(
            df$dateWithdrawn._value,
            "Y-m-d H:M:S"
          )
        )

        df$dateWithdrawn._datatype <- "POSIXct"
      }

      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname papers_laid
#' @export
hansard_papers_laid <- papers_laid
