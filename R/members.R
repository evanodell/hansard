

#' Members of both houses
#'
#' Imports basic details on current and former Members of Parliament including
#' the Lords and the Commons. For more details on a given member see
#' \link[mnis]{mnis_full_biog} from the \link[mnis]{mnis} package.
#'
#' @param ID The ID of a member of the House of Commons or the House of Lords
#' to return data on. If \code{NULL}, returns a tibble of all members of both
#' houses. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with data on members of the House of Commons
#' (\code{commons_members()}), the House of Lords, (\code{lords_members()}),
#' or both (\code{members()}).
#'
#' @export
#' @section Member details functions:
#' \describe{
#' \item{\code{members}}{Basic details on a given member from either house}
#' \item{\code{commons_members}}{MPs in the House of Commons}
#' \item{\code{lords_members}}{Peers in the House of Lords}
#' }
#' @seealso \code{\link{members_search}}
#' @examples \dontrun{
#' a <- members()
#'
#' x <- members(172)
#'
#' y <- commons_members()
#'
#' z <- lords_members()
#' }

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE,
                    tidy_style = "snake_case", verbose = TRUE) {
  id_query <- dplyr::if_else(
    is.null(ID) == TRUE,
    ".json?",
    paste0("/", ID, ".json?")
  )

  baseurl <- paste0(url_util, "members")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  q_members <- jsonlite::fromJSON(paste0(baseurl, id_query, extra_args),
    flatten = TRUE
  )

  if (is.null(ID) == TRUE) {
    jpage <- floor(q_members$result$totalResults / 500)

    query <- paste0(baseurl, id_query, extra_args, "&_pageSize=500&_page=")

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  } else {
    df <- tibble::as.tibble(as.data.frame(q_members$result$primaryTopic))

    names(df)[names(df) == "X_about"] <- "about"
    names(df)[names(df) == "X_value"] <- "additionalName"
    names(df)[names(df) == "X_value.1"] <- "familyName"
    names(df)[names(df) == "X_value.2"] <- "fullName"
    names(df)[names(df) == "X_value.3"] <- "gender"
    names(df)[names(df) == "X_value.4"] <- "givenName"
    names(df)[names(df) == "X_value.5"] <- "label"
    names(df)[names(df) == "X_value.6"] <- "party"
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- hansard_tidy(df, tidy_style)

      df$about <- gsub(
        "http://data.parliament.uk/members/", "",
        df$about
      )
    }

    df <- tibble::as.tibble(df)

    df
  }
}

#' @export
#' @rdname members
hansard_members <- members


#' @export
#' @rdname members
commons_members <- function(extra_args = NULL, tidy = TRUE,
                            tidy_style = "snake_case", verbose = TRUE) {
  baseurl <- paste0(url_util, "commonsmembers.json?_pageSize=500")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  c_members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

  jpage <- floor(c_members$result$totalResults / 500)

  query <- paste0(baseurl, extra_args, "&_page=")

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @export
#' @rdname members
hansard_commons_members <- commons_members


#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE,
                          tidy_style = "snake_case", verbose = TRUE) {
  baseurl <- paste0(url_util, "lordsmembers.json?_pageSize=500")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  l_members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

  jpage <- floor(l_members$result$totalResults / 500)

  query <- paste0(baseurl, extra_args, "&_page=")

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}

#' @export
#' @rdname members
hansard_lords_members <- lords_members
