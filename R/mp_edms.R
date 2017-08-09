


#' Imports data on early day motions signed, sponsored or primarily sponsored by a given MP.
#' @param mp_id The ID number of an MP. Required parameter, defaults to NULL. Accepts single IDs in numerical or character format, or a list, data.frame column, etc. If given multiple IDs, the results are combined into a single tibble.
#' @param primary_sponsor Includes all early day motions where the given member is the primary sponsor in the tibble. Defaults to TRUE.
#' @param sponsor Includes all early day motions where the given member a sponsor (but not the primary sponsor) in the tibble. Defaults to TRUE.
#' @param signatory Includes all early day motions signed (but not sponsored or primarily sponsored) by the given member in the tibble. Defaults to TRUE.
#' @param full_data If TRUE, returns all available data on the EDMs signed or sponsored by a member. Defaults to FALSE. Note that this can be a very slow process compared to other \code{hansard} functions.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with information on the tibbles signed, sponsored and/or primarily sponsored by the given MP.
#' @keywords Early Day Motion EDM
#' @seealso \code{\link{early_day_motions}}
#' @export
#' @examples \dontrun{
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = TRUE, signatory=TRUE)
#'
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = TRUE, signatory=FALSE, full_data=TRUE)
#'
#' }


mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = TRUE, signatory = TRUE, full_data = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  if (is.null(mp_id) == TRUE) {
    stop("mp_id must not be empty", call. = FALSE)
  }

  if (length(mp_id) > 1) {

    df <- multi_mp_edms(mp_id, extra_args, primary_sponsor, sponsor, signatory)

  }

    z <- c(sponsor, primary_sponsor, signatory)

    if(length(z[z==TRUE])>1){

      df <- sig_type(mp_id, extra_args, primary_sponsor, sponsor, signatory)

    } else {

    query <- paste0("&member=http://data.parliament.uk/members/", mp_id, "&isPrimarySponsor=", tolower(primary_sponsor), "&isSponsor=", tolower(sponsor))

    baseurl <- "http://lda.data.parliament.uk/edmsignatures.json?"

    edms <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500", extra_args), flatten = TRUE)

    jpage <- floor(edms$result$totalResults/edms$result$itemsPerPage)

    pages <- list()

    if(edms$result$totalResults>0){

      message("Connecting to API")

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
      }

    }

    df <- tibble::as.tibble(dplyr::bind_rows(pages))

    }

      if (full_data == TRUE) {

        df2 <- edm_search(df)

        df <- dplyr::left_join(df, df2)

      }

      if (tidy == TRUE) {

        if (full_data == FALSE) {

        df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

        df$about <- gsub("/signatures/.*", "", df$about)

        }

        df <- hansard::hansard_tidy(df, tidy_style)

      }

        df

      }
  #}



#' @export
#' @rdname mp_edms

hansard_mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- mp_edms(mp_id = mp_id, primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, full_data = full_data, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}

