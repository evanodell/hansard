
#' Early Day Motions by MP or Peer
#'
#' Imports data on early day motions signed, sponsored or primarily sponsored
#' by a given MP or Peer.
#' @param mp_id The ID number of an MP or Peer. Required parameter, Defaults
#' to \code{NULL}. Accepts single IDs in numerical or character format, or a
#' list, data.frame column, etc. If given multiple IDs, the results are
#' combined into a single tibble.
#' @param primary_sponsor Includes all early day motions where the given
#' member is the primary sponsor in the tibble. Defaults to \code{TRUE}.
#' @param sponsor Includes all early day motions where the given member a
#' sponsor (but not the primary sponsor) in the tibble.
#' Defaults to \code{TRUE}.
#' @param signatory Includes all early day motions signed (but not sponsored
#' or primarily sponsored) by the given member in the tibble.
#' Defaults to \code{TRUE}.
#' @param full_data If \code{TRUE}, returns all available data on the EDMs
#' signed or sponsored by a member. Defaults to \code{FALSE}. Note that
#' this can be a very slow process compared to other \code{hansard} functions.
#' @param start_date Only includes early day motions signed on or after this
#' date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects
#' of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes early day motions signed on or before this
#' date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects
#' of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with information on the tibbles signed, sponsored
#' and/or primarily sponsored by the given MP.
#'
#' @seealso \code{\link{early_day_motions}}
#' @export
#' @examples \dontrun{
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE,
#'              sponsor = TRUE, signatory=TRUE)
#'
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE,
#'              sponsor = TRUE, signatory=FALSE, full_data=TRUE)
#' }


mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = TRUE,
                    signatory = TRUE, full_data = FALSE,
                    start_date = "1900-01-01", end_date = Sys.Date(),
                    extra_args = NULL, tidy = TRUE,
                    tidy_style = "snake_case", verbose = FALSE) {

    dates <- paste0("&max-dateSigned=", as.Date(end_date),
                    "&min-dateSigned=", as.Date(start_date))

    if (is.null(mp_id) == TRUE) {

        stop("mp_id must not be empty", call. = FALSE)

    }

    if (length(mp_id) > 1) {

        df <- multi_mp_edms(mp_id, extra_args, primary_sponsor, sponsor,
                            signatory, end_date, start_date, verbose)

    } else {

        z <- c(sponsor, primary_sponsor, signatory)

        if (length(z[z == TRUE]) > 1) {

            df <- sig_type(mp_id, primary_sponsor, sponsor, signatory,
                           end_date, start_date, extra_args, verbose)

        } else {

            z_query <- paste0("member=http://data.parliament.uk/members/", mp_id,
                            "&isPrimarySponsor=", tolower(primary_sponsor),
                            "&isSponsor=", tolower(sponsor))

            baseurl <- "http://lda.data.parliament.uk/edmsignatures.json?"

            edms <- jsonlite::fromJSON(paste0(baseurl, z_query,
                                              dates, extra_args),
                                       flatten = TRUE)

            jpage <- floor(edms$result$totalResults/500)

            query <- paste0(baseurl, z_query, dates, extra_args,
                            "&_pageSize=500&_page=")

            df <- loop_query(query, jpage, verbose) # in utils-loop.R

            if (nrow(df) > 0) {

                names(df)[names(df) == "_about"] <- "about"

            }

        }

    }

    if (full_data == TRUE) {

        df2 <- edm_search(df, verbose)

        df <- dplyr::left_join(df, df2)

    }

    if (tidy == TRUE) {

        if (full_data == FALSE) {

            df$about <- stringi::stri_replace_all_fixed(df$about,
            "http://data.parliament.uk/resources/", "",
             vectorize_all = FALSE)

            df$about <- stringi::stri_replace_all_fixed(df$about,
             "/signatures/.*", "",
              vectorize_all = FALSE)

        }

        df <- hansard_tidy(df, tidy_style)

    }

    df

}




#' @export
#' @rdname mp_edms
hansard_mp_edms <- mp_edms
