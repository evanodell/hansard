


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
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = FALSE, signatory=FALSE)
#'
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = TRUE, signatory=TRUE, full_data=TRUE)
#'
#' }


mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = TRUE, signatory = TRUE, full_data = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  if (is.null(mp_id) == TRUE) {
    stop("mp_id must not be empty", call. = FALSE)
  }

  if (length(mp_id) > 1) {

    mp_id_list <- as.list(mp_id)

    dat <- vector("list", length(mp_id_list))

    for (i in 1:length(mp_id_list)) {

      dat[[i]] <- mp_edms(mp_id = mp_id_list[[i]], primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, full_data = full_data, extra_args = extra_args, tidy = FALSE)

    }

    dat <- dat[sapply(dat, function(d) nrow(d) > 0)]

    df <- dplyr::bind_rows(dat)

    if (nrow(df) == 0) {

      message("The request did not return any data. Please check your search parameters.")

    } else {

      if (tidy == TRUE) {

        df$dateSigned._value <- as.POSIXct(df$dateSigned._value)

        df$dateSigned._datatype <- "POSIXct"

        df$member <- gsub("http://data.parliament.uk/members/", "", df$member)

        df <- hansard::hansard_tidy(df, tidy_style)

      }

      df

      }

  } else {

    z <- c(sponsor, primary_sponsor, signatory)

    if(length(z[z==TRUE])>1){

      dat2 <- vector("list", 3)

        if(signatory==TRUE){

          dat2[[1]] <- mp_edms(mp_id = mp_id, primary_sponsor = FALSE, sponsor = FALSE, signatory = TRUE, full_data = full_data, extra_args = extra_args, tidy = FALSE)

        }

        if(primary_sponsor==TRUE){

          dat2[[2]] <- mp_edms(mp_id = mp_id, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data = full_data, extra_args = extra_args, tidy = FALSE)

        }

        if(sponsor==TRUE){

          dat2[[3]] <- mp_edms(mp_id = mp_id, primary_sponsor = FALSE, sponsor = TRUE, signatory = FALSE, full_data = full_data, extra_args = extra_args, tidy = FALSE)

        }

      dat2 <- dat2[sapply(dat2, function(d) nrow(d)>0)]

        df <- dplyr::bind_rows(dat2)

        if (nrow(df) == 0) {

          message("The request did not return any data. Please check your search parameters.")

        } else {

        if (tidy == TRUE && exists("mp_id_list")==FALSE) {

          df$dateSigned._value <- as.POSIXct(df$dateSigned._value)

          df$dateSigned._datatype <- "POSIXct"

          # df$member <- unlist(df$member)

          df$member <- gsub("http://data.parliament.uk/members/", "", df$member)

          df <- hansard::hansard_tidy(df, tidy_style)

        }

          df

        }

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

     if (nrow(df)==0 && (exists("mp_id_list")==FALSE | length(z[z==TRUE])<=1)) {

      message("The request did not return any data. Please check your search parameters.")

     } ###Do Nothing

       if(signatory==TRUE){

        df$isSignatory <- TRUE

        names(df)[names(df) == "_about"] <- "about"

        df <- df[c("about", "isPrimarySponsor","isSponsor", "isSignatory", "member","order", "constituency._value", "dateSigned._value", "dateSigned._datatype", "memberPrinted._value", "party._value", "withdrawn._value")]

      } else if(nrow(df)>0) {

        df$isSignatory <- FALSE

        names(df)[names(df) == "_about"] <- "about"

        df <- df[c("about", "isPrimarySponsor","isSponsor", "isSignatory", "member","order", "constituency._value", "dateSigned._value", "dateSigned._datatype", "memberPrinted._value", "party._value", "withdrawn._value")]

      }

      if (full_data == TRUE) {

        df2 <- edm_search(df)

        #message("Joining data")

        #df2 <- bind_rows(dat)

        df <- dplyr::left_join(df, df2, by = "about")

      }

      if (tidy == TRUE) {

        df$dateSigned._value <- as.POSIXct(df$dateSigned._value)

        df$dateSigned._datatype <- "POSIXct"

        df$member <- unlist(df$member)

        df$member <- gsub("http://data.parliament.uk/members/", "", df$member)

        if(full_data==TRUE){

          df$primarySponsor <- gsub("http://data.parliament.uk/members/", "", df$primarySponsor)

          df$creatorLabel <- gsub("http://data.parliament.uk/members/", "", df$creatorLabel)

        }

        df <- hansard::hansard_tidy(df, tidy_style)

      }

        df

      }
    }
  }
#}



#' @export
#' @rdname mp_edms

hansard_mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- mp_edms(mp_id = mp_id, primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, full_data = full_data, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}


# STILL NEED TO SORT THIS OUT
#' @noRd
edm_search <- function(df){

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  df$about <- gsub("/signatures/.*", "", df$about)

  search_list <- as.list(df$about)

  #dat3 <- vector("list", length(search_list))

  dat3 <- list()

  message("Retrieving EDM data")

  baseurl <- "http://lda.data.parliament.uk/edms/"

  for (i in search_list) {

  search <- jsonlite::fromJSON(paste0(baseurl, i, ".json?"), flatten = TRUE)


  signatories <- c(member= list(search$result$primaryTopic$signature$member),
                   order = list(search$result$primaryTopic$signature$order),
                   constituency._value = list(search$result$primaryTopic$signature$constituency._value),
                   dateSigned._value = list(as.POSIXct(search$result$primaryTopic$signature$dateSigned._value)),
                   dateSigned._datatype = list(search$result$primaryTopic$signature$dateSigned._datatype),
                   memberPrinted._value = list(search$result$primaryTopic$signature$memberPrinted._value),
                   party._value = list(search$result$primaryTopic$signature$party._value))



  primarySponsorPrinted  <- search$result$primaryTopic$primarySponsorPrinted

  sponsorPrinted <- search$result$primaryTopic$sponsorPrinted

  b <- as.data.frame(search$result$primaryTopic[c( "_about", "dateTabled", "edmNumber", "edmStatus", "isPrimaryTopicOf", "motionText", "numberOfSignatures", "session", "sessionNumber", "title", "type")])


  bind_cols(primarySponsorPrinted,sponsorPrinted,b)


  dat3[[i]] <- data.frame(primarySponsorPrinted = primarySponsorPrinted,
                          sponsorPrinted = sponsorPrinted,
                          signatories = signator)


  x <- data_frame(primarySponsorPrinted = list(primarySponsorPrinted),
                  sponsorPrinted = list(sponsorPrinted))


x %>% nest(signatories)


                  signatories = as.data.frame(signatories)


  xsearch_df <- lapply(search_df, as.character)

  #dat3[[i]] <- search_df

  }

  df2 <- dplyr::bind_rows(dat3)

  df2$about <- gsub("http://data.parliament.uk/resources/", "", df2$about)

  df2$about <- gsub("/signatures/.*", "", df2$about)

  facs <- c("legislature", "publisher", "session")

  df2[facs,] <- lapply(df2[facs,], as.factor)

  dateables <- c("dateTabled", "date")

  df2[dateables,] <- lapply(df2[dateables,], as.POSIXct)

  df2

}

