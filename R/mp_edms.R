

#' mp_edms
#'
#' Imports data on early day motions
#' @param mp_id The ID number of an MP. Required parameter.
#' @param primary_sponsor Returns a data frame of all early day motions where the given member is the primary sponsor. Defaults to TRUE.
#' @param sponsor Returns a data frame of early day motions where the given member is the primary sponsor or a sponsor. Defaults to FALSE.
#' @param signatory Returns a data frame of all early day motions signed by the given member. Because of the structure of the API, there is less information contained in the data frame return if signatory is TRUE. Defaults to FALSE.
#' @param full_data Requests all available data on the EDMs signed or sponsored by a member. Defaults to FALSE. Note that this can take
#' @keywords Early Day Motion
#' @export
#' @examples \dontrun{
#' x <- mp_edms('signed')
#'
#' x <- mp_edms('sponsor')
#'
#' }


mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data=FALSE) {

    if (is.null(mp_id) == TRUE) {
        stop("mp_id must not be empty", call. = FALSE)
    }

    query_primary_sponsor <- paste0("&isPrimarySponsor=",tolower(primary_sponsor))

    query_sponsor <- paste0("&isSponsor=", tolower(sponsor))

    query <- paste0("&member=http://data.parliament.uk/members/", mp_id)

    baseurl <- "http://lda.data.parliament.uk/edmsignatures.json?"

    message("Connecting to API")

    edms <- jsonlite::fromJSON(paste0(baseurl, query, query_primary_sponsor, query_sponsor, "&_pageSize=500"), flatten=TRUE)

    if (edms$result$totalResults > edms$result$itemsPerPage) {

        jpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    } else {

        jpage <- 0
    }

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, query_primary_sponsor, query_sponsor, "&_pageSize=500&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

    df$dateSigned._value <- as.Date(df$dateSigned._value)

    if(full_data==FALSE) {

      names(df)[names(df)=="_about"] <- "about"

      df$about <- gsub("http://data.parliament.uk/resources/", "",df$about)

      df$about <- gsub("/signatures/.*", "",df$about)

      search_list <- as.list(df$about)

      data <- matrix()

      [1] "_about"                "creator"               "date"                  "dateTabled"            "ddpCreated"
      [6] "ddpModified"           "edmNumber"             "edmStatus"             "externalLocation"      "humanIndexable"
      [11] "identifier"            "isPrimaryTopicOf"      "legislature"           "motionText"            "numberOfSignatures"
      [16] "primarySponsor"        "primarySponsorPrinted" "published"             "publisher"             "session"
      [21] "sessionNumber"         "signature"             "sponsorPrinted"        "sponsors"              "title"
      [26] "type"


      data <- data.frame(about = NA,
                         creator_label = NA,
                         date = NA,
                         dateTabled = NA,
                         ddpCreated = NA,
                         ddpModified = NA,
                         edmNumber = NA,
                         edmStatus = NA,
                         externalLocation = NA,
                         humanIndexable = NA,
                         identifier = NA,
                         isPrimaryTopicOf = NA,
                         legislature = NA,
                         motionText = NA,
                         numberOfSignatures = NA,
                         primarySponsor = NA,
                         primarySponsorPrinted = NA,
                         published = NA,
                         publisher = NA,
                         session = NA,
                         sessionNumber = NA,
                         title = NA)

      data <- as.matrix(data)

      baseurl <- "http://lda.data.parliament.uk/resources/"

      dat <- vector("list", length(search_list)+1)

      dat <- list()

      for(i in search_list){

        search <- jsonlite::fromJSON(paste0(baseurl, search_list[[i]], ".json"),flatten=TRUE)

        search_df <- data.frame(about = search$result$primaryTopic$`_about`,
                           creator_label = search$result$primaryTopic$creator$`_about`,
                           date = search$result$primaryTopic$date$`_value`,
                           dateTabled = search$result$primaryTopic$dateTabled$`_value`,
                           ddpCreated = search$result$primaryTopic$ddpCreated$`_value`,
                           ddpModified = search$result$primaryTopic$ddpModified$`_value`,
                           edmNumber = search$result$primaryTopic$edmNumber$`_value`,
                           edmStatus = search$result$primaryTopic$edmStatus$`_value`,
                           externalLocation = search$result$primaryTopic$externalLocation,
                           humanIndexable = search$result$primaryTopic$humanIndexable$`_value`,
                           identifier = search$result$primaryTopic$identifier$`_value`,
                           isPrimaryTopicOf = search$result$primaryTopic$isPrimaryTopicOf,
                           legislature = search$result$primaryTopic$legislature$prefLabel._value,
                           motionText = search$result$primaryTopic$motionText,
                           numberOfSignatures = search$result$primaryTopic$numberOfSignatures,
                           primarySponsor = search$result$primaryTopic$primarySponsor$`_about`,
                           primarySponsorPrinted = search$result$primaryTopic$primarySponsorPrinted,
                           published = search$result$primaryTopic$published$`_value`,
                           publisher = search$result$primaryTopic$publisher$prefLabel$`_value`,
                           session = search$result$primaryTopic$session,
                           sessionNumber = search$result$primaryTopic$sessionNumber$`_value`,
                           title = search$result$primaryTopic$title)


        dat[[length(dat)+1]] <-search_df

        #dat[[i]] <- search_df

        #rm(search_df)

        #data <- rbind(data,search_df)

      }

      data <- do.call(rbind.data.frame, dat)

    start_date= min(df$dateSigned._value)

    end_date = max(x$dateSigned._value)

    df2 <- early_day_motions(start_date = start_date, end_date = end_date)


    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
