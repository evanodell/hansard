

#' lords_vote_record
#'
#' Accepts an ID number for a member of the house of lords, and returns a data frame of of all their votes.
#' @param lord.id The ID number of a member of the House of Lords.
#' @param lordsRecord Accepts the arguments 'all', 'content' and 'notContent'. Defaults to 'all'.
#' @param all Returns a data frame with all recorded votes for a given member of the house of lords.
#' @param content Returns a data frame with all divisions where a given lord voted aye.
#' @param notContent Returns a data frame with all divisions where a given lord voted no.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- lords_vote_record(172, 'all')
#' }
#'


lords_vote_record <- function(lord.id, lordsRecord = c("all", "content", "notContent")) {

  match.arg(lordsRecord)

  if (lordsRecord == "all") {
    message("Retrieving content votes:")
    df_aye <- mp_vote_record(lord.id, "content")

    df_aye$vote <- "content"
    message("Retrieving not content votes:")
    df_no <- mp_vote_record(lord.id, "notContent")

    df_no$divisionNumber <- NULL

    df_no$vote <- "notContent"

    common <- intersect(colnames(df_aye),colnames(df_no))

    df <-rbind(
      subset(df_aye, select = common),
      subset(df_no, select = common)
    )

    df

  } else if (lordsRecord == "aye") {
    baseurl_aye <- "http://lda.data.parliament.uk/lordsdivisions/aye.json?mnisId="

    url_aye <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id, "&_pageSize=500"), flatten = TRUE)

    if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
      ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
    } else {
      ayeJPage <- 0
    }

    pages <- list()

    for (i in 0:ayeJPage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id, "&_pageSize=500&_page=", i), flatten = TRUE)
      message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

  } else if (lordsRecord == "no") {
    baseurl_no <- "http://lda.data.parliament.uk/lordsdivisions/no.json?mnisId="

    url_no <- jsonlite::fromJSON(paste0(baseurl_no, lord.id, "&_pageSize=500"), flatten = TRUE)

    if (url_no$result$itemsPerPage < url_no$result$totalResults) {
      noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
    } else {
      noJPage <- 0
    }

    pages <- list()

    for (i in 0:noJPage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_no, lord.id, "&_pageSize=500&_page=", i), flatten = TRUE)
      message("Retrieving page ", i + 1, " of ", noJPage + 1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

  }

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {
    df
  }

}
