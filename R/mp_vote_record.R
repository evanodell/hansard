
#' mp_vote_record
#'
#' Accepts an ID number for a member of the House of Commons, and returns a data frame of their votes. Provides similar functionality to the commons_divisions() function, but accepts member IDs as function parameters rather than requesting them from the console.
#' @param mp.id The ID number of a member of the House of Commons. To look up the ID number of a member of the House of Commons use the members_search() function.
#' @param voteRecord Accepts the arguments 'all', 'aye' and 'no'. Defaults to 'all'.
#' @param all Returns a data frame with all recorded votes for a given member of the house of commons.
#' @param aye Returns a data frame with all divisions where a given MP voted aye.
#' @param no Returns a data frame with all divisions where a given MP voted no.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- mp_vote_record(172, 'all')
#'
#' x <- mp_vote_record(172, 'aye')
#'
#' x <- mp_vote_record(172, 'no')
#' }


mp_vote_record <- function(mp.id, voteRecord = c("all", "aye", "no")) {

    match.arg(voteRecord)

    if (voteRecord == "all") {
        message("Retrieving aye votes:")
        df_aye <- mp_vote_record(mp.id, "aye")

        df_aye$vote <- "aye"
        message("Retrieving no votes:")
        df_no <- mp_vote_record(mp.id, "no")

        df_no$divisionNumber <- NULL

        df_no$vote <- "no"

        df <- rbind(df_aye, df_no)
        df$vote <- as.factor(df$vote)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)

        df

    } else if (voteRecord == "aye") {
        baseurl_aye <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

        message("Connecting to API")

        url_aye <- jsonlite::fromJSON(paste0(baseurl_aye, mp.id, "&_pageSize=500"), flatten = TRUE)

        if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
            ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        } else {
            ayeJPage <- 0
        }

        pages <- list()

        for (i in 0:ayeJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_aye, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)


    } else if (voteRecord == "no") {
        baseurl_no <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

        message("Connecting to API")

        url_no <- jsonlite::fromJSON(paste0(baseurl_no, mp.id, "&_pageSize=500"), flatten = TRUE)

        if (url_no$result$itemsPerPage < url_no$result$totalResults) {
            noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        } else {
            noJPage <- 0
        }

        pages <- list()

        for (i in 0:noJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_no, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", noJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}
