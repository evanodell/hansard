
#' Early Day Motions
#'
#' This imports data on early day motions
#' @param edmType The type of data you want, allows the argument 'all'
#' @param all Imports all available EDMs.
#' @keywords EDM
#' @export
#' @examples \donttest{
#'
#' x <- early_day_motions('all')
#' #Returns a data frame of all early day motions
#'
#'
#' }


early_day_motions <- function(edmType = c("all")) {

    match.arg(edmType)

    if (edmType == "all")
        {
            # Working

            baseurl_edms <- "http://lda.data.parliament.uk/edms.json?_pageSize=500"

            edms <- jsonlite::fromJSON(baseurl_edms)

            edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

            pages <- list()

            for (i in 0:edmsJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }
        }  # else if (edmType=='ID'){#Not working

    # mp.id <- readline('Enter Member ID: ')

    # baseurl_edms <- 'http://lda.data.parliament.uk/edmsignatures.json?_pageSize=500'

    # edms <- jsonlite::fromJSON(baseurl_edms)

    # edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    # pages <- list()

    # for (i in 0:edmsJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_edms, '&_page=', i), flatten = TRUE) message('Retrieving
    # page ', i+1, ' of ', edmsJpage+1) pages[[i + 1]] <- mydata$result$items } } #else if (edmType=='signature'){

    # baseurl_edms <- 'http://lda.data.parliament.uk/edms.json?_pageSize=500'

    # edms <- jsonlite::fromJSON('http://lda.data.parliament.uk/edms.json?_pageSize=500')

    # edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    # pages <- list()

    # for (i in 0:edmsJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_edms, '&_page=', i), flatten = TRUE) message('Retrieving
    # page ', i+1, ' of ', edmsJpage+1) pages[[i + 1]] <- mydata$result$items } }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}
