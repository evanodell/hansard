
#' House of Commons Oral Question Times
#'
#' Imports data on House of Commons oral question times
#' @param all Returns a data frame with all of the oral question times. Defaults to TRUE.
#' @keywords Oral Questions
#' @export
#' @examples \dontrun{
#' x <- commons_oral_question_times(all = TRUE)
#' }

commons_oral_question_times <- function(all = TRUE) {
    # READY
    
    baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500"
    
    oralTimes <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500")
    
    oralTimesJpage <- round(oralTimes$result$totalResults/oralTimes$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:oralTimesJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", oralTimesJpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
