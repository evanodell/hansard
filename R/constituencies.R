

#' House of Commons Constituencies
#'
#' Imports data on House of Commons constituencies
#' @param contType The type of data you want, allows the arguments 'all'
#' @param all Returns a data frame of all constituencies. Defaults to TRUE.
#' @keywords Constituencies
#' @export
#' @examples \dontrun{
#' x <- constituencies('all')
#' }
#'


constituencies <- function(contType = c("all")) {
    
    match.arg(contType)
    
    if (contType == "all") 
        {
            # Working
            
            baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"
            
            conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json?_pageSize=500")
            
            contsJpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)
            
            pages <- list()
            
            for (i in 0:contsJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", contsJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }
            
            
            
        }  # else if(contType=='ID') {#Working Weirdly
    
    # cont.ID <- readline('Enter the constituency ID: ')
    
    # cont.ID <- as.numeric(cont.ID)
    
    # baseurl_conts <- 'http://lda.data.parliament.uk/constituencies/'
    
    # conts <- jsonlite::fromJSON(paste0('http://lda.data.parliament.uk/constituencies/',cont.ID,'.json?'))
    
    # df<-conts$result
    
    # }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
