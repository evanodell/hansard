
### 8 CONSTITUENCIES

#' House of Commons Constituencies
#'
#' This imports data on House of Commons constituencies
#' @param type The type of data you want, allows the arguments "all"
#' @param all Imports all available constituencies information Defaults to TRUE.
#' @keywords Constituencies
#' @export
#' @examples
#' x <- constituencies("all")
#'# Returns a data frame of all constituencies
#'
#'


constituencies <- function(type =c("all")) {

  match.arg(type)

  if(type=="all") { #Working

    baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"

    conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json?_pageSize=500")

    contsJpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:contsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", contsJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

    df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

  }# else if(type=="ID") {#Working Weirdly

#    cont.ID <- readline("Enter the constituency ID: ")

#    cont.ID <- as.numeric(cont.ID)

#    baseurl_conts <- "http://lda.data.parliament.uk/constituencies/"

#    conts <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/constituencies/",cont.ID,".json?"))

#    df<-conts$result

#  }

}
