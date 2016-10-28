
library(httr)
library(jsonlite)
library(xml2)


### 23 PETITIONS - STILL NEED TO SORT THIS OUT

petitions <- function(all=TRUE) {

  baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  writ <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  writJpage <- round(writ$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:writJpage){
    mydata <- fromJSON(paste0(baseurl_writ, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


