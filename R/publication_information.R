#### PUBLICATION INFORMATION



### 19 PUBLICATION LOGS

publication_logs <- function(all=TRUE) {

  baseurl_logs <- "http://lda.data.parliament.uk/publicationlogs.json"

  logs <- fromJSON("http://lda.data.parliament.uk/publicationlogs.json")

  logsJpage <- round(loge$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:logsJpage){
    mydata <- fromJSON(paste0(baseurl_logs, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 20 RESEARCH BRIEFINGS

research_briefings <- function(all=TRUE) {

  baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json"

  research <- fromJSON("http://lda.data.parliament.uk/researchbriefings.json")

  researchJpage <- round(research$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:researchJpage){
    mydata <- fromJSON(paste0(baseurl_research, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



