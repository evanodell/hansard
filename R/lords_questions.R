### LORDS QUESTIONS


### 15 LORDS WRITTEN QUESTIONS

lords_written_questions <- function(all=TRUE) {

  baseurl_lordsWrit <- "http://lda.data.parliament.uk/lordswrittenquestions.json"

  lordsWrit <- fromJSON("http://lda.data.parliament.uk/lordswrittenquestions.json")

  lordsWritJpage <- round(lordsWrit$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsWritJpage){
    mydata <- fromJSON(paste0(baseurl_lordsWrit, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}
