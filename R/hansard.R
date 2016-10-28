
setwd("~/R/hansard/hansard")
library(httr)
library(jsonlite)
library(xml2)



###ORAL QUESTIONS

oral_questions <- function(all=TRUE){

baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?"

oral <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

oralJpage <- round(oral$result$totalResults/10+1,digits = 0)

pages <- list()

for(i in 0:oralJpage){
  mydata <- fromJSON(paste0(baseurl_oral, "_page=", i), flatten=TRUE)
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata$result$items
  }

oralQs2 <- rbind.pages(pages)
}

toMatch <- "disability|disabled people|disabled"

disOralQs <- oralQs[grep(toMatch, oralQs$questionText),]


###WRITTEN QUESTIONS

written_questions <- function(all=TRUE) {

baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json?"

writ <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

writJpage <- round(writ$result$totalResults/10+1,digits = 0)

pages <- list()

for(i in 0:writJpage){
  mydata <- fromJSON(paste0(baseurl_writ, "_page=", i), flatten=TRUE)
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata$result$items
}
##Need to figure out how to identify the number of pages available
writQs <- rbind.pages(pages)
}


toMatch <- "disability|disabled people|disabled"

disWritQs <- oralQs[grep(toMatch, oralQs$questionText),]

