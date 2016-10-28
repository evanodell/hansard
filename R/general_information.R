#### GENERAL INFORMATION

### 4 TV PROGRAMMES

tv_programmes <- function(all=TRUE) {

  baseurl_av <- "http://lda.data.parliament.uk/tvprogrammes.json"

  av <- fromJSON("http://lda.data.parliament.uk/tvprogrammes.json")

  avJpage <- round(av$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:avJpage){
    mydata <- fromJSON(paste0(baseurl_av, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 17 PAPERS LAID

papers_laid <- function(all=TRUE) {

  baseurl_papers <- "http://lda.data.parliament.uk/paperslaid.json"

  papers <- fromJSON("http://lda.data.parliament.uk/paperslaid.json")

  papersJpage <- round(papers$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:papersJpage){
    mydata <- fromJSON(paste0(baseurl_papers, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 18 ALL ANSWERED QUESTIONS

all_answered_questions <- function(all=TRUE) {

  baseurl_allAnswered <- "http://lda.data.parliament.uk/answeredquestions.json"

  allAnswered <- fromJSON("http://lda.data.parliament.uk/answeredquestions.json")

  allAnsweredJpage <- round(allAnswered$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:allAnsweredJpage){
    mydata <- fromJSON(paste0(baseurl_allAnswered, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}
