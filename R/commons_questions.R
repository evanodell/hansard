### Commons Questions


### 1 ORAL QUESTIONS

commons_oral_questions <- function(all=TRUE){

  baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json"

  oral <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  #if(numpages=TRUE){
  oralJpage <- round(oral$result$totalResults/10+1,digits = 0)
  #}else {
  #    oralJpage <- numpages
  #  }

  pages <- list()

  for(i in 0:oralJpage){
    mydata <- fromJSON(paste0(baseurl_oral, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}

### 2 WRITTEN QUESTIONS

commons_written_questions <- function(all=TRUE) {

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



### 6 COMMONS ANSWERED QUESTIONS

commons_answered_questions <- function(all=TRUE) {

  baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

  comAnswered <- fromJSON("http://lda.data.parliament.uk/commonsansweredquestions.json")

  comAnsweredJpage <- round(comAnswered$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:comAnsweredJpage){
    mydata <- fromJSON(paste0(baseurl_comAnswered, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 7 ORAL QUESTIONS TIMES

commons_oral_question_times <- function(all=TRUE) {

  baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json"

  oralTimes <- fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json")

  oralTimesJpage <- round(oralTimes$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:oralTimesJpage){
    mydata <- fromJSON(paste0(baseurl_oralTimes, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}
