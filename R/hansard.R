
library(httr)
library(jsonlite)
library(xml2)



### 1 ORAL QUESTIONS

oral_questions <- function(all=TRUE){

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

written_questions <- function(all=TRUE) {

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


### 3 DIVISIONS

commons_divisions <- function(all=TRUE) {

  baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json"

  divis <- fromJSON("http://lda.data.parliament.uk/commonsdivisions.json")

  divisJpage <- round(divis$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:divisJpage){
    mydata <- fromJSON(paste0(baseurl_divis, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


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


### 5 BILLS

commons_bills <- function(all=TRUE) {

  baseurl_bills <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  bills <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  billsJpage <- round(bills$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:billsJpage){
    mydata <- fromJSON(paste0(baseurl_bills, "?_page=", i), flatten=TRUE)
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

oral_question_times <- function(all=TRUE) {

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


### 8 CONSTITUENCIES

constituencies <- function(all=TRUE) {

  baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json"

  conts <- fromJSON("http://lda.data.parliament.uk/constituencies.json")

  contsJpage <- round(conts$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:contsJpage){
    mydata <- fromJSON(paste0(baseurl_conts, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 9 EARLY DAY MOTIONS

early_day_motions <- function(all=TRUE) {

  baseurl_edms <- "http://lda.data.parliament.uk/edms.json"

  edms <- fromJSON("http://lda.data.parliament.uk/edms.json")

  edmsJpage <- round(edms$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:edmsJpage){
    mydata <- fromJSON(paste0(baseurl_edms, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 10 ELECTION RESULTS

election_results <- function(all=TRUE) {

  baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json"

  electR <- fromJSON("http://lda.data.parliament.uk/electionresults.json")

  electRJpage <- round(electR$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:electRJpage){
    mydata <- fromJSON(paste0(baseurl_electR, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 11 ELECTIONS

elections <- function(all=TRUE) {

  baseurl_elect <- "http://lda.data.parliament.uk/elections.json"

  elect <- fromJSON("http://lda.data.parliament.uk/elections.json")

  electJpage <- round(elect$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:electJpage){
    mydata <- fromJSON(paste0(baseurl_elect, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 12 LORDS ATTENDANCE

lords_attendance <- function(all=TRUE) {

  baseurl_lordsAttend <- " http://lda.data.parliament.uk/lordsattendances.json"

  lordsAttend <- fromJSON(" http://lda.data.parliament.uk/lordsattendances.json")

  lordsAttendJpage <- round(lordsAttend$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsAttendJpage){
    mydata <- fromJSON(paste0(baseurl_lordsAttend, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 13 LORDS AMMENDMENTS

lords_ammendments <- function(all=TRUE) {

  baseurl_lordsAmmend <- "http://lda.data.parliament.uk/lordsbillamendments.json"

  lordsAmmend <- fromJSON("http://lda.data.parliament.uk/lordsbillamendments.json")

  lordsAmmendJpage <- round(lordsAmmend$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsAmmendJpage){
    mydata <- fromJSON(paste0(baseurl_lordsAmmend, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 14 LORDS DIVISIONS

lords_divisions <- function(all=TRUE) {

  baseurl_lordsDivs <- "http://lda.data.parliament.uk/lordsdivisions.json"

  lordsDivs <- fromJSON("http://lda.data.parliament.uk/lordsdivisions.json")

  lordsDivsJpage <- round(lordsDivs$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsDivsJpage){
    mydata <- fromJSON(paste0(baseurl_lordsDivs, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



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



### 16a ALL MEMBERS

all_members <- function(all=TRUE) {

  baseurl_allMems <- "http://lda.data.parliament.uk/members.json"

  allMems <- fromJSON("http://lda.data.parliament.uk/members.json")

  allMemsJpage <- round(allMems$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:allMemsJpage){
    mydata <- fromJSON(paste0(baseurl_allMems, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 16b COMMONS MEMBERS

commons_members <- function(all=TRUE) {

  baseurl_comMems <- "http://lda.data.parliament.uk/commonsmembers.json"

  comMems <- fromJSON("http://lda.data.parliament.uk/commonsmembers.json")

  comMemsJpage <- round(comMems$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:comMemsJpage){
    mydata <- fromJSON(paste0(baseurl_comMems, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 16c LORDS MEMBERS

lords_members <- function(all=TRUE) {

  baseurl_lordMems <- "http://lda.data.parliament.uk/lordsmembers.json"

  lordMems <- fromJSON("http://lda.data.parliament.uk/lordsmembers.json")

  lordMemsJpage <- round(lordMems$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordMemsJpage){
    mydata <- fromJSON(paste0(baseurl_lordsMems, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 16d COMMONS REGISTERED INTERESTS

commons_interest <- function(all=TRUE) {

  baseurl_comInterest <- "http://lda.data.parliament.uk/commonsregisteredinterests.json"

  comInterest <- fromJSON("http://lda.data.parliament.uk/commonsregisteredinterests.json")

  comInterestJpage <- round(comInterest$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:comInterestJpage){
    mydata <- fromJSON(paste0(baseurl_comInterest, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 16e LORDS REGISTERED INTERESTS

lords_interest <- function(all=TRUE) {

  baseurl_lordInterest <- "http://lda.data.parliament.uk/lordsregisteredinterests.json"

  lordInterest <- fromJSON("http://lda.data.parliament.uk/lordsregisteredinterests.json")

  lordInterestJpage <- round(lordInterest$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordInterestJpage){
    mydata <- fromJSON(paste0(baseurl_lordInterest, "?_page=", i), flatten=TRUE)
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



### 21 SESSIONS

sessions <- function(all=TRUE) {

  baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json"

  sessions <- fromJSON("http://lda.data.parliament.uk/sessions.json")

  sessionsJpage <- round(sessions$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:sessionsJpage){
    mydata <- fromJSON(paste0(baseurl_sessions, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 22 TERMS

commons_terms <- function(all=TRUE) {

  baseurl_terms <- "http://lda.data.parliament.uk/terms.json"

  cTerms <- fromJSON("http://lda.data.parliament.uk/terms.json")

  cTermsJpage <- round(cTerms$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:cTermsJpage){
    mydata <- fromJSON(paste0(baseurl_terms, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


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


