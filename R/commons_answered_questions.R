### 6 COMMONS ANSWERED QUESTIONS

#' House of Commons Answered Questions
#'
#' This imports data on House of Commons answered questions
#' @param all Imports all available answered questions Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_answered_questions()


commons_answered_questions <- function(type) {

  if(!type %in% c("all", "date",
                  "uin", "department",
                  "answered by"))
    ("Warning: Please select an API query")

  if(type=="all"){  ##UNKNOWN
      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

      comAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsansweredquestions.json")

      comAnsweredJpage <- round(comAnswered$result$totalResults/10 + 1, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", comAnsweredJpage)
        pages[[i + 1]] <- mydata$result$items
    }

  }else if (type=="date"){  ##WORKING!

      qDate <- readline("Enter date. Format: yyyy-mm-dd:  ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?date=", qDate))

      comAnsweredJpage <- round(comAnswered$result$totalResults/10, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?date=", qDate, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", comAnsweredJpage)
        pages[[i + 1]] <- mydata$result$items
      }

  }else if (type=="uin"){  ##UNKNOWN
    #   commonsansweredquestions?uin={uin}
      qUin <- readline <- ("Enter the question's UIN:  ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?uin=", uin))

      comAnsweredJpage <- round(comAnswered$result$totalResults/10 + 1, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", comAnsweredJpage)
        pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="department"){ ##UNKNOWN

      qDepartment <- readline("Enter department: ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=0", "&AnsweringBody.=", qDepartment))

      comAnsweredJpage <- round(comAnswered$result$totalResults/10 + 1, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=", i, "&AnsweringBody.=", department), flatten = TRUE)
        message("Retrieving page ", i, " of ", comAnsweredJpage)
        pages[[i + 1]] <- mydata$result$items
    }
  } else if(type=="answered by") { ##WORKING!

      qAnsweredBy <- readline("Enter MP ID: ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy,".json"))

      comAnsweredJpage <- round(comAnswered$result$totalResults/10, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy,".json", "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", comAnsweredJpage)
        pages[[i + 1]] <- mydata$result$items
      }
  }
  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}
