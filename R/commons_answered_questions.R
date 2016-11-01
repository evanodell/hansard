### 6 COMMONS ANSWERED QUESTIONS

#' House of Commons Answered Questions
#'
#' This imports data on House of Commons answered questions
#' @param all Imports all available answered questions Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_answered_questions()


commons_answered_questions <- function(type =c("all", "date", "department",
                                               "answered by", "recent")) {
  match.arg(type)

  if(type=="all"){  ##WORKING!
      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

      comAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500")

      comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", comAnsweredJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

  }else if (type=="date"){  ##WORKING!

      qDate <- readline("Enter date. Format: yyyy-mm-dd:  ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?date="

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500"))

      comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate,"&_pageSize=500", "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", comAnsweredJpage+1)
        pages[[i + 1]] <- mydata$result$items
      }

#  } else if (type=="uin"){  ##UNKNOWN
#     qUin <- readline("Enter the question's UIN:  ")
#     baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"
#      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?uin=", qUin))
###MAYBE ONE DAY I'LL GET AROUND TO ADDING THE ABILITY TO SEARCH BY UIN

  } else if(type=="department"){ ##UNKNOWN

      qDepartment <- readline("Enter department: ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=0", "&AnsweringBody.=", qDepartment))

      comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=", i, "&AnsweringBody.=", qDepartment), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", comAnsweredJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }
  } else if(type=="answered by") { ##WORKING!

      qAnsweredBy <- readline("Enter MP ID: ")

      baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

      comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy,".json"))

      comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:comAnsweredJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy,".json", "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", comAnsweredJpage+1)
        pages[[i + 1]] <- mydata$result$items
      }
  } else if (type=="recent"){  ##Not Working!

    qRecent <- readline("Select the number of answers ")

    baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?date="

    comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500"))

    comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:comAnsweredJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate,"&_pageSize=500", "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", comAnsweredJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }
  }
  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}
