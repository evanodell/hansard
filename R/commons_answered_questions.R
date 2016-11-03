### 6 COMMONS ANSWERED QUESTIONS

#' House of Commons Answered Questions
#'
#' This imports data on House of Commons answered questions
#' @param type The type of data you want, allows the arguments "all", "date", "department", "answeredBy", "recent"
#' @param all Imports all available answered questions
#' @param date Imports all available answered questions on a particular date
#' @param department Imports all available answered questions by answering department
#' @param answeredBy Imports all available answered questions by answering MP
#' @param recent Imports all available answered questions - NOT WORKING!
#' @keywords bills
#' @export
#' @examples
#' commons_answered_questions("all")
#' # Returns a data frame with all answered questions in the House of Commons
#'
#' commons_answered_questions("date")
#' # Returns:
#' Enter date. Format: yyyy-mm-dd: #eg 2016-10-10
#'
#' # Returns a data frame with all answered questions in the House of Commons on the given date
#'
#' commons_answered_questions("department")
#' # Returns:
#' Enter department:
#' #Enter department as string. Eg "Department of Health"
#' # Returns a data frame with all answered questions in the House of Commons from the given department
#'
#' commons_answered_questions("answeredBy")
#' # Returns
#' Enter MP ID:
#' # Enter the ID number of the MP Eg 8 (Theresa May)
#' # Returns a data frame with all answered questions in the House of Commons by the given MP
#'
#' commons_answered_questions("recent")
#' # Returns a data frame with all answered questions from the House of Commons

commons_answered_questions <- function(type =c("all", "date", "department",
                                               "answeredBy", "recent")) {
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
  } else if(type=="answeredBy") { ##WORKING!

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
  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}
