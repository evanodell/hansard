
##Tidy up Lords Written Questions

lwq_tidy <- function(df){

  df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

  df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

  df$dateTabled._datatype <- "POSIXct"

  df$AnswerDate._value <- "POSIXct"

  df$AnsweringBody <- unlist(df$AnsweringBody)

  df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

  df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

  df

}
