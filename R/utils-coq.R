
coq_tidy <- function(df){

  df$AnswerDateTime._value <- gsub("T", " ", df$AnswerDateTime._value)

  df$AnswerDateTime._value <- lubridate::parse_date_time(df$AnswerDateTime._value, "Y-m-d H:M:S")

  df$AnswerDateTime._datatype <- "POSIXct"

  df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

  df$AnswerDate._datatype <- "POSIXct"

  # df$modified._value <- gsub('T', ' ', df$modified._value)

  # df$modified._value <- lubridate::parse_date_time(df$modified._value, 'Y-m-d H:M:S')

  # df$modified._datatype <- 'POSIXct'

  df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

  df$AnsweringBody <- unlist(df$AnsweringBody)

  df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

}
