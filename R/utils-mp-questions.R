
## MP question tidying

mp_question_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

    df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

    df$AnswerDate._datatype <- "POSIXct"

    df$dateTabled._datatype <- "POSIXct"

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

  }

  df <- hansard_tidy(df, tidy_style)

  df

}
