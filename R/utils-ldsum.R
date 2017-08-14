
ldsum_tidy <- function(df){

  if(nrow(df)>0){

    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

    df$vote.type <- gsub("http://data.parliament.uk/schema/parl#", "", df$vote.type)

    df$vote.type <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", df$vote.type)

    df$vote.member <- unlist(df$vote.member)

    df$vote.member <- gsub("http://data.parliament.uk/resources/members/api/lords/id/", "", df$vote.member)

    if (tidy_style == "camelCase") {

      df$vote.type <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", df$vote.type, perl = TRUE)

      substr(df$vote.type, 1, 1) <- tolower(substr(df$vote.type, 1, 1))

    } else if (tidy_style == "period.case") {

      df$vote.type <- gsub("_", ".", df$vote.type)

      df$vote.type <- tolower(df$vote.type)

    } else {

      df$vote.type <- tolower(df$vote.type)

    }

  }

}
