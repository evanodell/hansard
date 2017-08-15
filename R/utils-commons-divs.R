cd_tidy <- function(df, tidy_style, division_id, summary) {

  if (nrow(df) > 0) {

    if (is.null(division_id) == TRUE) {

      df$date._datatype <- "POSIXct"

      df$date._value <- as.POSIXct(df$date._value)

    } else {

      if (summary == TRUE) {

        df$date <- as.POSIXct(df$date)

      } else {

        df$`_about` <- gsub("http://data.parliament.uk/resources/", "", df$`_about`)

        names(df)[names(df) == "_about"] <- "voteId"

        df <- tidyr::unnest(df)

        df$type <- gsub("http://data.parliament.uk/schema/parl#", "", df$type)

        df$type <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", df$type)

        if (tidy_style == "camelCase") {

          df$type <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", df$type, perl = TRUE)

          substr(df$type, 1, 1) <- tolower(substr(df$type, 1, 1))

        } else if (tidy_style == "period.case") {

          df$type <- gsub("_", ".", df$type)

          df$type <- tolower(df$type)

        } else {

          df$type <- tolower(df$type)

        }
      }
    }
  }



  df <- hansard_tidy(df, tidy_style)

  df$about <- gsub("http://data.parliament.uk/members/", "", df$about)

  df

}
