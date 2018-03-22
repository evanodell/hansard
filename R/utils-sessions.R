
## tidying sessions queries
sessions_tidy <- function(df, days, tidy_style) {
  df$`_about` <- gsub("http://data.parliament.uk/resources/", "", df$`_about`)

  if (days == FALSE) {
    df$endDate._value <- as.POSIXct(df$endDate._value)

    df$startDate._value <- as.POSIXct(df$startDate._value)

    df$endDate._datatype <- "POSIXct"

    df$startDate._datatype <- "POSIXct"
  } else {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)
}
