### tidying bills datas

bills_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

    if ("bill" %in% names(df)) {
      df$bill <- gsub("http://data.parliament.uk/resources/", "", df$bill)
    }
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
