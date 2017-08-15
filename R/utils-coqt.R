
coqt_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$date._value <- gsub("T", " ", df$date._value)

    df$date._value <- lubridate::parse_date_time(df$date._value, "Y-m-d H:M:S")

    df$modified._value <- gsub("T", " ", df$modified._value)

    df$modified._value <- lubridate::parse_date_time(df$modified._value, "Y-m-d H:M:S")

    df$modified._datatype <- "POSIXct"

    df$date._datatype <- "POSIXct"

  }

  df <- hansard_tidy(df, tidy_style)

  df

}
