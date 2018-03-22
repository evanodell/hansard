
# Tidying research briefing retrievals
research_tidy <- function(df, tidy_style) {
  df$date._value <- gsub("T", " ", df$date._value)

  df$date._value <- lubridate::parse_date_time(
    df$date._value,
    "Y-m-d H:M:Sz!*"
  )

  df$date._datatype <- "POSIXct"

  df$description <- as.character(df$description)

  names(df)[names(df) == "_about"] <- "about"

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  df$description[df$description == "NULL"] <- NA

  seq_list <- seq(from = 1, to = nrow(df), by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    if (is.null(df$section[[seq_list[[i]]]]) == FALSE) {
      df$section[[seq_list[[i]]]] <- hansard_tidy(
        df$section[[seq_list[[i]]]],
        tidy_style
      )
    }
  }

  df <- hansard_tidy(df, tidy_style)
}
