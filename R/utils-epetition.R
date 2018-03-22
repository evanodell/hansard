
### tidy function for epetition_tibble

epetition_tibble_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$created._value <- stringi::stri_replace_all_fixed(
      df$created._value, "T", " ",
      vectorize_all = FALSE
    )

    df$created._value <- lubridate::parse_date_time(
      df$created._value, "Y-m-d H:M:S"
    )

    df$created._datatype <- "POSIXct"

    df$status <- as.factor(df$status)
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
