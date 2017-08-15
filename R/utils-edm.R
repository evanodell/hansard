### early_day_motions tidying


edm_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

    df$dateTabled._datatype <- "POSIXct"

  }

  df <- hansard_tidy(df, tidy_style)

  df

}
