


# publication logs tidying ------------------------------------------------

pub_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$publicationDate._value <- as.POSIXct(df$publicationDate._value)

    df$publicationDate._datatype <- "POSIXct"

  }

  df <- hansard_tidy(df, tidy_style)

  df

}


