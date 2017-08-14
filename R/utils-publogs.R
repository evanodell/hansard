

pub_tidy <- function(df){

  if(nrow(df)>0){

    df$publicationDate._value <- as.POSIXct(df$publicationDate._value)

    df$publicationDate._datatype <- "POSIXct"

  }

  df

}


