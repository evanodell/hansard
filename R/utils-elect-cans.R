


##election_candidates tidy function


elect_can_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$election._about <- gsub("http://data.parliament.uk/resources/", "", df$election._about)

    df$constituency._about <- gsub("http://data.parliament.uk/resources/", "", df$constituency._about)

  }

    df <- hansard_tidy(df, tidy_style)

    df

}
