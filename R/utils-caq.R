
# all_answered_questions_multiple -----------------------------------------



caq_multi <- function(answering_department, answered_by, start_date, end_date, extra_args, verbose) {

  mp_id_list <- as.list(answered_by)

  dep_list <- as.list(answering_department)

  search_grid <- expand.grid(dep_list, mp_id_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid)=="Var1"] <-"department"
  names(search_grid)[names(search_grid)=="Var2"] <-"member"

  dat <- vector("list", nrow(search_grid))

  for (i in 1:nrow(search_grid)) {

    dat[[i]] <- hansard::commons_answered_questions(answering_department=search_grid$department[[i]], answered_by  = search_grid$member[[i]], end_date = end_date, start_date = start_date, extra_args = extra_args, verbose=verbose, tidy = FALSE)

  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}


caq_tidy <- function(df){

  if(nrow(df)>0){

  df$dateOfAnswer._value <- as.POSIXct(df$dateOfAnswer._value)

  df$dateOfAnswer._datatype <- "POSIXct"

  df$answeringMember._about <- gsub("http://data.parliament.uk/members/", "", df$answeringMember._about)

  df$AnsweringBody <- unlist(df$AnsweringBody)

  }

  df

}

