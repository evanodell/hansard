


# all_answered_questions_multiple -----------------------------------------



aaq_multi <- function(mp_id, tabling_mp_id, house, answering_body, start_date, end_date, extra_args, verbose) {

  #mp_id_list <- as.list(mp_id)

  #tabling_mp_id_list <- as.list(tabling_mp_id)

  #answering_body_list <- as.list(answering_body)


  if(is.null(mp_id)==TRUE){

    mp_id_list <- NA

  } else {

    mp_id_list <- as.list(mp_id)

  }


  if(is.null(tabling_mp_id)==TRUE){

    tabling_mp_id_list <- NA

  } else {

    tabling_mp_id_list <- as.list(tabling_mp_id)

  }

  if(is.null(answering_body)==TRUE){

    answering_body_list <- NA

  } else {

    answering_body_list <- as.list(answering_body)

  }


  search_grid <- expand.grid(mp_id_list, tabling_mp_id_list, answering_body_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid)=="Var1"] <-"answering_mp"
  names(search_grid)[names(search_grid)=="Var2"] <-"tabling_mp"
  names(search_grid)[names(search_grid)=="Var2"] <-"department"

  dat <- vector("list", nrow(search_grid))

  for (i in 1:nrow(search_grid)) {

    dat[[i]] <- hansard::all_answered_questions(mp_id = search_grid$answering_mp[[i]], tabling_mp_id=search_grid$tabling_mp[[i]], house=house, answering_body=search_grid$department[[i]], end_date = end_date, start_date = start_date, extra_args = extra_args, tidy = FALSE, verbose=verbose)

  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}



# all_answered_questions tidying -----------------------------------------

aaq_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    names(df) <- gsub("answer.answeringMember.fullName._value", "answeringMember.fullName._value", names(df))

    names(df) <- gsub("answer.answeringMember._about", "answeringMember._about", names(df))

    names(df) <- gsub("answer.answerText._value", "answerText._value", names(df))

    names(df) <- gsub("answer.dateOfAnswer._datatype", "dateOfAnswer._datatype", names(df))

    names(df) <- gsub("answer.dateOfAnswer._value", "dateOfAnswer._value", names(df))

    df$dateOfAnswer._value <- as.POSIXct(df$dateOfAnswer._value)

    df$answeringMember._about <- gsub("http://data.parliament.uk/members/", "", df$answeringMember._about)

    df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$legislature <- do.call("rbind", df$legislature)

    df$legislature.prefLabel._value <- df$legislature$prefLabel._value

    df$legislature_about <- df$legislature$`_about`

    df$legislature_about <- gsub("http://data.parliament.uk/terms/", "", df$legislature_about)

    df$legislature <- NULL

  }

  df <- hansard_tidy(df, tidy_style)

  df

}
