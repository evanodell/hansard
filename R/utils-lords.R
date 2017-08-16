

# lords_votes -----------------------------------------

multi_mp_edms <- function(mp_id = mp_id, extra_args = extra_args, primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, end_date = end_date, start_date = start_date, verbose=verbose) {

  mp_id_list <- as.list(mp_id)

  dat <- vector("list", length(mp_id_list))

  for (i in 1:length(mp_id_list)) {

    dat[[i]] <- hansard::mp_edms(mp_id = mp_id_list[[i]], primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, full_data = FALSE, end_date = end_date, start_date = start_date, extra_args = extra_args, tidy = FALSE, verbose=verbose)

  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}







# lords_written_questions_multiple -----------------------------------------
# lords Written questions multiple function

lwq_multi <- function(answering_department, peer_id, start_date, end_date, extra_args, verbose){

  if(is.null(peer_id)==TRUE){

    mp_id_list <- NA

  } else {

    mp_id_list <- as.list(peer_id)

  }

  if(is.null(answering_department)==TRUE){

    dep_list <- NA

  } else {

    dep_list <- as.list(answering_department)

  }

  search_grid <- expand.grid(dep_list, mp_id_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid)=="Var1"] <-"department"
  names(search_grid)[names(search_grid)=="Var2"] <-"member"

  dat <- vector("list", nrow(search_grid))

  for (i in 1:nrow(search_grid)) {

    dat[[i]] <- hansard::lords_written_questions(answering_department=search_grid$department[[i]], peer_id = search_grid$member[[i]], end_date = end_date, start_date = start_date, extra_args = extra_args, verbose=verbose, tidy = FALSE)

  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}








# lords Written questions tidy function -------------------------------------------
##Tidy up Lords Written Questions

lwq_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

    df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

    df$dateTabled._datatype <- "POSIXct"

    df$AnswerDate._value <- "POSIXct"

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

  }

  df <- hansard_tidy(df, tidy_style)

  df

}



# lords divisions tidy function -------------------------------------------

lords_division_tidy <- function(df, division_id, summary, tidy_style) {

  if (nrow(df) > 0) {

    if (is.null(division_id) == TRUE) {

      df$date._datatype <- "POSIXct"

      df$date._value <- as.POSIXct(df$date._value)

    } else {

      if (summary == FALSE) {

        df <- ldsum_tidy(df, tidy_style)

      }
    }

  }

  df <- hansard_tidy(df, tidy_style)

  df

}


# lords vote record tidy function -------------------------------------------


lord_vote_record_tidy <- function(df, tidy_style) {

  if (nrow(df) > 0) {

    df$date._datatype <- "POSIXct"

    df$date._value <- as.POSIXct(df$date._value)

  }

  df <- hansard_tidy(df, tidy_style)

  df

}

# lords amendments tidy function -------------------------------------------

lords_amendments_tidy <- function(df, tidy_style) {

  if (nrow(df) > 0) {

    df$bill.date._value <- as.POSIXct(df$bill.date._value)

    df$bill.date._datatype <- "POSIXct"

  }

  df <- hansard_tidy(df, tidy_style)

  df

}

# lords attendance tidy function -------------------------------------------


lords_attendance_tidy <- function(df, tidy_style) {

  if (nrow(df) > 0) {

    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

  }

  df <- hansard_tidy(df, tidy_style)

  df

}



# lords division summary tidy function -------------------------------------------



ldsum_tidy <- function(df, tidy_style){

  if(nrow(df)>0){

    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

    df$vote.type <- gsub("http://data.parliament.uk/schema/parl#", "", df$vote.type)

    df$vote.type <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", df$vote.type)

    df$vote.member <- unlist(df$vote.member)

    df$vote.member <- gsub("http://data.parliament.uk/resources/members/api/lords/id/", "", df$vote.member)

    if (tidy_style == "camelCase") {

      df$vote.type <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", df$vote.type, perl = TRUE)

      substr(df$vote.type, 1, 1) <- tolower(substr(df$vote.type, 1, 1))

    } else if (tidy_style == "period.case") {

      df$vote.type <- gsub("_", ".", df$vote.type)

      df$vote.type <- tolower(df$vote.type)

    } else {

      df$vote.type <- tolower(df$vote.type)

    }

  }

  df

}
