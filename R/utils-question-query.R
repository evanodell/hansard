
question_query_construction <- function(mp_id, answering_department) {
  if (is.null(mp_id) || is.na(mp_id)) {
    mp_id_query <- ""
  } else {
    mp_id_query <- utils::URLencode(
      paste0(
        "&tablingMember=http://data.parliament.uk/members/", mp_id
      )
    )
  }

  if (is.null(answering_department) || is.na(answering_department)) {
    json_query <- ".json?"
  } else {
    json_query <- utils::URLencode(
      paste0("/answeringdepartment.json?q=", answering_department)
    )
  }

  paste0(json_query, mp_id_query)
}
