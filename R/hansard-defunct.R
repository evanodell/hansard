

#' Defunct parameters for hansard package
#'
#'

lords_ammendments <- function() {
    .Defunct("lords_amendments")
    lords_amendments()
}



lords_vote_record <- function(lord.id, lordsRecord = c("all", "content", "notContent")) {
    .Deprecated("lord_vote_record")
    lord_vote_record(peer_id = lord.id, lobby = tolower(lordsRecord))
}

