
house_query_util <- function(house){
  if (house == "commons") {
    house_query <- "&legislature.prefLabel=House%20of%20Commons"
  } else if (house == "lords" ) {
    house_query <- "&legislature.prefLabel=House%20of%20Lords"
  } else {
    house_query <- ""
  }
}
