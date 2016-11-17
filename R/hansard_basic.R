

#' Hansard - Generic API Function
#'
#' Walks the user through the various steps of calling data from the parliamentary API. STILL A WORK IN PROGRESS, DO NOT USE!!!!
#' @param call Calls the
#' @keywords Hansard API
#' @export
#' @examples \dontrun{
#' x <- hansard_basic('elections.json')
#' }


hansard_basic <- function() {

  message("WORK IN PROGRESS DO NOT USE!!!!!")
  warning("WORK IN PROGRESS DO NOT USE!!!!!", immediate. = TRUE)

  ###Lists

  hansard_list <- c("AV Live Logging", "Bills", "Commons Answered Questions",
                    "Commons Divisions","Commons Oral Question Times", "Commons Oral Questions",
                    "Commons Written Questions", "Constituencies", "Early Day Motions",
                    "Elections", "Election Results", "Lords Attendances", "Lords Bill Amendments",
                    "Lords Divisions", "Lords Written Questions", "Members", "Papers Laid",
                    "Parliamentary Questions Answered", "Publication Logs", "Research Briefings",
                    "Sessions", "Thesaurus", "e-Petitions")

  av_list <- c("TV", "Clips")

  bills_list <- c("Ammended", "Stage Types", "Publications")

  commons_answered_questions_list <- c("All", "Date",
                                       "Department", "Answered By")

  commons_divisions_list <- c("All", "Date", "Aye", "No",
                              "Vote Summary", "Vote Full",
                              "Session", "UIN Summary",
                              "UIN Full")

  commons_oral_question_times_list <- c("All", "ID", "Session")

  commons_oral_questions_list <- c("All", "Times", "Days Tabled",
                                   "Days Answered", "Asked By",
                                   "Session", "Department")

  commons_written_questions_list <- c("All", "Department", "Dates",
                                      "Asked By")

  constituencies_list <- c("All")

  early_day_motions_list <- c("All", "All Sponsors", "Primary Sponsor",
                              "Signatures", "Motion ID")

  elections_list <- c("All", "Election ID")

  election_results_list <- c("All", "Election ID")

  lords_attendances_list <- c("All", "Date")

  lords_bill_amendments_list <- c("All")

  lords_divisions_list <- c("All", "Date", "Not Content",
                            "Content", "Session")

  lords_written_questions_list <- c("All", "Department", "Dates")

  members_list <- c("All", "Commons", "Lords", "Lords Interests")

  papers_laid_list <- c("All")

  parliamentary_questions_answered_list <- c("All", "Asked By")

  publication_logs_list <- c("All")

  research_briefings_list <- c("All", "Topics", "Types", "By Topic",
                               "Sub-Topic", "Topic Sub-Topic")

  sessions_list <- c("All", "Days")

  thesaurus_list <- c("All")

  epetitions_list <- c("All", "By Constituency", "Petition ID", "Response",
                        "Constituency ID")


    ###SELECTION PROCESS

    select1 <- select.list(hansard_list, title="Please select a data set")

    if(select1==hansard_list[1]) {

    select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[2]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[3]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[4]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[5]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }



    }
    else if (select1==hansard_list[6]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[7]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[8]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[9]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[10]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[11]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[12]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[13]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[14]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[15]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[16]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[17]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[18]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[19]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[20]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[21]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[22]) {

      select_av <- select.list(av_list)

      if(select_av==av_list[1]){

        tv_programmes("TV")

      } else if (select_av==av_list[2]) {

        tv_programmes("clips")

      }

    }
    else if (select1==hansard_list[23]) {

      select_epetitions <- select.list(epetitions_list)

      if(select_epetitions==epetitions_list[1]){

        epetition("all")

      } else if (select_epetitions==epetitions_list[2]) {

        epetition("byConstituency")

      }else if (select_epetitions==epetitions_list[3]) {

        epetition("petitionID")

      }else if (select_epetitions==epetitions_list[4]) {

        epetition("idConstituency")

      }

    }
    else {
      message("Please select an option")
    }
}
