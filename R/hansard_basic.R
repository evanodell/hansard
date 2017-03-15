## need to copy all the functions from the current, console based version of the package.

#' hansard_basic
#'
#' Walks the user through the various steps of calling data from the parliamentary API.
#' @keywords Hansard API
#' @export
#' @examples \dontrun{
#' x <- hansard_basic()
#' }

hansard_basic <- function() {
    
    hansard_list <- c("AV Live Logging", "Bills", "Commons Answered Questions", "Commons Divisions", "Commons Oral Question Times", 
        "Commons Oral Questions", "Commons Written Questions", "Constituencies", "Early Day Motions", "Elections", 
        "Election Results", "Lords Attendances", "Lords Bill Amendments", "Lords Divisions", "Lords Written Questions", 
        "Members", "Papers Laid", "Publication Logs", "Research Briefings", "Sessions", "Thesaurus", "e-Petitions")
    
    select1 <- select.list(hansard_list, title = "Please select a data set")
    
    if (select1 == hansard_list[1]) {
        
        av_list <- c("TV", "Clips")
        
        select_av <- select.list(av_list)
        
        if (select_av == av_list[1]) {
            
            tv_programmes_console("TV")
            
        } else if (select_av == av_list[2]) {
            
            tv_programmes_console("clips")
            
        }
        
    } else if (select1 == hansard_list[2]) {
        
        bills_list <- c("Ammended", "Stage Types", "Publications")
        
        select_bills <- select.list(bills_list)
        
        if (select_bills == bills_list[1]) {
            
            bills_console("ammended")
            
        } else if (select_bills == bills_list[2]) {
            
            bills_console("stageTypes")
            
        } else if (select_bills == bills_list[3]) {
            
            bills_console("publications")
            
        }
        
    } else if (select1 == hansard_list[3]) {
        
        commons_answered_questions_list <- c("All", "Date", "Department", "Answered By")
        
        select_commons_answered_questions <- select.list(commons_answered_questions_list)
        
        if (select_commons_answered_questions == commons_answered_questions_list[1]) {
            
            commons_answered_questions_console("all")
            
        } else if (select_commons_answered_questions == commons_answered_questions_list[2]) {
            
            commons_answered_questions_console("date")
            
        } else if (select_commons_answered_questions == commons_answered_questions_list[3]) {
            
            commons_answered_questions_console("department")
            
        } else if (select_commons_answered_questions == commons_answered_questions_list[4]) {
            
            commons_answered_questions_console("answeredBy")
            
        }
        
    } else if (select1 == hansard_list[4]) {
        
        commons_divisions_list <- c("All", "Date", "Aye", "No", "Vote Summary", "Vote Full", "Session", "UIN Summary", 
            "UIN Full")
        
        select_commons_divisions <- select.list(commons_divisions_list)
        
        if (select_commons_divisions == commons_divisions_list[1]) {
            
            commons_divisions_console("all")
            
        } else if (select_commons_divisions == commons_divisions_list[2]) {
            
            commons_divisions_console("date")
            
        } else if (select_commons_divisions == commons_divisions_list[3]) {
            
            commons_divisions_console("aye")
            
        } else if (select_commons_divisions == commons_divisions_list[4]) {
            
            commons_divisions_console("no")
            
        } else if (select_commons_divisions == commons_divisions_list[5]) {
            
            commons_divisions_console("voteSummary")
            
        } else if (select_commons_divisions == commons_divisions_list[6]) {
            
            commons_divisions_console("voteFull")
            
        } else if (select_commons_divisions == commons_divisions_list[7]) {
            
            commons_divisions_console("session")
            
        } else if (select_commons_divisions == commons_divisions_list[8]) {
            
            commons_divisions_console("uinSummary")
            
        } else if (select_commons_divisions == commons_divisions_list[9]) {
            
            commons_divisions_console("uinFull")
            
        }
        
    } else if (select1 == hansard_list[5]) {
        
        commons_oral_question_times_list <- c("All", "ID", "Session")
        
        select_commons_oral_question_times <- select.list(commons_oral_question_times_list)
        
        if (select_commons_oral_question_times == commons_oral_question_times_list[1]) {
            
            commons_oral_question_times_console("all")
            
        } else if (select_commons_oral_question_times == commons_oral_question_times_list[2]) {
            
            commons_oral_question_times_console("id")
            
        } else if (select_commons_oral_question_times == commons_oral_question_times_list[3]) {
            
            commons_oral_question_times_console("session")
            
        }
        
    } else if (select1 == hansard_list[6]) {
        
        commons_oral_questions_list <- c("All", "Times", "Days Tabled", "Days Answered", "Asked By", "Session", 
            "Department")
        
        select_commons_oral_questions <- select.list(commons_oral_questions_list)
        
        if (select_commons_oral_questions == commons_oral_questions_list[1]) {
            
            commons_oral_questions_console("all")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[2]) {
            
            commons_oral_questions_console("times")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[3]) {
            
            commons_oral_questions_console("daysTabled")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[4]) {
            
            commons_oral_questions_console("daysAnswered")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[5]) {
            
            commons_oral_questions_console("askedBy")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[6]) {
            
            commons_oral_questions_console("session")
            
        } else if (select_commons_oral_questions == commons_oral_questions_list[7]) {
            
            commons_oral_questions_console("department")
            
        }
        
    } else if (select1 == hansard_list[7]) {
        
        commons_written_questions_list <- c("All", "Department", "Dates", "Asked By")
        
        select_commons_written_questions <- select.list(commons_written_questions_list)
        
        if (select_commons_written_questions == commons_written_questions_list[1]) {
            
            commons_written_questions_console("all")
            
        } else if (select_commons_written_questions == commons_written_questions_list[2]) {
            
            commons_written_questions_console("department")
            
        } else if (select_commons_written_questions == commons_written_questions_list[3]) {
            
            commons_written_questions_console("dates")
            
        } else if (select_commons_written_questions == commons_written_questions_list[4]) {
            
            commons_written_questions_console("askedBy")
            
        }
        
    } else if (select1 == hansard_list[8]) {
        
        # constituencies_list <- c('All')
        
        constituencies("all")
        
    } else if (select1 == hansard_list[9]) {
        
        early_day_motions_list <- c("All", "All Sponsors", "Primary Sponsor", "Signatures", "Motion ID")
        
        select_early_day_motions <- select.list(early_day_motions_list)
        
        if (select_early_day_motions == early_day_motions_list[1]) {
            
            early_day_motions_console("all")
            
        } else if (select_early_day_motions == early_day_motions_list[2]) {
            
            early_day_motions_console("allSponsors")
            
        } else if (select_early_day_motions == early_day_motions_list[3]) {
            
            early_day_motions_console("primarySponsor")
            
        } else if (select_early_day_motions == early_day_motions_list[4]) {
            
            early_day_motions_console("signatures")
            
        } else if (select_early_day_motions == early_day_motions_list[5]) {
            
            early_day_motions_console("ID")
            
        }
        
    } else if (select1 == hansard_list[10]) {
        
        elections_list <- c("All", "Election ID")
        
        select_elections <- select.list(elections_list)
        
        if (select_elections == elections_list[1]) {
            
            elections_console("all")
            
        } else if (select_elections == elections_list[2]) {
            
            elections_console("ID")
            
        }
        
    } else if (select1 == hansard_list[11]) {
        
        election_results_list <- c("All", "Election ID")
        
        select_election_results <- select.list(election_results_list)
        
        if (select_election_results == election_results_list[1]) {
            
            election_results_console("all")
            
        } else if (select_election_results == election_results_list[2]) {
            
            election_results_console("ID")
            
        }
        
    } else if (select1 == hansard_list[12]) {
        
        lords_attendances_list <- c("All", "Date")
        
        select_lords_attendances <- select.list(lords_attendances_list)
        
        if (select_lords_attendances == lords_attendances_list[1]) {
            
            lords_attendance_console("all")
            
        } else if (select_lords_attendances == lords_attendances_list[2]) {
            
            lords_attendance_console("date")
            
        }
        
    } else if (select1 == hansard_list[13]) {
        
        lords_amendments_console("all")
        
    } else if (select1 == hansard_list[14]) {
        
        lords_divisions_list <- c("All", "Date", "Not Content", "Content", "Session")
        
        select_lords_divisions <- select.list(lords_divisions_list)
        
        if (select_lords_divisions == lords_divisions_list[1]) {
            
            lords_divisions_console("all")
            
        } else if (select_lords_divisions == lords_divisions_list[2]) {
            
            lords_divisions_console("date")
            
        } else if (select_lords_divisions == lords_divisions_list[3]) {
            
            lords_divisions_console("notContent")
            
        } else if (select_lords_divisions == lords_divisions_list[4]) {
            
            lords_divisions_console("content")
            
        } else if (select_lords_divisions == lords_divisions_list[5]) {
            
            lords_divisions_console("session")
            
        }
        
    } else if (select1 == hansard_list[15]) {
        
        lords_written_questions_list <- c("All", "Department", "Dates")
        
        select_lords_written_questions <- select.list(lords_written_questions_list)
        
        if (select_lords_written_questions == lords_written_questions_list[1]) {
            
            lords_written_questions_console("all")
            
        } else if (select_lords_written_questions == lords_written_questions_list[2]) {
            
            lords_written_questions_console("department")
            
        } else if (select_lords_written_questions == lords_written_questions_list[3]) {
            
            lords_written_questions_console("dates")
            
        }
        
    } else if (select1 == hansard_list[16]) {
        
        members_list <- c("All", "Commons", "Lords", "Lords Interests")
        
        select_members <- select.list(members_list)
        
        if (select_members == members_list[1]) {
            
            members_console("all")
            
        } else if (select_members == members_list[2]) {
            
            members_console("commons")
            
        } else if (select_members == members_list[3]) {
            
            members_console("lords")
            
        } else if (select_members == members_list[4]) {
            
            members_console("lordsInterest")
            
        }
        
    } else if (select1 == hansard_list[17]) {
        
        # papers_laid_list <- c('All')
        
        papers_laid_console("all")
        
    } else if (select1 == hansard_list[18]) {
        
        # publication_logs_list <- c('All')
        
        publication_logs_console()
        
    } else if (select1 == hansard_list[19]) {
        
        research_briefings_list <- c("All", "Topics", "Types", "By Topic", "Sub-Topic", "Topic Sub-Topic")  #19
        
        select_research_briefings <- select.list(research_briefings_list)
        
        if (select_research_briefings == research_briefings_list[1]) {
            
            research_briefings_console("all")
            
        } else if (select_research_briefings == research_briefings_list[2]) {
            
            research_briefings_console("topics")
            
        } else if (select_research_briefings == research_briefings_list[3]) {
            
            research_briefings_console("types")
            
        } else if (select_research_briefings == research_briefings_list[4]) {
            
            research_briefings_console("byTopic")
            
        } else if (select_research_briefings == research_briefings_list[5]) {
            
            research_briefings_console("subTopic")
            
        } else if (select_research_briefings == research_briefings_list[6]) {
            
            research_briefings_console("topicSubTopic")
            
        }
        
    } else if (select1 == hansard_list[20]) {
        
        sessions_list <- c("All", "Days")
        
        select_sessions <- select.list(sessions_list)
        
        if (select_sessions == sessions_list[1]) {
            
            sessions_info_console("all")
            
        } else if (select_sessions == sessions_list[2]) {
            
            sessions_info_console("days")
            
        }
        
    } else if (select1 == hansard_list[21]) {
        
        # thesaurus_list <- c('All')
        
        commons_terms_console("all")
        
    } else if (select1 == hansard_list[22]) {
        
        epetitions_list <- c("All", "By Constituency", "Petition ID", "Response", "Constituency ID")
        
        select_epetitions <- select.list(epetitions_list)
        
        if (select_epetitions == epetitions_list[1]) {
            
            epetition_console("all")
            
        } else if (select_epetitions == epetitions_list[2]) {
            
            epetition_console("byConstituency")
            
        } else if (select_epetitions == epetitions_list[3]) {
            
            epetition_console("petitionID")
            
        } else if (select_epetitions == epetitions_list[4]) {
            
            epetition_console("idConstituency")
            
        }
        
    } else {
        message("Please select an option")
    }
}
