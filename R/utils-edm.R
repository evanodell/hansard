
# Retrieving EDM data -----------------------------------------------------

edm_search <- function(df){

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  df$about <- gsub("/signatures/.*", "", df$about)

  search_list <- as.list(df$about)

  #dat3 <- vector("list", length(search_list))

  dat3 <- list()

  message("Retrieving EDM details")

  baseurl <- "http://lda.data.parliament.uk/edms/"

  for (i in search_list) {

    search <- jsonlite::fromJSON(paste0(baseurl, i, ".json?"), flatten = TRUE)

    dat3[[i]] <- tibble::data_frame(about = list(search$result$primaryTopic$`_about`),
                            title = list(search$result$primaryTopic$title),
                            dateTabled._value = list(lapply(search$result$primaryTopic$dateTabled._value,as.POSIXct)),
                            dateTabled._datatype = list("POSIXct"),
                            session = list(search$result$primaryTopic$session),
                            sessionNumber = list(search$result$primaryTopic$sessionNumber),
                            edmNumber = list(search$result$primaryTopic$edmNumber$`_value`),
                            motionText = list(search$result$primaryTopic$motionText),
                            numberOfSignatures = list(search$result$primaryTopic$numberOfSignatures),
                            primarySponsor = list(search$result$primaryTopic$primarySponsorPrinted),
                            sponsor = list(search$result$primaryTopic$sponsorPrinted),
                            signingMembers = list(search$result$primaryTopic$signature$member),
                            memberSigningOrder = list(search$result$primaryTopic$signature$order),
                            memberConstituency._value = list(search$result$primaryTopic$signature$constituency._value),
                            memberDateSigned._value = list(lapply(search$result$primaryTopic$signature$dateSigned._value, as.POSIXct)),
                            memberDateSigned._datatype = list("POSIXct"),
                            signingMemberPrinted._value = list(search$result$primaryTopic$signature$memberPrinted._value),
                            memberParty._value = list(search$result$primaryTopic$signature$party._value))

  }

  df2 <- dplyr::bind_rows(dat3)

  df2$about <- gsub("http://data.parliament.uk/resources/", "", df2$about)

  df2$about <- gsub("/signatures/.*", "", df2$about)

  df2$session <- as.factor(unlist(df2$session))

  df2$dateTabled._datatype <- as.factor(unlist(df2$dateTabled._datatype))

  df2$memberDateSigned._datatype <- as.factor(unlist(df2$memberDateSigned._datatype))

  df2$title <- as.character(df2$title)

  df2

}


# Formula for multiple MPs ------------------------------------------------

multi_mp_edms <- function(mp_id=mp_id, extra_args=extra_args, primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, end_date=end_date, start_date=start_date){

  mp_id_list <- as.list(mp_id)

  dat <- vector("list", length(mp_id_list))

  for (i in 1:length(mp_id_list)) {

    dat[[i]] <- hansard::mp_edms(mp_id = mp_id_list[[i]], primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, full_data = FALSE, end_date=end_date, start_date=start_date, extra_args = extra_args, tidy = FALSE)

  }

  dat <- dat[sapply(dat, function(d) is.null(d)==FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df)=="_about"] <- "about"

  df

  }


# Formula for multiple relationships to EDMS ------------------------------------------------

sig_type <- function(mp_id=mp_id, primary_sponsor = primary_sponsor, sponsor = sponsor, signatory = signatory, end_date=end_date, start_date=start_date, extra_args =extra_args){

  sig1 <- NULL
  sig2 <- NULL
  sig3 <- NULL

  if(primary_sponsor==TRUE){

   sig1 <- hansard::mp_edms(mp_id = mp_id, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data = FALSE, end_date=end_date, start_date=start_date, extra_args = extra_args, tidy = FALSE)

    if(nrow(sig1)>0){

      sig1$isSignatory <- FALSE

    }

  }

  if(sponsor==TRUE){

    sig2 <- hansard::mp_edms(mp_id = mp_id, primary_sponsor = FALSE, sponsor = TRUE, signatory = FALSE, full_data = FALSE, end_date=end_date, start_date=start_date, extra_args = extra_args, tidy = FALSE)

    if(nrow(sig2)>0){

      sig2$isSignatory <- FALSE

    }

  }

  if(signatory==TRUE){

    sig3 <- hansard::mp_edms(mp_id = mp_id, primary_sponsor = FALSE, sponsor = FALSE, signatory = TRUE, full_data = FALSE, end_date=end_date, start_date=start_date, extra_args = extra_args, tidy = FALSE)

    if(nrow(sig3)>0){

      sig3$isSignatory <- FALSE

    }

  }

  df <- dplyr::bind_rows(sig1, sig2, sig3)

  names(df)[names(df)=="_about"] <- "about"

  df <- df[c("about", "isPrimarySponsor","isSponsor", "isSignatory", "member","order", "constituency._value", "dateSigned._value", "dateSigned._datatype", "memberPrinted._value", "party._value", "withdrawn._value")]

  df

  }
