#' A quick function for plotting a cumulative prob vs time plot; the definition of cumulative prob in this plot is to be given in a vignette
#'
#' @param event.prof, the event profile generated via gutbehaviour::lm_preprocess(data, average.velocity) %>% EventProfiles(freeze_time_duration = 15, flight_time_duration = 3)
#' @param time_intervals, the time_interval vector of the P_DATA object returned from lm_preprocess()
#' @export
#'
#' @return A list of two objects; it contains p, which is the plot.
cumuprob_plot <- function(event.prof, time_intervals) {

  # get the total subject number
  total_subj_number <- length(unique(event.prof$subject)) # which is the number of UNIQUE subjects in the subject column of event.prof; this should be equal to the number of subjects in the original data
  # getting the limits of time_intervals
  initial_time_limit = gsub("-(.*)","",time_intervals[1]) %>% as.POSIXct(format = "%H:%M:%OS")
  final_time_limit = gsub("(.*)-","",time_intervals[length(time_intervals)]) %>% as.POSIXct(format = "%H:%M:%OS")

  # preprocessing
  result2 <- filter(event.prof, end != "0:00:00-0:00:00") # filter all those rows that represent no occurence of either flight or freeze
  result2 <- processed_eventprofiles(result2)

  # freeze and flight profile generation
  freeze_profile <- filter(result2, event == "freeze") %>% arrange(start)
  flight_profile <- filter(result2, event == "flight") %>% arrange(start)
  shelter_profile <- filter(result2, event == "shelter") %>% arrange(start)

  # further process freeze_profile: remove rows corresponding to subjects with multiple event except for the earliest such event
  subject_records <- c() # this will contain all the subjects that have the freeze events
  to_remove_index <- c() # this will contain the indices of those subjects in freeze_profile that have more than one event
  freeze_subjects <- freeze_profile$subject
  for (i in seq_len(dim(freeze_profile)[1])) {
    subj <- freeze_subjects[i]
    if (subj %in% subject_records) {
      to_remove_index <- c(to_remove_index, i)
    } else {
      subject_records <- c(subject_records,subj)
    }
  }
  if (length(to_remove_index) != 0) {
    pfreeze_profile <- freeze_profile[-to_remove_index,]
  } else {
    pfreeze_profile <- freeze_profile
  }
  pfreeze_profile$cumuprob <- seq_len(dim(pfreeze_profile)[1])/total_subj_number # add the probability vector; at each time step the cumulative prob is increased by 1/total_subj_number
  initial <- list(initial_time_limit, initial_time_limit, "initialPointNotSujbect", "freeze", 0) # these two data points are added so that the cumulative probability plot can flatten towards the end of the period; they are not the data of any subjects
  final <- list(final_time_limit, final_time_limit, "finalPointNotSujbect", "freeze", pfreeze_profile[dim(pfreeze_profile)[1],5])
  if (length(pfreeze_profile[dim(pfreeze_profile)[1],5]) == 0) {
    pfreeze_profile <- data.frame(initial[[1]], final_time_limit, initial[[3]], initial[[4]], initial[[5]])
    names(pfreeze_profile) <- c("start", "end", "subject", "event", "cumuprob")
    nofreeze <- 1
  } else {
    nofreeze <- 0
    pfreeze_profile <- initial %>% rbind(pfreeze_profile) %>% rbind(final)
  }

  # further process flight_profile: remove rows corresponding to subjects with multiple event except for the earliest such event
  subject_records <- c() # this will contain all the subjects that have the flight events
  to_remove_index <- c() # this will contain the indices of those subjects in flight_profile that have more than one event
  flight_subjects <- flight_profile$subject
  for (i in seq_len(dim(flight_profile)[1])) {
    subj <- flight_subjects[i]
    if (subj %in% subject_records) {
      to_remove_index <- c(to_remove_index, i)
    } else {
      subject_records <- c(subject_records,subj)
    }
  }
  if (length(to_remove_index) != 0) {
    pflight_profile <- flight_profile[-to_remove_index,]
  } else {
    pflight_profile <- flight_profile
  }
  pflight_profile$cumuprob <- seq_len(dim(pflight_profile)[1])/total_subj_number # add the probability vector; at each time step the cumulative prob is increased by 1/total_subj_number
  pflight_profile$cumuprob <- seq_len(dim(pflight_profile)[1])/total_subj_number # add the probability vector; at each time step the cumulative prob is increased by 1/total_subj_number
  initial <- list(initial_time_limit, initial_time_limit, "initialPointNotSujbect", "flight", 0)
  final <- list(final_time_limit, final_time_limit, "finalPointNotSujbect", "flight", pflight_profile[dim(pflight_profile)[1],5])
  if (length(pflight_profile[dim(pflight_profile)[1],5]) == 0) {
    pflight_profile <- data.frame(initial[[1]], final_time_limit, initial[[3]], initial[[4]], initial[[5]])
    names(pflight_profile) <- c("start", "end", "subject", "event", "cumuprob")
    noflight <- 1
  } else {
    noflight <- 0
    pflight_profile <- initial %>% rbind(pflight_profile) %>% rbind(final)
  }

  # shelter logic
  # further process shelter_profile: remove rows corresponding to subjects with multiple event except for the earliest such event
  subject_records <- c() # this will contain all the subjects that have the shelter events
  to_remove_index <- c() # this will contain the indices of those subjects in shelter_profile that have more than one event
  shelter_subjects <- shelter_profile$subject
  for (i in seq_len(dim(shelter_profile)[1])) {
    subj <- shelter_subjects[i]
    if (subj %in% subject_records) {
      to_remove_index <- c(to_remove_index, i)
    } else {
      subject_records <- c(subject_records,subj)
    }
  }
  if (length(to_remove_index) != 0) {
    pshelter_profile <- shelter_profile[-to_remove_index,]
  } else {
    pshelter_profile <- shelter_profile
  }
  pshelter_profile$cumuprob <- seq_len(dim(pshelter_profile)[1])/total_subj_number # add the probability vector; at each time step the cumulative prob is increased by 1/total_subj_number
  pshelter_profile$cumuprob <- seq_len(dim(pshelter_profile)[1])/total_subj_number # add the probability vector; at each time step the cumulative prob is increased by 1/total_subj_number
  initial <- list(initial_time_limit, initial_time_limit, "initialPointNotSujbect", "shelter", 0)
  final <- list(final_time_limit, final_time_limit, "finalPointNotSujbect", "shelter", pshelter_profile[dim(pshelter_profile)[1],5])
  if (length(pshelter_profile[dim(pshelter_profile)[1],5]) == 0) {
    pshelter_profile <- data.frame(initial[[1]], final_time_limit, initial[[3]], initial[[4]], initial[[5]])
    names(pshelter_profile) <- c("start", "end", "subject", "event", "cumuprob")
    noshelter <- 1
  } else {
    noshelter <- 0
    pshelter_profile <- initial %>% rbind(pshelter_profile) %>% rbind(final)
  }

  # combining profiles, returning results
  subjwiseunique_event_profile <- rbind(pfreeze_profile, pflight_profile)
  subjwiseunique_event_profile <- rbind(subjwiseunique_event_profile, pshelter_profile)
  # generate plot
  initial_time_limit = gsub("-(.*)","",time_intervals[1])
  final_time_limit = gsub("(.*)-","",time_intervals[length(time_intervals)])
  if (nofreeze == 1) {
    p <- ggplot(subjwiseunique_event_profile, aes(x=start, y=cumuprob, group=event)) + geom_step(aes(color=event)) + scale_x_datetime(breaks = ("5 sec"), labels = scales::date_format("%S"), limits = as.POSIXct(c(initial_time_limit,final_time_limit), format = "%H:%M:%OS")) + xlab("time (in seconds)") + ylab("cumulative probability") + geom_hline(yintercept = 0, color = "#00C3C6") + ylim(0,1) + theme_classic()
  } else if (noflight == 1) {
    p <- ggplot(subjwiseunique_event_profile, aes(x=start, y=cumuprob, group=event)) + geom_step(aes(color=event)) + scale_x_datetime(breaks = ("5 sec"), labels = scales::date_format("%S"), limits = as.POSIXct(c(initial_time_limit,final_time_limit), format = "%H:%M:%OS")) + xlab("time (in seconds)") + ylab("cumulative probability") + geom_hline(yintercept = 0, color = "#FF6C67") + ylim(0,1) + theme_classic()
  } else {
    p <- ggplot(subjwiseunique_event_profile, aes(x=start, y=cumuprob, group=event)) + geom_step(aes(color=event), direction = "hv") + scale_x_datetime(breaks = ("5 sec"), labels = scales::date_format("%S"), limits = as.POSIXct(c(initial_time_limit,final_time_limit), format = "%H:%M:%OS")) + xlab("time (in seconds)") + ylab("cumulative probability") + ylim(0,1) + theme_classic() + scale_colour_manual(values = c("#E26860","#53B0B5","#F5D584"))
  }
  result <- list(subjwiseunique_event_profile, p)
  names(result) <- c("unique_event_profile", "plot")
  return(result)
}
