#' Function for getting relevant profile from event profiles
#' @export
get_relevant_profile <- function(event.prof, subject_concerned, event_concerned){
  considerata <- filter(event.prof, subject == subject_concerned) %>% filter(event == event_concerned)
  return(considerata)
}
#' Function for generating master cumulative time event profiles
#' @export
generate_cumulative_profiles <- function(P.DATA, event.prof){
  time_intervals <- P.DATA$time_intervals
  final_time_limit <- gsub("(.*)-","",time_intervals[length(time_intervals)]) %>% as.POSIXct(format = "%H:%M:%OS")
  default_time_seq <- seq.POSIXt(from = as.POSIXct("00:00:00",format = "%H:%M:%OS"), to = final_time_limit, by=0.1) # in milliseconds
  time_diff_0 <- as.POSIXct("00:00:00",format = "%H:%M:%OS")-as.POSIXct("00:00:00",format = "%H:%M:%OS")
  CUMULATIVE_PROFILES <- lapply(default_time_seq, function(time_instance){mutate(event.prof, time_considered = time_instance) %>%
      mutate(less_than_start = time_considered <= start) %>%
      mutate(more_than_end = time_considered >= end) %>%
      mutate(cumu_time = less_than_start*(time_diff_0) + more_than_end*(end-start) + (1-less_than_start)*(1-more_than_end)*(time_considered-start))})
  cumulative_profile <- do.call(rbind, CUMULATIVE_PROFILES)
  result <- list(cumulative_profile, default_time_seq)
  return(result)
}
#' Function for calculating cumulative time for a subject at a specific time for a particular event
#' @export
calculate_cumulative_time <- function(time, subject_considered, event_considered, cumulative_profile){
  cumulative_profile_subt <- cumulative_profile %>% filter(subject == subject_considered) %>% filter(time_considered == time) %>% filter(event == event_considered)
  cumu_time <- sum(cumulative_profile_subt$cumu_time)
  return(cumu_time)
}
#' Function for generating cumulative time profile at a particular time
#' @export
generate_cumutime_per_timestep <- function(time_considered, event_considered, P.DATA, cumulative_profile){
  all_subjects <- P.DATA$subject_names
  cumu_time <- sapply(all_subjects, function(x){calculate_cumulative_time(time_considered, x, event_considered, cumulative_profile)})
  df <- list(time = time_considered, cumu_time = cumu_time, subjects = all_subjects)
  df <- as.data.frame(df)
  return(df)
}
#' Function for generating cumulative time profile at all considered times for a particular event
#' @export
generate_cumutime_per_event <- function(default_time_seq, event_considered, P.DATA, cumulative_profile){
  DF <- lapply(default_time_seq, function(x){generate_cumutime_per_timestep(x, event_considered, P.DATA, cumulative_profile)})
  df <- do.call(rbind, DF)
  df$event_considered <- event_considered
  return(df)
}
#' Function for generating cumulative time profile at all considered times for all events
#' @export
generate_cumutime_all_events <- function(default_time_seq, P.DATA, cumulative_profile){
  events <- c("freeze","flight","shelter","eventless")
  DFs <- lapply(events, function(p){generate_cumutime_per_event(default_time_seq, event_considered = p, P.DATA, cumulative_profile)})
  return(DFs)
}
