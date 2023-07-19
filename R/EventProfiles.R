#' A function for creating event profiles given processed data from lm experiment
#'
#'
#' @param processed_data The list output by lm_preprocess
#' @param freeze_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a freeze event to be said to have occured
#' @param flight_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a flight event to be said to have occured
#' @export
#' @return EventProfiles, a data frame of dimension m*4; the first column is the start time of the event; second column the end time
EventProfiles <- function(processed_data, freeze_time_duration, flight_time_duration) {
  processed_data_matrix <- processed_data$processed_data
  ave_velocity <- my_iterator(processed_data$avelocity)
  ave_velocity2 <- my_iterator(processed_data$avelocity)
  Match_freeze <- apply(processed_data_matrix, 1, function(x) .check_freeze(x, ave_velocity))
  Match_flight <- apply(processed_data_matrix, 1, function(x) .check_flight(x, ave_velocity2))
  FreezeProfile <- .FreezeProfile(Match_freeze, freeze_time_duration, processed_data)
  FlightProfile <- .FlightProfile(Match_flight, flight_time_duration, processed_data)

  # rbind the list
  FreezeProfile <- as.data.frame(do.call(rbind, FreezeProfile))
  FreezeProfile$event <- "freeze"
  FlightProfile <- as.data.frame(do.call(rbind, FlightProfile))
  FlightProfile$event <- "flight"

  EventProfiles <- rbind(FreezeProfile, FlightProfile) #rbind the freeze and flight profiles
  colnames(EventProfiles) <- c("start", "end", "subject", "event")

  return(EventProfiles)
}
#' Helper function for identifying freezing condition
.FreezeProfile <- function(Match_freeze, freeze_time_duration, processed_data) {
  FreezeEventProfiles <- list()
  FreezeMatchIndices <- list()
  for(i in seq_len(dim(Match_freeze)[2])) {

    match_vec <- Match_freeze[,i] # get the i-th column
    match_indices <- which(match_vec == TRUE) # access the indices of timepoints which corresponds to TRUE
    indices_indices <- .event_timepoints(match_indices,freeze_time_duration) # get the start and end indices of the match indices
    freeze_match_indices <- c(NULL)
    if (length(indices_indices) == 0) { # if we cannot find any freezing event

      result <- c("0:00:00-0:00:00","0:00:00-0:00:00", processed_data$subject_names[i])
      FreezeEventProfiles <- c(FreezeEventProfiles, list(result))
      freeze_match_indices <- c(NULL)
      FreezeMatchIndices[[i]] <- freeze_match_indices
    } else {

      for (j in indices_indices) {

        start_index <- j[1]
        end_index <- j[2]
        match_start_index <- match_indices[start_index]
        match_end_index <- match_indices[end_index]
        start_time <- processed_data$time_intervals[match_start_index]
        end_time <- processed_data$time_intervals[match_end_index]
        result <- c(start_time, end_time, processed_data$subject_names[i])
        FreezeEventProfiles <- c(FreezeEventProfiles, list(result))
        freeze_match_indices <- c(freeze_match_indices, seq(from = match_start_index, to = match_end_index))
      }
      FreezeMatchIndices[[i]] <- freeze_match_indices
    }

  }
  result <- list(FreezeEventProfiles, FreezeMatchIndices)
  return(result)
}
#' Helper function for identifying flight condition
.FlightProfile <- function(Match_flight, flight_time_duration, processed_data) {
  FlightEventProfiles <- list()
  FlightMatchIndices <- list()
  for(i in seq_len(dim(Match_flight)[2])) {

    match_vec <- Match_flight[,i] # get the i-th column
    match_indices <- which(match_vec == TRUE) # access the indices of timepoints which corresponds to TRUE
    indices_indices <- .event_timepoints(match_indices,flight_time_duration) # get the start and end indices of the match indices
    flight_match_indices <- c(NULL)
    if (length(indices_indices) == 0) { # if we cannot find any freezing event

      result <- c("0:00:00-0:00:00","0:00:00-0:00:00", processed_data$subject_names[i])
      FlightEventProfiles <- c(FlightEventProfiles, list(result))
      FlightMatchIndices[[i]] <- c()
    } else {

      for (j in indices_indices) {

        start_index <- j[1]
        end_index <- j[2]
        match_start_index <- match_indices[start_index]
        match_end_index <- match_indices[end_index]
        start_time <- processed_data$time_intervals[match_start_index]
        end_time <- processed_data$time_intervals[match_end_index]
        result <- c(start_time, end_time, processed_data$subject_names[i])
        FlightEventProfiles <- c(FlightEventProfiles, list(result))
        flight_match_indices <- c(flight_match_indices, seq(from = match_start_index, to = match_end_index))
      }
      FlightMatchIndices[[i]] <- flight_match_indices
    }

  }
  result <- list(FlightEventProfiles, FlightMatchIndices)
  return(result)
}
#' Helper function for identifying the timepoints of shelter condition
.ShelterProfile <- function(Match_shelter, processed_data) {
  ShelterEventProfiles <- list()
  ShelterMatchIndices <- list()
  for(i in seq_len(dim(Match_shelter)[2])) {

    match_vec <- Match_shelter[,i] # get the i-th column
    match_indices <- which(match_vec == TRUE) # access the indices of timepoints which corresponds to TRUE
    indices_indices <- .event_timepoints(match_indices,2) # get the start and end indices of the match indices
    shelter_match_indices <- c(NULL)
    if (length(indices_indices) == 0) { # if we cannot find any freezing event

      result <- c("0:00:00-0:00:00","0:00:00-0:00:00", processed_data$subject_names[i])
      ShelterEventProfiles <- c(ShelterEventProfiles, list(result))
      ShelterMatchIndices[[i]] <- c()

    } else {

      for (j in indices_indices) {

        start_index <- j[1]
        end_index <- j[2]
        match_start_index <- match_indices[start_index]
        match_end_index <- match_indices[end_index]
        start_time <- processed_data$time_intervals[match_start_index]
        end_time <- processed_data$time_intervals[match_end_index]
        result <- c(start_time, end_time, processed_data$subject_names[i])
        ShelterEventProfiles <- c(ShelterEventProfiles, list(result))
        shelter_match_indices <- c(shelter_match_indices, seq(from = match_start_index, to = match_end_index))
      }
      ShelterMatchIndices[[i]] <- shelter_match_indices
    }

  }
  result <- list(ShelterEventProfiles, ShelterMatchIndices)
  return(result)
}
#' Helper function for identifying the time points at which there are no events
#' @export
.EventlessProfile <- function(ident, processed_data) {
  EventlessProfiles <- list()
  time_lengths <- length(processed_data$time_intervals)
  subjects_no <- length(processed_data$subject_names)
  for(i in seq_len(subjects_no)) {

    freeze_events_indices <- .eventless_helper(ident, i, condition = "freeze")
    flight_event_indices <- .eventless_helper(ident, i, condition = "flight")
    shelter_event_indices <- .eventless_helper(ident, i, condition = "shelter")
    event_indices <- c(freeze_events_indices, flight_event_indices, shelter_event_indices)
    eventless_boolean <- seq_len(time_lengths) %in% event_indices
    eventless_indices <- seq_len(time_lengths)[!eventless_boolean]
    match_indices <- eventless_indices
    indices_indices <- .event_timepoints(eventless_indices, 2)
    if (length(indices_indices) == 0) {

      result <- c("0:00:00-0:00:00","0:00:00-0:00:00", processed_data$subject_names[i], "eventless")
      EventlessProfiles <- c(EventlessProfiles, list(result))

    } else {

      for (j in indices_indices) {

        start_index <- j[1]
        end_index <- j[2]
        match_start_index <- match_indices[start_index]
        match_end_index <- match_indices[end_index]
        start_time <- processed_data$time_intervals[match_start_index]
        end_time <- processed_data$time_intervals[match_end_index]
        result <- c(start_time, end_time, processed_data$subject_names[i], "eventless")
        EventlessProfiles <- c(EventlessProfiles, list(result))
      }
    }
  }
  EventlessProfiles <- do.call(rbind, EventlessProfiles) %>% as.data.frame()
  colnames(EventlessProfiles) <- c("start", "end", "subject", "event")
  return(EventlessProfiles)
}
#' Helper function for identifying eventless profiles
#' @export
.eventless_helper <- function(result, i, condition = "freeze"){
  if (condition == "freeze"){
    len = length(result[[2]])
    if (i > len){
      return(c(NULL))
    } else{
      return(result[[2]][[i]])
    }
  }
  if (condition == "flight"){
    len = length(result[[3]])
    if (i > len){
      return(c(NULL))
    } else{
      return(result[[3]][[i]])
    }
  }
  if (condition == "shelter"){
    len = length(result[[4]])
    if (i > len){
      return(c(NULL))
    } else{
      return(result[[4]][[i]])
    }
  }
}
#' Helper function for generating iterators
#' @export
my_iterator <- function(array){
  position <- 1
  my_iterate <- function(){
    post <- position
    position <<- position + 1
    return(array[post])
  }
  return(my_iterate)
}
#' Helper function for calling an iterator
#' @export
call_iterator <- function(my_iterator){
  return(my_iterator())
}
