#' A function for creating event profiles given processed data from lm experiment
#'
#'
#' @param processed_data The list output by lm_preprocess
#' @param freeze_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a freeze event to be said to have occured
#' @param flight_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a flight event to be said to have occured
#'
#' @return EventProfiles, a data frame of dimension m*4; the first column is the start time of the event; second column the end time
#' @export
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

  for(i in seq_len(dim(Match_freeze)[2])) {

    match_vec <- Match_freeze[,i] # get the i-th column
    match_indices <- which(match_vec == TRUE) # access the indices of timepoints which corresponds to TRUE
    indices_indices <- .event_timepoints(match_indices,freeze_time_duration) # get the start and end indices of the match indices
    if (length(indices_indices) == 0) { # if we cannot find any freezing event

      result <- c(NA,NA, processed_data$subject_names[i])
      FreezeEventProfiles <- c(FreezeEventProfiles, list(result))

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
      }

    }

  }
  return(FreezeEventProfiles)
}
#' Helper function for identifying flight condition
.FlightProfile <- function(Match_flight, flight_time_duration, processed_data) {
  FlightEventProfiles <- list()

  for(i in seq_len(dim(Match_flight)[2])) {

    match_vec <- Match_flight[,i] # get the i-th column
    match_indices <- which(match_vec == TRUE) # access the indices of timepoints which corresponds to TRUE
    indices_indices <- .event_timepoints(match_indices,flight_time_duration) # get the start and end indices of the match indices
    if (length(indices_indices) == 0) { # if we cannot find any freezing event

      result <- c(NA,NA, processed_data$subject_names[i])
      FlightEventProfiles <- c(FlightEventProfiles, list(result))

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
      }

    }

  }
  return(FlightEventProfiles)
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
