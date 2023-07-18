#' A function for creating event profiles given processed data from lm experiment, using specific check_conditions functions
#' allowing different freezing and flight conditions to be set
#'
#'
#' @param processed_data The list output by lm_preprocess
#' @param freeze_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a freeze event to be said to have occured
#' @param flight_time_duration A positive integer indicating the minimum number of time intervals that should be spanned for a flight event to be said to have occured
#' @export
#' @return EventProfiles, a data frame of dimension m*4; the first column is the start time of the event; second column the end time
EventProfiles2 <- function(processed_data, freeze_time_duration, flight_time_duration, factor.freeze = 0.2, factor.flight = 3) {
  processed_data_matrix <- processed_data$processed_data
  ave_velocity <- my_iterator(processed_data$avelocity)
  ave_velocity2 <- my_iterator(processed_data$avelocity)
  Match_freeze <- apply(processed_data_matrix, 1, function(x) .check_freeze2(x, ave_velocity, factor.freeze)) #update check_freeze2
  Match_flight <- apply(processed_data_matrix, 1, function(x) .check_flight2(x, ave_velocity2, factor.flight)) #update check_flight2
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
#' A function for creating event profiles given processed data from lm experiment, using specific check_conditions functions. Moreover, sheltering events are also identified.
EventProfiles_all <- function(processed_data, freeze_time_duration, flight_time_duration, factor.freeze = 0.2, factor.flight = 3) {
  processed_data_matrix <- processed_data$processed_data
  ave_velocity <- my_iterator(processed_data$avelocity)
  ave_velocity2 <- my_iterator(processed_data$avelocity)
  Match_freeze <- apply(processed_data_matrix, 1, function(x) .check_freeze2(x, ave_velocity, factor.freeze)) #update check_freeze2
  Match_flight <- apply(processed_data_matrix, 1, function(x) .check_flight2(x, ave_velocity2, factor.flight)) #update check_flight2
  Match_shelter <- apply(processed_data_matrix, 1, function(x){.check_shelter(x)})

  Freeze_result <- .FreezeProfile(Match_freeze, freeze_time_duration, processed_data)
  Flight_result <- .FlightProfile(Match_flight, flight_time_duration, processed_data)
  Shelter_result <- .ShelterProfile(Match_shelter, processed_data)

  FreezeProfile <- Freeze_result[[1]]
  Freeze_indices <- Freeze_result[[2]]
  FlightProfile <- Flight_result[[1]]
  Flight_indices <- Flight_result[[2]]
  ShelterProfile <- Shelter_result[[1]]
  Shelter_indices <- Shelter_result[[2]]

  # rbind the list
  FreezeProfile <- as.data.frame(do.call(rbind, FreezeProfile))
  FreezeProfile$event <- "freeze"
  FlightProfile <- as.data.frame(do.call(rbind, FlightProfile))
  FlightProfile$event <- "flight"
  ShelterProfile <- as.data.frame(do.call(rbind, ShelterProfile))
  ShelterProfile$event <- "shelter"

  EventProfiles <- rbind(FreezeProfile, FlightProfile) #rbind the freeze and flight profiles
  EventProfiles <- rbind(EventProfiles, ShelterProfile)
  colnames(EventProfiles) <- c("start", "end", "subject", "event")

  result <- list(EventProfiles, Freeze_indices, Flight_indices, Shelter_indices)
  return(result)
}
