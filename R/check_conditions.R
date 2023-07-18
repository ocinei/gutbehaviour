#' Helper functions for checking whether a scalar fulfills the required condition for freezing and flight
#' The freeze condition supported should always satisfies the syntax
#' "< m * ave_velocity"
#'
#' @param vec the velocity row vector contained in the output tibble of lm_preprocess
#' @param ave_velocity an iterator object created from the average_velocity vector
.check_freeze <- function(vec, ave_velocity) {
  vec <- vec < (0.2 * call_iterator(ave_velocity)) # specify the required condition here
  return(vec)
}
#' The flight condition supported should always satisfies the syntax
#' "> m * ave_velocity"
.check_flight <- function(vec, ave_velocity) {
  vec <- vec > (3 * call_iterator(ave_velocity)) # specify the required condition here
  return(vec)
}
#' Helper functions for checking whether a scalar fulfills the required condition for freezing and flight, for EventProfiles2 specifically
#' The freeze condition supported should always satisfies the syntax
#' "< m * ave_velocity"
#'
#' @param vec the velocity row vector contained in the output tibble of lm_preprocess
#' @param ave_velocity an iterator object created from the average_velocity vector
.check_freeze2 <- function(vec, ave_velocity, factor_freeze) {
  vec <- vec < (factor_freeze * call_iterator(ave_velocity)) # specify the required condition here
  return(vec)
}
#' The flight condition supported should always satisfies the syntax
#' "> m * ave_velocity"
.check_flight2 <- function(vec, ave_velocity, factor_flight) {
  vec <- vec > (factor_flight * call_iterator(ave_velocity)) # specify the required condition here
  return(vec)
}
#' A function for checking shelter condition
.check_shelter <- function(vec) {
vec <- is.na(vec)
return(vec)
}
