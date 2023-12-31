#' Preprocess the data from lm experiment
#'
#' @param data A data frame that is the lm dataset of dimension m*(n+1); m is the number of time intervals and n is the number of subjects
#' @param average_velocity A data frame of dimension 1*n that contains the average velocities of the n subjects;
#'
#' @return A list containing the preprocessed data and other additional information
#' @export
#'
lm_preprocess <- function(data,average_velocity) {

  ave_velocity <- as.numeric(average_velocity)
  time <- data[,1] # time variables
  data <- data[,-1] # subjects data
  total_subj_number <- dim(data)[2]
  subject_names <- colnames(data)
  data <- data %>% as_tibble() # data is therefore the transposed form of the csv data imported
  colnames(data) <- time

  Preprocessed_data <- list(data, time, total_subj_number, subject_names, ave_velocity)
  names(Preprocessed_data) <- c("processed_data", "time_intervals", "total_subjects_number", "subject_names", "avelocity")
  return(Preprocessed_data)

}
