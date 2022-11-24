#' ABX looming data set
#'
#' An example dataset with the acceptable format
#'
#' @format ## `ABX_Looming`
#' A data frame with 150 rows (each representing a time interval) and 6 columns, each column from the second column onward representing a subject
#'
"ABX_Looming"

#' ABX looming data set average_velocity vectors
#'
#' The average velocity vector associated with the ABX_Looming dataset
#'
#' @format ## `ABX_Looming_baseline`
#' A data frame with 1 row and 5 columns, each column representing a subject
"ABX_Looming_baseline"

#' Processed data
#'
#' A list of processed data after running lm_preprocess() on ABX_Looming and ABX_Looming_baseline
#'
#' @format ## `processed_data`
#' A list containing the processed data. The first object in the list is the tibble (which should be the transpose of the original data)
"processed_data"

#' Event profile example
#'
#' Using ABX_Looming and ABX_Looming_baseline as the input data, an event profile is generated using EventProfiles function
#'
#' @format ## `Eventprofiles`
#' A data frame of dimension k*4, with each row representing an event of interest; the first 2 columns are the start and end times of the events respectively, while the third column is the name of the subject, the fourth column describes the event
"Eventprofiles"

#' processed event profile example
#'
#' Using processed_eventprofiles function, an event profile that is Eventprofiles without the NAs is produced
#'
#' @format ## `processed_event_data`
#' A dataframe of dimension k*4 (the same as that of Eventprofiles); there should be no NA within processed_event_data
"processed_event_data"
