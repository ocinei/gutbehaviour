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
