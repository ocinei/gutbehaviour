#' A quick temporary function used for generating trend plot; to be updated
#'
#' @param data the data matrix, ABX_Looming for example
#' @param avg_velocity the average velocity vector, ABX_Looming_baseline for example
#' @export
#'
#' @return a time-series line plot with smoothed curves
trendplot <- function(data, avg_velocity) {

  avg.velocty <- my_iterator(as.numeric(avg_velocity))
  # important: this is the normalisation step
  vdata <- apply(data[,-1], 2, function(x) normalisation(x, avg_velocity = avg.velocty)) %>% as.data.frame() # data normalised by dividing over the subject-specific average velocity and taking log
  # end of normalisation step
  vdata <- cbind(data$time, vdata)

  g_data <- gather(vdata, key = "subjects", value = "log_velocity", 2:ncol(data))
  # density_p <- ggplot(g_data, aes(x = log_velocity, y = subjects)) + geom_density_ridges() + theme_classic()

  # time series plot
  colnames(g_data)[1] <- "time"
  g_data$time <- as.POSIXct(g_data$time, "Asia/Taipei", format = "%H:%M:%OS")
  p1 <-ggplot(g_data, aes(x = time, y = log_velocity)) + geom_line() + facet_wrap(vars(subjects), ncol = 6) + stat_smooth() + theme_light() # generate facet plots

  return(p1)
}
#' Helper function for normalisation of the data
normalisation <- function(x, avg_velocity) {
  x <- log(x/call_iterator(avg_velocity))
}
