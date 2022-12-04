#' A quick temporary function for generating combined trend and raster plot
#'
#' @param data the data matrix; ABX_Looming for example
#' @param avg_velocity the average velocity vector; ABX_Looming-baseline for example
#' @param event.prof event profile generated from processed_eventprofiles(), the input of which is from EventProfiles(), the input of which is from P_DATA, which is output by lm_preprocess()
#' @export
#'
#' @return a plot combining trend and raster plots
combinedplot <- function(data, avg_velocity, event.prof) {

  avg.velocty <- my_iterator(as.numeric(avg_velocity))
  # important: this is the normalisation step
  vdata <- apply(data[,-1], 2, function(x) normalisation(x, avg_velocity = avg.velocty)) %>% as.data.frame() # data normalised by dividing over the subject-specific average velocity and taking log
  # end of normalisation step
  vdata <- cbind(data$time, vdata)

  g_data <- gather(vdata, key = "subjects", value = "log_velocity", 2:ncol(data))
  # density_p <- ggplot(g_data, aes(x = log_velocity, y = subjects)) + geom_density_ridges() + theme_classic()

  # data post-processing
  colnames(g_data)[1] <- "time"
  g_data$time <- as.POSIXct(g_data$time, "Asia/Taipei", format = "%H:%M:%OS")
  colnames(event.prof)[3] <- "subjects"

  # combined plot
  p1b <- ggplot(g_data, aes(x = time, y = log_velocity)) + geom_line() +  geom_segment(data = event.prof, aes(x = start, xend = end, y = -6, yend = -6, color = event), linewidth = 2) + facet_wrap(vars(subjects), ncol = 3) + theme_classic()

  return(p1b)
}
