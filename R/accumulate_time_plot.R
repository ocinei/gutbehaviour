#' Function for creating accumulated time plot
#' @export
accumulated_time_plot <- function(cumu, experiment_time_in_sec = 15, subject_vec = NULL){
  if (length(subject_vec) == 0) {
    cumu_plot <- ggplot(cumu, aes(x=time, y=cumu_time/experiment_time_in_sec, color = subjects)) + geom_line() + theme_classic()
    return(cumu_plot)
  } else {
    cumu_plot <- ggplot(cumu %>% filter(subjects %in% subject_vec), aes(x=time, y=cumu_time/experiment_time_in_sec, color = subjects)) + geom_line() + theme_classic()
    return(cumu_plot)
  }
}
