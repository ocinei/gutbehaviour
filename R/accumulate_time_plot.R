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
#' Function for creating heat plot for accumulated time
#' @export
accumulated_time_heat_plot <- function(cumu, master_cumulative_prof, time_to_consider = 1, experiment_time_in_sec = 15, subject_vec = NULL, show_values = FALSE){
  time_index <- ceiling(time_to_consider/0.1 + 1)
  total_time <- experiment_time_in_sec
  cumu_prof_time_t <- cumu_prof %>% filter(time == master_cumulative_prof[[2]][time_index])
  caption_text = paste("t=",time_to_consider,"s",sep="")
  heat_map <- ggplot(cumu_prof_time_t, aes(x=event_considered, y=subjects, fill=cumu_time/total_time)) + geom_tile() + scale_fill_gradient(low="white", high="blue") + theme_classic() + labs(caption = caption_text)
  if (length(subject_vec) == 0){
    if (show_values == TRUE){
      heat_map <- heat_map + geom_text(aes(label=round(cumu_time/total_time,2)))
      return(heat_map)
    }
    return(heat_map)
  } else {
    cumu_prof_time_t <- cumu_prof_time_t %>% filter(subjects %in% subject_vec)
    heat_map <- ggplot(cumu_prof_time_t, aes(x=event_considered, y=subjects, fill=cumu_time/total_time)) + geom_tile() + scale_fill_gradient(low="white", high="blue") + theme_classic() + labs(caption = caption_text)
    if (show_values == TRUE) {
      heat_map <- heat_map + geom_text(aes(label=round(cumu_time/total_time,2)))
      return(heat_map)
    }
    return(heat_map)
  }
}
#' Helper function for generating caption in heat plot video
generate_caption <- function(t){
  text = strftime(t,format = "%H:%M:%S")
  return(text)
}
#' Function for generating heat plot video gif
#' @export
generate_heat_plot_video <- function(cumu_prof, ordering, video_name = "heat_plot_video.gif"){
  heat_plot_video <- ggplot(cumu_prof, aes(x=event_considered, y=subjects, fill=cumu_time/15)) + geom_tile() + scale_fill_gradient(low="white", high="blue") + theme_classic() + scale_y_discrete(limits = ordering) + theme(axis.text.y = element_blank(), axis.ticks = element_blank(),  axis.title.y.left = element_blank()) + labs(caption = "Time = {generate_caption(frame_time)} s") +
    transition_time(time) +
    enter_fade() +
    exit_shrink() +
    ease_aes('linear')
  print(heat_plot_video)
  anim_save(video_name)
  print("The heat plot video is produced and saved.")
}
