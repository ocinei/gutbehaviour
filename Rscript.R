data <- read.csv("Fast.csv")
average.velocity <- read.csv("Fast_Baseline.csv")

# Preprocess
P_DATA <- gutbehaviour::lm_preprocess(data, average_velocity = average.velocity)
P_DATA$time_intervals # time intervals

# Event profiles
event.prof <- gutbehaviour::EventProfiles(P_DATA, freeze_time_duration = 15, flight_time_duration = 3)  # modify the durations here
event.prof <- gutbehaviour::processed_eventprofiles(event.prof)

# Raster plot
time_int <- P_DATA$time_intervals
plot <- gutbehaviour::various_lm_plots(event.prof, size = 6, time_intervals = time_int) # size parameter can be changed and this represents the width of the segment in the raster plot
