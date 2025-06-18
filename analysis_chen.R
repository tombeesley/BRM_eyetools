# process Chen tobii data

library(tidyverse)

d_raw <- read_csv("Chen_data_tobii.csv")

d_raw <- janitor::clean_names(d_raw)


d <- 
  d_raw %>% 
  select(recording_timestamp, 
         pID = participant_name,
         gaze_point_x,
         gaze_point_y,
         eye_movement_type,
         gaze_event_duration,
         eye_movement_type_index,
         fixation_point_x,
         fixation_point_y)
