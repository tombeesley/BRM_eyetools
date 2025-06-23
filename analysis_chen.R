# process Chen tobii data

library(tidyverse)
library(eyetools)
library(patchwork)

d_raw <- read_csv("Chen_data_tobii.csv")

d_raw <- janitor::clean_names(d_raw)


d <- 
  d_raw %>% 
  select(pID = participant_name,
         time = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         eye_movement_type,
         duration = gaze_event_duration,
         eye_movement_type_index,
         fixation_point_x,
         fixation_point_y)

# extract tobii fixations
tobii_fix <- 
  d %>%
  filter(eye_movement_type == "Fixation") %>% 
  group_by(eye_movement_type_index) %>% 
  slice(1) %>% 
  mutate(start = time,
         end = start + duration,
         trial = 1,
         fix_n = 1:n(),
         prop_NA = 0, 
         min_dur = 150,
         disp_tol = 100) %>% 
  ungroup() %>% 
  select(pID, trial, fix_n, start, end, duration, x, y, prop_NA, min_dur, disp_tol)

# prepare eyetools fixations
eyetools_raw <- 
  d %>% 
  select(pID, time, x, y) %>% 
  mutate(trial = 1)
eyetools_raw <- interpolate(eyetools_raw)
eyetools_fix <- fixation_dispersion(eyetools_raw)

# specify time period
start_time <- sample(max(eyetools_fix$end),1)
end_time <- start_time + 10000

sel_tobii <- filter(tobii_fix, start >= start_time & end <= end_time)
sel_eyetools <- filter(eyetools_fix, start >= start_time & end <= end_time)

a <- 
  plot_spatial(fix_data = sel_tobii) +
  ggtitle("Tobii extracted fixations")
  

b <- plot_spatial(fix_data = sel_eyetools) +
  ggtitle("eyetools extracted fixations")

a+b
