library(tidyverse)
library(eyetools)
library(patchwork)

load("sample_data.RData")

d <- 
  all_samples |> 
  select(pID = participant_name,
         time = recording_timestamp_ms,
         x = gaze_point_x_dacs_px,
         y = gaze_point_y_dacs_px,
         eye_movement_type,
         eye_movement_type_index,
         duration = gaze_event_duration_ms, 
         fix_x = fixation_point_x_dacs_px,
         fix_y = fixation_point_y_dacs_px)

d <- d[2:nrow(d),] # first row seems to be NA. 

d$time <- round(d$time/1000,0) # round to ms

# take a sub-sample of the data

sample_size <- 30000 # size of the sample

start <- sample(nrow(d)-sample_size,1) # random start point
end <- start + sample_size - 1

sample <- d[start:end,] # get those samples

sample$time <- sample$time - sample$time[1] # make this sample start timestamps at 0

# get tobii defined fixations
tobii_fix <- 
  sample %>%
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

# prep data for eyetools

sample_eyetools <- 
  sample |> 
  select(pID, time, x, y) |> 
  mutate(trial = 1)

sample_eyetools_i <- interpolate(sample_eyetools)
sample_eyetools_s <- smoother(sample_eyetools_i)

eyetools_fix_disp <- fixation_dispersion(sample_eyetools_s)

# problem 1: vti algorithm seems to be unusually slow? taking minutes, compared to a few seconds for dispersion
# problem 2: if the smoothed data is used, very few fixations picked up. Is smoothing too aggressive?

eyetools_fix_vti <-  fixation_VTI(sample_eyetools_i)

# we then want to visualise (some) of these fixations, as we did before, to compare.

# we want to do the same process for saccades

