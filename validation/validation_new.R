library(tidyverse)
library(eyetools)
library(patchwork)

load("../BRM_eyetools/validation/sample_data.RData")

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

set.seed(2710)

start <- sample(nrow(d)-sample_size,1) # random start point
end <- start + sample_size - 1

sample <- d[start:end,] # get those samples

sample$time <- sample$time - sample$time[1] # make this sample start timestamps at 0

sample <- na.omit(sample)

# get tobii definesample# get tobii defined fixations
tobii_fix <- 
  sample %>%
  filter(time < 2000) |> 
  filter(eye_movement_type == "Fixation") %>% 
  group_by(eye_movement_type_index) %>% 
  slice(1) %>% 
  mutate(start = time,
         end = start + duration,
         trial = 1,
         #fix_n = row_number(),
         prop_NA = 0, 
         min_dur = 150,
         disp_tol = 100) %>% 
  ungroup() %>% 
  rowid_to_column("fix_n") |> 
  select(pID, trial, fix_n, start, end, duration, x, y, prop_NA, min_dur, disp_tol)

# prep data for eyetools

sample_eyetools <- 
  sample |> 
  select(pID, time, x, y) |> 
  mutate(trial = 1)

sample_eyetools_i <- interpolate(sample_eyetools)
sample_eyetools_s <- smoother(sample_eyetools_i)


bind_rows(
  sample_eyetools_i |> mutate(source = "raw"),
  sample_eyetools_s |> mutate(source = "smooth")) |> 
ggplot(aes(time, x, colour = source)) + geom_path()


#eyetools_fix_disp_raw <- fixation_dispersion(filter(sample_eyetools_i, time <2000), min_dur = 150, disp_tol = 100)
#eyetools_fix_disp_smooth <- fixation_dispersion(filter(sample_eyetools_s, time <2000), min_dur = 150, disp_tol = 100)


# we then want to visualise (some) of these fixations, as we did before, to compare.

plot_data <- bind_rows(
  tobii_fix |> mutate(source = "Tobii"),
  eyetools_fix_disp_raw |> mutate(source = "Eyetools_raw"),
  eyetools_fix_disp_smooth |> mutate(source = "Eyetools_smooth")
)

radius <- 50

plot_data |> 
  filter(duration >= 150) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(shape = source), size = 2) +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius, colour = source), inherit.aes = FALSE) +
  coord_fixed() +
  theme_minimal()

plot_data |> #filter(fix_n < 10) |> 
  ggplot(aes(y = source)) +
  geom_segment(
    aes(x = start, xend = end, yend = source),
    linewidth = 3,
    alpha = 0.7
  ) +
  facet_grid(pID ~ trial) +
  labs(
    x = "Time (ms)",
    y = NULL
  ) +
  theme_minimal()
