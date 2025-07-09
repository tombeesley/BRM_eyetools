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
sel_tobii <- list()
sel_eyetools <- list()

for (p in 1:3) {

start_time <- sample(max(eyetools_fix$end),1)
end_time <- start_time + 10000

sel_tobii[[p]] <- filter(tobii_fix, start >= start_time & end <= end_time)
sel_eyetools[[p]] <- filter(eyetools_fix, start >= start_time & end <= end_time)

}

# create all the plots and piece together 

t_1 <- plot_spatial(fix_data = sel_tobii[[1]]) + ggtitle("Tobii extracted fixations - period 1")
e_1 <- plot_spatial(fix_data = sel_eyetools[[1]]) + ggtitle("eyetools extracted fixations - period 1")
t_2 <- plot_spatial(fix_data = sel_tobii[[2]]) + ggtitle("Tobii extracted fixations - period 2")
e_2 <- plot_spatial(fix_data = sel_eyetools[[2]]) + ggtitle("eyetools extracted fixations - period 2")
t_3 <- plot_spatial(fix_data = sel_tobii[[3]]) + ggtitle("Tobii extracted fixations - period 3")
e_3 <- plot_spatial(fix_data = sel_eyetools[[3]]) + ggtitle("eyetools extracted fixations - period 3")

(t_1/t_2/t_3)+(e_1/e_2/e_3)

(t_1+e_1)/(t_2+e_2)/(t_3+e_3)


# compute distance between a and b

c_t <- sel_tobii[,c("x", "y")]
c_e <- sel_eyetools[,c("x", "y")]

min_dist <- NULL

for (i in 1:nrow(c_t)) {
  dist_vals <-  as.matrix(dist(rbind(c_t[i,], c_e)))
  min_dist[i] <- min(dist_vals[2:nrow(dist_vals),1])
}

mean(min_dist)

