# process Chen tobii data
#install_github('tombeesley/eyetools@0.9.3')

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

# prepare eyetools fixations_disp
eyetools_raw <- 
  d %>% 
  select(pID, time, x, y) %>% 
  mutate(trial = 1)
eyetools_raw <- interpolate(eyetools_raw)
#eyetools_raw <- smoother(eyetools_raw)
eyetools_fix <- fixation_dispersion(eyetools_raw)

# prepare eyetools fixations_vti

#eyetools_raw <- smoother(eyetools_raw)
eyetools_fix_vti <- fixation_VTI(eyetools_raw)

# extract tobii saccades
tobii_sac <- 
  d %>%
  filter(eye_movement_type == "Saccade") %>% 
  group_by(eye_movement_type_index) %>% 
  slice(c(1,n())) %>%
  mutate(saccade_event = c("origin", "terminal"),
         time = time[1],
         trial = 1) %>% 
  pivot_wider(values_from = c(x,y), 
              names_from = saccade_event, 
              names_glue = "{saccade_event}_{.value}") %>% 
  mutate(end = time + duration, mean_velocity = NA, peak_velocity = NA) %>% 
  select(pID, trial, sac_n = eye_movement_type_index, 
         start = time, end, duration,
         origin_x:terminal_y, mean_velocity, peak_velocity)
  
  
eyetools_sac <- saccade_VTI(eyetools_raw, threshold = 20)

# Sample periods from fixations
# specify time period
sel_tobii_fix <- list()
sel_eyetools_fix <- list()
sel_eyetools_fix_vti <- list()

for (p in 1:3) {

start_time <- sample(max(eyetools_fix$end),1)
end_time <- start_time + 10000

sel_tobii_fix[[p]] <- filter(tobii_fix, start >= start_time & end <= end_time)
sel_eyetools_fix[[p]] <- filter(eyetools_fix, start >= start_time & end <= end_time)
sel_eyetools_fix_vti[[p]] <- filter(eyetools_fix_vti, start >= start_time & end <= end_time)

}

# create all the plots and piece together 

t_1 <- plot_spatial(fix_data = sel_tobii_fix[[1]]) #+ ggtitle("Tobii extracted fixations")
e_1 <- plot_spatial(fix_data = sel_eyetools_fix[[1]]) #+ ggtitle("eyetools extracted fixations (dispersion method)")
v_1 <- plot_spatial(fix_data = sel_eyetools_fix_vti[[1]]) #+ ggtitle("eyetools extracted fixations (VTI method)")
t_2 <- plot_spatial(fix_data = sel_tobii_fix[[2]]) #+ ggtitle("Tobii extracted fixations")
e_2 <- plot_spatial(fix_data = sel_eyetools_fix[[2]]) #+ ggtitle("eyetools extracted fixations (dispersion method)")
v_2 <- plot_spatial(fix_data = sel_eyetools_fix_vti[[2]]) #+ ggtitle("eyetools extracted fixations (VTI method)")
t_3 <- plot_spatial(fix_data = sel_tobii_fix[[3]]) #+ ggtitle("Tobii extracted fixations")
e_3 <- plot_spatial(fix_data = sel_eyetools_fix[[3]]) #+ ggtitle("eyetools extracted fixations (dispersion method)")
v_3 <- plot_spatial(fix_data = sel_eyetools_fix_vti[[3]]) #+ ggtitle("eyetools extracted fixations (VTI method)")

row_label_0 <- wrap_elements(panel = grid::textGrob(''))
row_label_1 <- wrap_elements(panel = grid::textGrob('1'))
row_label_2 <- wrap_elements(panel = grid::textGrob('2'))
row_label_3 <- wrap_elements(panel = grid::textGrob('3'))

col_label_0 <- wrap_elements(panel = grid::textGrob(''))
col_label_1 <- wrap_elements(panel = grid::textGrob('Tobii extracted fixations'))
col_label_2 <- wrap_elements(panel = grid::textGrob('eyetools::fixation_dispersion()'))
col_label_3 <- wrap_elements(panel = grid::textGrob('eyetools::fixation_VTI()'))

(col_label_0|col_label_1|col_label_2|col_label_3)/(row_label_1|t_1|e_1|v_1)/(row_label_2|t_2|e_2|v_2)/(row_label_3|t_3|e_3|v_3) +
  plot_layout(widths = c(.1,1,1,1))  # adjust width

# Sample periods from saccades
# specify time period
sel_tobii_sac <- list()
sel_eyetools_sac <- list()

for (p in 1:3) {
  
  start_time <- sample(max(eyetools_sac$end),1)
  end_time <- start_time + 10000
  
  sel_tobii_sac[[p]] <- filter(tobii_sac, start >= start_time & end <= end_time)
  sel_eyetools_sac[[p]] <- filter(eyetools_sac, start >= start_time & end <= end_time)
  
}

# create all the plots and piece together 

t_1 <- plot_spatial(sac_data = sel_tobii_sac[[1]]) #+ ggtitle("Tobii extracted saccades - period 1")
e_1 <- plot_spatial(sac_data = sel_eyetools_sac[[1]]) #+ ggtitle("eyetools extracted saccades - period 1")
t_2 <- plot_spatial(sac_data = sel_tobii_sac[[2]]) #+ ggtitle("Tobii extracted saccades - period 2")
e_2 <- plot_spatial(sac_data = sel_eyetools_sac[[2]]) #+ ggtitle("eyetools extracted saccades - period 2")
t_3 <- plot_spatial(sac_data = sel_tobii_sac[[3]]) #+ ggtitle("Tobii extracted saccades - period 3")
e_3 <- plot_spatial(sac_data = sel_eyetools_sac[[3]]) #+ ggtitle("eyetools extracted saccades - period 3")

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

