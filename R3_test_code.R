library(eyetools)
head(HCL,4)
data <-combine_eyes(HCL)

head(data, 4)


data <-interpolate(data, method ="approx", report=F)
data <-smoother(data, span = .02, plot= TRUE)



data <-merge(data,HCL_behavioural)
data <-conditional_transform(data, flip= "x", cond_column= "cue_order", cond_values= "2")
fixations<-fixation_dispersion(data, min_dur= 150, #Mindurationin ms
                               disp_tol= 100,#Max dispersiontoleranceinpixels
                               NA_tol= 0.25, #proportionofNAs tolerated
                               progress= FALSE) #toggle progressbar
head(fixations)
saccades<-saccade_VTI(data, threshold= 150, min_dur= 20)
head(saccades)
AOI_areas <- create_AOI_df(3)
AOI_areas[1,] <-c(460, 840,400, 300) #Leftrectangualar AOI
AOI_areas[2,] <-c(1460, 840,200, NA) #RightcircularAOI
AOI_areas[3,] <-c(960, 840,200, 400) #CentrerectangularAOI
data_AOI_time<- AOI_time(data =fixations, data_type= "fix", AOIs= HCL_AOIs,
                         AOI_names= c("target", "distractor", "outcomes"), 
                         as_prop= TRUE, 
                         trial_time = HCL_behavioural$RT)
head(data_AOI_time, 9)
library(dplyr)
data_AOI_time %>% dplyr::group_by(AOI) %>% dplyr::summarise(mean_time =
                                                              mean(time))
data_AOI_time_binned<-AOI_time_binned(data, AOIs = HCL_AOIs, AOI_names= c("target",
                                                                          "distractor", "outcomes"), bin_length= 1000, #inmilliseconds
                                      max_time= 8000) #inmilliseconds
head(data_AOI_time_binned, 10)
data_AOI_entry <- AOI_seq(fixations, AOIs =HCL_AOIs, AOI_names= c("target",
                                                                  "distractor", "outcomes"))
head(data_AOI_entry, 9)
#addacentralfixation AOIregion
HCL_AOIs[4,] <-c(960, 810, 200,200)
data_AOI_entry <-AOI_seq(fixations, AOIs= HCL_AOIs, AOI_names= c("target", "distractor",
                                                                 "outcomes", "fixation"))
data_AOI_entry %>% dplyr::group_by(pID, trial) %>% dplyr::slice(1)
plot_spatial(raw_data = data, fix_data = fixations, pID_values = 118, trial_values = 6)
plot_seq(data = data, bin_time = 1000, bin_range = c(1,4), trial_values = 1, pID_values =
           118, AOIs = HCL_AOIs)
plot_AOI_growth(data = data, AOIs = HCL_AOIs, type = "abs", pID_values = 118,
                trial_values = 1)
plot_AOI_growth(data = data, AOIs = HCL_AOIs, type = "prop", pID_values = 118,
                trial_values = 1)
plot_heatmap(data, pID_values = 118, trial_values = c(1,3), alpha_range = c(0.1,1))
