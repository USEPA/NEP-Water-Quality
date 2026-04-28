# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# DATE OF LAST UPDATE: Apr 28, 2026 
# Updated by: Andrew Mandovi 

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    INSTRUCTIONS FOR USER: 
#                    ----------------------
#  1. Define parameters and thresholds unique to BARNEGAT BAY before running 
#  2. Runs qaqc script for BARNEGAT BAY
#  3. Save the results (optional)
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

cat('Processing NEP: Barnegat Bay \n')

##### Step 1. PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: ####

# For Gross-Range Test:
ph_user_min = 6
ph_user_max = 9
temp_user_min = -1
temp_user_max = 35
sal_user_min = 0
sal_user_max = 36
co2_user_min = 100
co2_user_max = 2500
do_user_min = 0
do_user_max = 15
# sensor min/max's
ph_sensor_min = 0
ph_sensor_max = 14
temp_sensor_min = -10
temp_sensor_max = 45
sal_sensor_min = -1
sal_sensor_max = 50
co2_sensor_min = 0
co2_sensor_max = 3500
do_sensor_min = 0
do_sensor_max = 25
# for Spike Test:
spike_low_ph = 1
spike_high_ph = 2
spike_low_temp = 3
spike_high_temp = 5
spike_low_sal = 10
spike_high_sal = 20
spike_low_do = 5
spike_high_do = 10
spike_low_co2 = 200
spike_high_co2 = 400
# Seasonal thresholds for climatology test:
seasonal_thresholds = list(
  ph_min = list(DJF = 6, MAM = 6, JJA = 6, SON = 6),
  ph_max = list(DJF = 9, MAM = 9, JJA = 9, SON = 9),
  temp.c_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  temp.c_max = list(DJF = 36, MAM = 36, JJA = 36, SON = 36),
  sal.ppt_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  sal.ppt_max = list(DJF = 36, MAM = 36, JJA = 36, SON = 36),
  do.mgl_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  do.mgl_max = list(DJF = 15, MAM = 15, JJA = 15, SON = 15),
  co2.ppm_min = list(DJF = 100, MAM = 100, JJA = 100, SON = 100),
  co2.ppm_max = list(DJF = 2000, MAM = 2000, JJA = 2000, SON = 2000)
)
# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
min_num_pts_rate_of_change = 3
sample_interval = 15 # minutes
# For Flatline Test:
num_flatline_sus = 48 # 12 hours
num_flatline_fail = 96 # 24 hours
flatline_thresholds = c(
  'ph' = 0.0099,
  'temp.c' = 0.01,
  'sal.ppt' = 0.001,
  'do.mgl' = 0.005
)
# For Attenuated Signal Test:
# these values dictate the exceedence thresholds to which the standard deviation over the previous 12-hour period would FAIL or be SUSPECT if they do not exceed them 
# similar to a flat-line test, it tests for near-flat-line scenarios, where a signal is overly dampened by an external factor
attenuated_signal_thresholds = list(
  ph = list(sus = 0.005, fail = 0.001),  # suspect if variability < 15%, fail if < 5%
  temp.c = list(sus=0.05, fail = 0.02),
  sal.ppt = list(sus = 0.15, fail = 0.05),
  do.mgl = list(sus = 0.05, fail = 0.02)
)
time_window_attsig = 12  # Time (in hours) to look back across to compare the signal against (default = 24-hours)

# Threshold lists 
user_thresholds = list(
  ph = list(min=ph_user_min, max=ph_user_max),
  temp.c = list(min=temp_user_min, max=temp_user_max),
  sal.ppt = list(min=sal_user_min, max=sal_user_max),
  do.mgl = list(min=do_user_min, max=do_user_max),
  co2.ppm = list(min=co2_user_min, max=co2_user_max)
)
sensor_thresholds = list(
  ph = list(min=ph_sensor_min, max=ph_sensor_max),
  temp.c = list(min=temp_sensor_min, max=temp_sensor_max),
  sal.ppt = list(min=sal_sensor_min, max=sal_sensor_max),
  do.mgl = list(min=do_sensor_min, max=do_sensor_max),
  co2.ppm = list(min=co2_sensor_min, max=co2_sensor_max)
)
spike_thresholds = list(
  ph = list(low=spike_low_ph, high=spike_high_ph),
  temp.c = list(low=spike_low_temp, high=spike_high_temp),
  sal.ppt = list(low=spike_low_sal, high=spike_high_sal),
  do.mgl = list(low=spike_low_do, high=spike_high_do),
  co2.ppm = list(low=spike_low_co2, high=spike_high_co2)
)
# END PARAMETERIZATION #

#### Step 2: Running QA script for Barnegat Bay: ####
# filter co2 data out of Barnegat:
barnegat_filtered = subset(data_list$Barnegat, sensor.YSI == 1)
# define variables to be tested:
vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')

# RUN SCRIPT:
qa_barnegat = qaqc_nep(barnegat_filtered, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_window,
                       time_interval=sample_interval, attenuated_signal_thresholds, time_window_attsig, num_sd_for_rate_of_change, num_flatline_sus, num_flatline_fail, flatline_thresholds)

### CREATE 'flags' column to take the maximum (worst) flag across the row:
qa_barnegat = qa_barnegat %>% 
  mutate(flags = do.call(pmax, c(select(qa_barnegat, starts_with('test.')), na.rm=TRUE))) %>% 
  mutate(ph_flag = do.call(pmax, c(select(qa_barnegat, ends_with('_ph')),na.rm=TRUE)),
               do_flag = do.call(pmax, c(select(qa_barnegat, ends_with('_do.mgl')),na.rm=TRUE)),
               temp_flag = do.call(pmax,c(select(qa_barnegat, ends_with('_temp.c')),na.rm=TRUE)),
               sal_flag = do.call(pmax,c(select(qa_barnegat, ends_with('_sal.ppt')),na.rm=TRUE))
         )

# qa_data_list$Barnegat = qa_barnegat
# qa_data_list$Barnegat = qa_data_list$Barnegat |> 
#   mutate(ph_flag = do.call(pmax, c(select(qa_data_list$Barnegat, ends_with('_ph')),na.rm=TRUE)),
#          do_flag = do.call(pmax, c(select(qa_data_list$Barnegat, ends_with('_do.mgl')),na.rm=TRUE)),
#          temp_flag = do.call(pmax,c(select(qa_data_list$Barnegat, ends_with('_temp.c')),na.rm=TRUE)),
#          sal_flag = do.call(pmax,c(select(qa_data_list$Barnegat, ends_with('_sal.ppt')),na.rm=TRUE))
#   ) 
#---------

#### Step 3: Saving Options ####

# if (interactive()) {
#   if (tolower(save_Odrive_option) %in% c('y','yes')) {
#     save_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/qa_barnegat.Rdata'
#     cat('Saving qa_barnegat to:',save_path,'\n')
#     save(qa_barnegat, file=save_path)
#     cat('qa_barnegat saved successfully to O:drive. \n')
#   } else {
#     cat('Skipped saving to O:drive. \n')
#   }
#   if (tolower(save_local_option) %in% c('y','yes')) {
#     save_path = getwd()
#     cat('Saving Barnegat data locally to current directory \n')
#     save(qa_barnegat, file = paste0(getwd(),'qa_barnegat.Rdata'))
#     cat('qa_barnegat saved locally. \n')
#   }
# } else {
#   cat('Skipped saving locally. \n')
# }
