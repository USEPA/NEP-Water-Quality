# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Feb 13, 2026
# Last updated: Mar 12, 2026

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# The purpose of this script is to 
# (1) Adjust the names of San Francisco Bay column names to work with older scripts
# (2) Split the San Francisco Bay NEP data into its separate sites
# (3) Run QAQC on the individual sites, separately
# (4) Re-combine QAQC'd data into one Data Frame
# (5) Re-name columns so they use '_' instead of '.' connectors
# (6) Filter the dataset according to new flags (flags_2026)

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# (1) Adjust the names of San Francisco Bay column names to work with older scripts & ensure that '.' is used in column headers
# (1a) Re-name site.codes for SF Bay: 
# CARQ (Carquinez) --> CMA (Cal Maritime Academy)
# TIBC1 (Tiburon) --> EOS (Earth Ocean Sciences Bldg, SFSU)

# # load packages
# library(data.table)
# library(lubridate)
# library(ggplot2)
# library(scales)
# library(broom)
# library(seacarb)
# library(patchwork)
# library(gridExtra)
# library(grid)
# library(viridis) # color scale package
# library(ggsci)  # high quality color packages used in scientific journals
# library(RColorBrewer) # color mixer
# library(hexbin) # hex binning for plotting dense data
# library(tidyverse)
# library(scattermore)



# ~~~~~~~~~~~~~~~~ ALWAYS Save All the data from end of day:  ~~~~~~~~~~~~~~~~~~~####
# local_data_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project/Data/'
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/Finalized Data from NEPs/Continuous'
Odrive_data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/'

# load(paste0(Odrive_data_path,'qa_data_list_revision.Rdata'))
# load(paste0(Odrive_data_path,'pass_data_list_revision.Rdata'))
# load('Alldata.R')       # to load data
# save.image('Alldata.R') # to save after working with data / shutting down
load(paste0(Odrive_data_path,'nep_unfiltered_data_20260107-165206.Rdata'))
load(paste0(Odrive_data_path,'nep_filtered_data_20260107-165206.Rdata'))

# Set Working Directory: Adjust to local 
setwd('C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R')

# Check if column names are using _ or . --> adjust to '.' temporarily 
if (any(grepl('\\_', colnames(nep_unfiltered_data$SanFrancisco)))) {
  nep_unfiltered_data$SanFrancisco = nep_unfiltered_data$SanFrancisco %>% 
    rename_with(~ gsub('_','.', .x, fixed=TRUE))
}

# rename site codes in data
nep_unfiltered_data$SanFrancisco = nep_unfiltered_data$SanFrancisco %>% 
  mutate(site.code = case_match(site.code,
                                'TIBC1' ~ 'EOS',
                                'CARQ' ~ 'CMA',
                                .default = site.code))

# (2) Split the San Francisco Bay NEP data into its separate sites
SF_tib = nep_unfiltered_data$SanFrancisco %>% 
  filter(site.code == 'EOS')
SF_car = nep_unfiltered_data$SanFrancisco %>% 
  filter(site.code == 'CMA')

# (3) Run QAQC on the individual sites, separately
progress_print_option = readline(prompt='Would you like timestamped progress statements in R Console through the process for troubleshooting? (y/n): ')

source('qaqc_NEP_SF_Tiburon.R')
cat('SF Bay - Tiburon QA process complete. \n')
source('qaqc_NEP_SF_Carquinez.R')
cat('SF Bay - Carquinez QA process complete. \n')

# (4) Re-combine QAQC'd data into one Data Frame
sf_recombined = bind_rows(qa_sf_tib, qa_sf_car) %>% 
  arrange(datetime.utc) # and arrange chronologically
# (4b) and sort them chronologically 
# sf_recombined = sf_recombined %>% 
#   arrange(datetime.utc)

# (5) Re-name columns so they use '_' instead of '.' connectors
sf_recombined = sf_recombined %>% 
  rename_with(~ gsub('.','_',.x, fixed=TRUE))

# (6) filtering for passing *new* flags (ADJUST AS NEEDED!)
sf_filtered = sf_recombined %>% 
  filter(year(datetime_utc) > 2014,
         flags_2026 == 1)

# ####### SAVING: #########
# write_csv(sf_data, paste0(Odrive_data_path,'sf_nep_data.csv'))

# Inserting new SF Data in place of previous data:
# nep_unfiltered_data$SanFrancisco = sf_recombined
# nep_filtered_data$SanFrancisco = sf_filtered
#
# timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
# save(nep_unfiltered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_unfiltered_data_",timestamp,".Rdata"))
# save(nep_filtered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_filtered_data_",timestamp,".Rdata"))
#


### Testing block
# interp_car = interpolate_data(SF_car, vars_to_test)
# car_rolling_sd = calc_rolling_sd(interp_car, vars_to_test, sd_window_hrs = 12)
# car_attsig_test = dynamic_attenuated_test(SF_car, car_rolling_sd, vars_to_test, attenuated_signal_thresholds)
# 
# interp_tib = interpolate_data(SF_tib, vars_to_test)
# tib_rolling_sd = calc_rolling_sd(interp_tib, vars_to_test, sd_window_hrs = 12)
# tib_attsig_test = dynamic_attenuated_test(SF_tib, tib_rolling_sd, vars_to_test, attenuated_signal_thresholds)
# 
# 
# sf_segment = sf_recombined %>% 
#   filter(site_code == 'EOS') %>% 
#   filter(flags_2026 == 1) %>% 
#   filter(between(datetime_utc,
#                  as.POSIXct('2019-07-01 00:00:00'),
#                  as.POSIXct('2019-08-05 23:59:59')))
# 
# ggplot(sf_segment, aes(datetime_utc, ph, color=as.factor(flags_2026)))+
#   # ylim(5,11)+
#   geom_scattermore(pointsize = 3)+
#   scale_color_manual(values = c("0" = 'gray',"1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))
#   
# 
# # filtering only for 2015-present data to align with other NEPs:
# sf_data = sf_recombined %>% filter(year(datetime_utc) > 2014)


# # ################################### Plotting & Troubleshooting Below ################################
# 
# sample_interval = 6
# sf_car_attenuated_thresholds = list(
#   # values represent the % of "normal" variability required to not be flagged 
#   ph = list(sus = 0.15, fail = 0.05),  # suspect if variability < 15%, fail if < 5%
#   temp.c = list(sus=0.1, fail = 0.03),
#   sal.ppt = list(sus = 0.15, fail = 0.05),
#   do.mgl = list(sus = 0.2, fail = 0.1) # DO is very noisy, so we are using a wider threshold
# )
# interp_car = interpolate_data(SF_car,vars_to_test,time_interval = sample_interval)
# interp_car_withSD = calc_rolling_sd(interp_car, vars_to_test, sd_window_hrs = 12)
# sf_car_test = dynamic_attenuated_test(SF_car, interp_car_withSD, vars_to_test, sf_car_attenuated_thresholds)
# 
# ggplot(sf_car_test, aes(datetime.utc,sal.ppt,color=as.factor(test.AttenuatedSignal)))+
#   geom_scattermore(pointsize=2)+
#   scale_color_manual(values = c("0" = 'gray',"1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(0,50)
# 
# test_subset = sf_car_test %>%
#   filter(between(datetime.utc,
#                  as.POSIXct('2016-07-01 00:00:00'),
#                  as.POSIXct('2017-01-01 23:59:59')))
# 
# 
# 
# 
# SF_car_test = attenuated_signal_test_sd(SF_car, interp_car, vars_to_test, time_window=24, attsig_fail = 0.5, attsig_sus = 1.0, time_interval = sample_interval)
# car_subset = SF_car_test %>%
#   filter(between(datetime.utc,
#                  as.POSIXct('2011-04-28 00:00:00'),
#                  as.POSIXct('2011-04-30 23:59:59')))
# 
# ggplot(car_subset, aes(datetime.utc,ph, color=as.factor(test.AttenuatedSignal_ph)))+
#   geom_scattermore()+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   theme(legend.position='none')
# # 
# ggplot(sf_recombined, aes(x = as.factor(do_mgl_qc), y = as.factor(test_AttenuatedSignal_do_mgl))) +
#   geom_bin2d() +
#   scale_fill_viridis_c() +
#   labs(x = "Original QC Flag", y = "New Attenuated Test Flag",
#        title = "DO: Old vs. New Logic Agreement")
# 
# sf_data$flag_diff = sf_data$flags_2026-sf_data$flags_revision
# sf_diffs = sf_data %>% filter(flag_diff > 1 | flag_diff < -1)
# # 
# # APRIL 2015 'JUMP' IN DATA POST-CLEANING
# # 
# ggplot(sf_data, aes(datetime_utc, flag_diff))+
#   geom_point()+
#   labs(title='EPA Flags minus Caloos Flags (flags_2026 - flags_revision)',subtitle='Higher = EPA flags are catching bad data, Lower = CALOOS flags are catching')
# 
# sf_subset = sf_data %>%
#   filter(between(datetime_utc,
#                  as.POSIXct('2015-04-01 00:00:00'),
#                  as.POSIXct('2015-04-30 23:59:59'))) %>%
#   filter(site_code=='CMA')
# ggplot(sf_subset, aes(x=datetime_utc,  y=temp_c, color=as.factor(flags_2026)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   theme(legend.position = 'none')
# 
# 
# sf_subset2 = sf_data %>% 
#   filter(between(datetime_utc,
#                  as.POSIXct('2015-06-01 00:00:00'),
#                  as.POSIXct('2015-06-03 23:59:59'))) %>% 
#   filter(site_code == 'CMA')
# ggplot(sf_subset2, aes(datetime_utc,sal_ppt,color=as.factor(flags_2026)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=4,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   theme(legend.position = 'none')+
#   ylim(0,30)
# ggplot(sf_subset2, aes(datetime_utc,sal_ppt,color=as.factor(flags)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=4,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   theme(legend.position = 'none')+
#   ylim(0,30)
# 
# 
# ggplot(sf_data, aes(x=datetime_utc,  y=ph_T, color=as.factor(flags_2026)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(5,10)+
#   theme(legend.position = 'none')
# 
# ggplot(sf_data, aes(x=datetime_utc,  y=sal_ppt, color=as.factor(flags_2026)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(0,50)+
#   theme(legend.position = 'none')
# 
# # zooming in onto stretch in 2019 where pH dropped lower but still a lot of 'pass' flags
# sf_subset = sf_data %>%
#   filter(between(datetime_utc,
#                  as.POSIXct('2019-07-06 00:00:00'),
#                  as.POSIXct('2019-07-18 23:59:59'))) %>%
#   filter(site_code == 'EOS')
# ggplot(sf_subset, aes(x=datetime_utc,  y=ph_T, color=as.factor(test_Flatline_ph)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(5,10)+
#   theme(legend.position = 'none')
# 
# # same zoomed-in stretch but with new-new flatline test that flags the entire length of the flatline:
# eos_subset = tib_test %>% 
#   filter(between(datetime.utc,
#                  as.POSIXct('2019-07-06 00:00:00'),
#                  as.POSIXct('2019-07-18 23:59:59')))
# ggplot(eos_subset, aes(x=datetime.utc,  y=ph.T, color=as.factor(test.Flatline_ph)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(5,10)+
#   theme(legend.position = 'none')
# 
# # suspect/fail flags (flatline test) only 
# sf_flat_susfail = sf_data %>% filter(test_Flatline_ph > 1)
# ggplot(sf_flat_susfail, aes(x=datetime_utc,  y=ph_T, color=as.factor(test_Flatline_ph)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(5,10)+
#   theme(legend.position = 'none')
# 
# 
# 
# 
# ph_flatline_test_tib = get_flatline_lengths(SF_tib$ph.T,flatline_thresholds[['ph.T']])
# 
# sf_subset = sf_data %>% 
#   filter(between(datetime_utc,
#                  as.POSIXct('2021-03-23 00:06:00'),
#                  as.POSIXct('2021-03-24 00:06:00'))) %>% 
#   filter(site_code == 'EOS')
# 
# tol = 0.01
# n = nrow(sf_subset)
# test_subset = rep(1, n)
# for (i in 2:n) {
#   if (abs(sf_subset$ph[i] - sf_subset$ph[i-1]) < tol) {
#     test_subset[i] = test_subset[i-1] + 1
#   }
# }
# diff_subset = rep(0,n-1)
# for (i in 2:n) {
#   diff_subset[i] = abs(sf_subset$ph[i] - sf_subset$ph[i-1])
# }
# 
# ggplot(sf_subset, aes(datetime_utc, ph, color=as.factor(test_Flatline_ph)))+
#   geom_scattermore(pointsize=2.5)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))
# 
# 
# # Casco troubleshooting
# test_casco = qa_casco %>% filter(between(datetime.utc,
#                                          as.POSIXct('2022-01-01 00:06:00'),
#                                          as.POSIXct('2022-01-31 00:06:00')))
# ggplot(test_casco, aes(datetime.utc,ph, color=as.factor(test.Flatline_ph)))+
#   geom_scattermore(pointsize=3)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(7.5,8.5)
# 
# ggplot(nep_unfiltered_data$Cascobay, aes(datetime_utc,ph, color=as.factor(test_Flatline_ph)))+
#   geom_scattermore(pointsize=1.5)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   ylim(7,9)

# # pH time-series:
# ggplot(sf_filtered, aes(x=datetime_utc, y=ph_T, color=site_code))+
#   geom_scattermore(pointsize=1.2)
# 
# # Salinity time-series
# ggplot(sf_filtered, aes(x=datetime_utc, y=sal_ppt, color=site_code))+
#   geom_scattermore(pointsize=1)
# 
# # pH vs Sal, colored by site
# ggplot(sf_filtered, aes(x=sal_ppt, y=ph_T, color=site_code))+
#   geom_scattermore(pointsize=1)
# 
# # pH vs Sal, colored by flag
# ggplot(sf_filtered, aes(x=sal_ppt, y=ph_T, color=flags_2026))+
#   geom_scattermore(pointsize=1)
# 
# 
# # plotting time-series of 2014-present data that passes flags_2026, colored by old flags
# 
# # ordering sf_filtered by flag so that suspect and fail are prioritized "last" and thus rendered on "top":
# sf_filtered = sf_filtered[order(sf_filtered$flags_revision),]
# flag_counts <- sf_filtered %>%
#   count(flags_revision) %>%
#   rename(Old_Flags = flags_revision, Total = n)
# 
# 
# ggplot(sf_filtered, aes(x=datetime_utc, y=ph_T, color=as.factor(flags_revision)))+
#   geom_scattermore(pixels=c(1000,1000),pointsize=3,alpha=1)+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
# #   theme(legend.position = c(0.85,0.1))
# 


# qa_sf_test_2019 = qa_sf_tib_test %>% 
#   filter(year(datetime.utc) == 2019 & month(datetime.utc) == 7)
# 
# p = ggplot(qa_sf_test_2019, aes(x=datetime.utc, y=ph.T, color=as.factor(test.Flatline_ph.T)))+
#   geom_point()+
#   scale_color_manual(values = c("1" = "seagreen", "2" = "goldenrod", "3" = "firebrick1" ))+
#   theme(legend.position = 'none')
# ggsave('SF_Tib_flatline_pH_hightol_long_threshold.png')
# 
# 
# p = ggplot()+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '1'),aes(datetime_utc,ph_T), 
#                    color='seagreen',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '2'),aes(datetime_utc,ph_T), 
#                    color='goldenrod',pixels=c(1000,1000),pointsize=2.3,alpha=0.5)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '3'),aes(datetime_utc,ph_T), 
#                    color='firebrick1',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   labs(title = 'SF pH Time Series - passing 2026 QAQC, colored by caloos.org flags')
# ggsave('SF_flag_compare_pH.png', plot = p, width = 8, height = 8, dpi = 300)
# 
# p = ggplot()+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '1'),aes(datetime_utc,do_mgl), 
#                    color='seagreen',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '2'),aes(datetime_utc,do_mgl), 
#                    color='goldenrod',pixels=c(1000,1000),pointsize=2.3,alpha=0.5)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '3'),aes(datetime_utc,do_mgl), 
#                    color='firebrick1',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   labs(title = 'SF DO Time Series - passing 2026 QAQC, colored by caloos.org flags')
# ggsave('SF_flag_compare_DO.png', plot = p, width = 8, height = 8, dpi = 300)
# 
# p = ggplot()+
#   #layer 1 - pass
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '1'),aes(datetime_utc,sal_ppt), 
#                    color='seagreen',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '2'),aes(datetime_utc,sal_ppt), 
#                    color='goldenrod',pixels=c(1000,1000),pointsize=2.3,alpha=0.5)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '3'),aes(datetime_utc,sal_ppt), 
#                    color='firebrick1',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   labs(title = 'SF Salinity Time Series - passing 2026 QAQC, colored by caloos.org flags')
# ggsave('SF_flag_compare_Sal.png', plot = p, width = 8, height = 8, dpi = 300)
# 
# 
# p = ggplot()+
#   #layer 1 - pass
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '1'),aes(datetime_utc,temp_c), 
#                    color='seagreen',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '2'),aes(datetime_utc,temp_c), 
#                    color='goldenrod',pixels=c(1000,1000),pointsize=2.3,alpha=0.5)+
#   geom_scattermore(data=filter(sf_filtered, flags_revision == '3'),aes(datetime_utc,temp_c), 
#                    color='firebrick1',pixels=c(1000,1000),pointsize=2.3,alpha=1)+
#   labs(title = 'SF Temperature Time Series - passing 2026 QAQC, colored by caloos.org flags')
# ggsave('SF_flag_compare_Temp.png', plot = p, width = 8, height = 8, dpi = 300)
# 
# 
# 
