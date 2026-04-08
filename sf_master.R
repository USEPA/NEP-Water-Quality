## Master script for SF ###

# Set file path where all SF-related scripts are downloaded to:
SF_R_path = 'C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/SF subfolder'

setwd(local_R_path)
source('qaqc_NEP_main.R')
setwd(SF_R_path)

source('qaqc_NEP_SanFrancisco.R')

setwd(SF_R_path) # re-set path 
# spike "group" adjustment to spike test: flagging points between two spikes:
source('qaqc_sf_spikegroup.R')
# perform manual QC and save re-merged file:
source('sfbay_manual_qaqc.R')

# Run script to download pressure data from raw files, match to correct site/timestamps, and combine to dataset
source('SF_pressure_parse.R')

sf_filtered_new_w_Press = sf_recombined_new_w_Press %>% 
  filter(flag_manual == 1)

#  Save simplified data frame to CSV
setwd(SF_R_path)
sf_simplified_w_Press = sf_recombined_new_w_Press %>% 
  select(datetime_utc, ph, sal_ppt, temp_c, do_mgl, depth_m, site_code, flags, flags_2026, flag_manual)
write_csv(sf_simplified_w_Press, 'NEP_SF_manualRevision_wPress.csv')

# replace San Francisco data within nep_unfiltered_data and nep_filtered_data
nep_unfiltered_data$SanFrancisco = sf_recombined_new_w_Press
nep_filtered_data$SanFrancisco = sf_filtered_new_w_Press


# Save data to O:Drive with unique timestamp:
timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
save(nep_unfiltered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_unfiltered_data_",timestamp,".Rdata"))
save(nep_filtered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_filtered_data_",timestamp,".Rdata"))

# library(ggplot2)
# library(scattermore)
# 
# plot = ggplot(sf_recombined_new_w_Press, aes(datetime_utc,depth_m,color=site_code))+
#   geom_scattermore()+
#   labs(title='San Francisco Depth Data Time Series',y='Depth (m)', x='Time')
# ggsave('SF_depth_time.png')

# sf_section = sf_recombined_new_w_Press %>% 
#   filter(between(datetime_utc,as.Date('2021-01-01'),as.Date('2021-01-31')))
# plot = ggplot(sf_section, aes(datetime_utc,depth_m,color=site_code))+
#     geom_point()+
#     labs(title='San Francisco Depth Data Time Series',subtitle='Jan 2021',y='Depth (m)', x='Time')
# ggsave('SF_depth_time_jan2021.png')
