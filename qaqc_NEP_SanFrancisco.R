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

# POST-PROCESSED DATA:
load(paste0(Odrive_data_path,'nep_unfiltered_data_20260406-1106.Rdata'))
load(paste0(Odrive_data_path,'nep_filtered_data_20260406-1106.Rdata'))


# Set Working Directory: Adjust to local 
setwd(SF_R_path)

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
