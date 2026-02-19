# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Feb 13, 2026
# Last updated: Feb 13, 2026

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

#####################################
#    Data massaging #



################################################
#            Plotting Below                    #

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
#   theme(legend.position = c(0.85,0.1))
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
