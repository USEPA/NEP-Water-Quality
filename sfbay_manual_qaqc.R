# Manual review of San Francisco NEP data by S. Pacella
# Last update: 3/18/2026
# Dependencies: requires 'sf_data' created by running qaqc_NEP_SanFrancisco pulled from github 3/18/26

# Break data up into CMS and EOS stations specifically
# Assumes sf_data is already loaded in the workspace.

# Set Working Directory: Adjust to local 
setwd(local_R_path)


# ------------------------------------------------------------
# R script: MATLAB-to-R translation with ggplot and file output
# Assumes `sf_data` is already in the workspace.
# All figures are saved to the "figs" directory as publication-quality PNGs.
# ------------------------------------------------------------

# Packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(patchwork)   # for multi-panel layouts
  library(scales)
})

# -------------------------
# 1) Split stations & add manual QC column defaults
# -------------------------
sf_nep_cma <- sf_nep_cma %>% mutate(flag_manual = flag_max)
sf_nep_eos <- sf_nep_eos %>% mutate(flag_manual = flag_max)

# -------------------------
# 2) Manual flags for pH: CMA
# -------------------------
dt_cma <- sf_nep_cma$datetime_utc

# 2.1 Flag: 2015-05-12 18:43:00 to 2015-05-12 18:44:00 (UTC)
tStart <- as.POSIXct("2015-05-12 18:43:00", tz = "UTC")
tEnd   <- as.POSIXct("2015-05-12 18:44:00", tz = "UTC")
mask <- dt_cma >= tStart & dt_cma <= tEnd
sf_nep_cma$flag_manual[mask] <- 2

# 2.4 Erratic CMA pH: Jun-Aug 2017 (inspect)
tStart <- as.POSIXct("2017-06-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2017-08-30 00:00:00", tz = "UTC")
mask_2017 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd

# 2.5 Spikey pH: Jul 28–31 2021 -> flag suspect
tStart <- as.POSIXct("2021-07-28 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2021-07-31 00:00:00", tz = "UTC")
mask_2021 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2021] <- 2

# Erratic pH: Aug 4–16 2017 -> flag suspect
tStart <- as.POSIXct("2017-08-04 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2017-08-16 19:00:00", tz = "UTC")
mask_2017b <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2017b] <- 2

# Outlier pH: Sep 10 2015 -> flag suspect
tStart <- as.POSIXct("2015-09-10 20:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2015-09-10 22:00:00", tz = "UTC")
mask_2015 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2015] <- 2

# Random outlier pH values -> flag suspect
tStart <- as.POSIXct("2015-05-12 12:18:00", tz = "UTC")
tEnd   <- as.POSIXct("2015-05-12 12:19:00", tz = "UTC")
mask_2015 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2015] <- 2

tStart <- as.POSIXct("2015-06-03 00:07:00", tz = "UTC")
tEnd   <- as.POSIXct("2015-06-03 00:08:00", tz = "UTC")
mask_2015 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2015] <- 2

tStart <- as.POSIXct("2017-08-14 19:25:00", tz = "UTC")
tEnd   <- as.POSIXct("2017-08-14 19:26:00", tz = "UTC")
mask_2017 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2017] <- 2


# -------------------------
# 3) EOS station
# -------------------------
dt_eos <- sf_nep_eos$datetime_utc


# 3.5 EOS: Flag suspect for 2015-01-01 to 2016-02-25
tStart <- as.POSIXct("2015-01-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2016-02-25 00:00:00", tz = "UTC")
mask <- dt_eos >= tStart & dt_eos <= tEnd
sf_nep_eos$flag_manual[mask] <- 2

# 3.6 EOS: Large change around Feb 2018 (inspect window)
tStart <- as.POSIXct("2017-11-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2018-05-01 00:00:00", tz = "UTC")
mask_2018 <- sf_nep_eos$datetime_utc >= tStart & sf_nep_eos$datetime_utc <= tEnd


# 3.7 EOS: Drop in pH July 2019 (inspect window)
tStart <- as.POSIXct("2019-06-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2019-10-01 00:00:00", tz = "UTC")
mask_2019 <- sf_nep_eos$datetime_utc >= tStart & sf_nep_eos$datetime_utc <= tEnd


# EOS: Flag suspect pH data in 2019
tStart <- as.POSIXct("2019-07-04 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2019-08-10 00:00:00", tz = "UTC")
mask <- dt_eos >= tStart & dt_eos <= tEnd
sf_nep_eos$flag_manual[mask] <- 2


# -------------------------------------------------
# 4) Re-merge the two data frames with manual plotting: : sf_nep_eos and sf_nep_cma:
# -------------------------------------------------
sf_recombined_new = bind_rows(sf_nep_eos, sf_nep_cma) %>% 
  arrange(datetime_utc) # and arrange chronologically

sf_filtered_new = sf_recombined_test %>% 
  filter(year(datetime_utc) > 2014,
         flag_manual == 1)

# 4.1) replace San Francisco data within nep_unfiltered_data and nep_filtered_data
nep_unfiltered_data$SanFrancisco = sf_recombined_new
nep_filtered_data$SanFrancisco = sf_filtered_new


# 5) Create additional data frame with only critical data to share: timestamp, data, site, flags, flags_2026, flags_manual
    # flags = CALOOS flags
    # flags_2026 = our automated flagging system
    # flags_manual = flags_2026 + our manual review
sf_simplified = sf_recombined_new %>% 
  select(datetime_utc, ph, sal_ppt, temp_c, do_mgl, flags, flags_2026, flag_manual)

# 5.1) Save simplified data frame to CSV
write_csv(sf_simplified, 'NEP_SF_manualRevision.csv')
