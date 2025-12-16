# Stephen R. Pacella
# EPA Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: June 25, 2025
# Last updated: Dec 16, 2025
# Edits by Andrew Mandovi (ORISE) denoted by 'AWM' initials

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    This R script performs the following: 
#                    -------------------------------------
#  1. Revises qa_data_list following internal EPA reviews from 1). Pimenta and 2). Alford
#  2. Updates Tillamook data with latest QC version 12/9/2025
#  3. Creates an output file containing the updated datasets: qa_data_list_revision.Rdata and pass_data_list_revision.Rdata
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# library(plyr) #Earlier version of dplyr
library(tidyverse) #User-friendly data manipulations
library(lubridate) #Manipulating all sorts of date types
library(magrittr) #Pipe operations
library(ggplot2) #universal plotting tools
library(scales) #Scale functions for visualizations
library(seacarb) #Calculating carbonate system calculations
library(mltools) #Machine learning tools 
library(dplyr) #Easy dataframe manipulations
library(Metrics) #Metrics for machine learning
library(readxl) #Load in Excel spreadsheets
library(gtools) #Misc statistical tools
library(writexl) #Write Excel files to txt, etc
library(lmodel2) #Model II linear regression
library(stringr) #Easier manipulation of with character strings
library(janitor) #Misc cleaning tools
library(readr) #Read tables
library(svDialogs) # Dialogue box
library(ggpubr) #Arrange ggplots
library(RColorBrewer) #ggplot colors
library(ggthemes)
library(data.table)
library(broom)
library(patchwork)
library(gridExtra)
library(grid)
library(viridis) # color scale package
library(ggsci)  # high quality color packages used in scientific journals
library(hexbin) # hex binning for plotting dense data
library(dataCompareR)
# Load original qa_data_list from O: drive
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/'
load(paste0(data_path,'qa_data_list.Rdata'))
Odrive_data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/'


##### Barnegat Bay ###############################################

qa_data_list$Barnegat$flags_revision <- qa_data_list$Barnegat$flags

#Comment: Negative temperature values marked with flag #2
#Revision: No revision needed; flag == 2 means suspect



#Comment: Negative turbidity values marked with flag #2
#Revision: No revision needed; turbidity can sometimes read negative in low turbidity waters


##### Casco Bay ###############################################

qa_data_list$Cascobay$flags_revision <- qa_data_list$Cascobay$flags

# ---- AWM: 12.11.25 Rewrote following block (received error, now using dplyr functions) ----
#Comment: Negative depths and alkalinity values marked with flag #1
#Revision: Flag depths < 0 and alk < 0 as "2"
qa_data_list$Cascobay = qa_data_list$Cascobay %>% 
  mutate(flags_revision = if_else(
    depth.m < 0 & flags == 1, # if depth is negative & flags == 1
    2,                        # then: make flags_revision = 2
    flags_revision)) %>%      # else: keep same value 
  mutate(flags_revision = if_else(
    alk.mgl < 0 & flags == 1,
    2,
    flags_revision))
#Comment: Alk.mgl order of magnitude too high???? I think this is the case or the column name is incorrect.
#Revision: adjusted columns to properly be named alk.umolkg, which aligns with other uses. 
if ('alk.mgl' %in% names(qa_data_list$Cascobay)) {
  qa_data_list$Cascobay = qa_data_list$Cascobay %>% 
    rename(alk.umolkg = alk.mgl)
}
# ---- AWM: end 12.11.25 edited block ----

#Compare old vs new flags
# qa_data_list$Cascobay[qa_data_list$Cascobay$flags != qa_data_list$Cascobay$flags_revision, ]


##### Coastal Bend ###############################################

qa_data_list$Coastalbend$flags_revision <- qa_data_list$Coastalbend$flags

#Comment: No issues identified with dataset
#Revision: No change needed


##### Delaware Inland Bays ###############################################

qa_data_list$DelawareInland$flags_revision <- qa_data_list$DelawareInland$flags

#Comment:	pH values marked as acceptable have values as low as 5.44 and as high as 9.77
#Revision: SRP checked and flags == 1 only includes pH from ~7-9, so no change needed

#Comment: DO.mgL and DO% unreasonably low values
#Revision: SRP checked and values with flag == 1 look reasonable; no change needed

#Comment: DO.mgL and DO% unreasonably high values
#Revision: SRP checked and values with flag == 1 look reasonable; no change needed

#Comment: Unreasonable high values for DO.mgL and DO%
#Revision: SRP checked and values with flag == 1 look reasonable; no change needed

#Comment: Negative values for temp.c
#Revision: SRP checked and values with flag == 1 look reasonable; no change needed

#Comment: High values for temp.c (~38)
#Revision: SRP checked and values with flag == 1 look reasonable; no change needed

##### Indian River Lagoon ###############################################

qa_data_list$IndianRiverLagoon$flags_revision <- qa_data_list$IndianRiverLagoon$flags

# ## testing:
# irl_limit = qa_data_list_revision$IndianRiverLagoon[qa_data_list_revision$IndianRiverLagoon$co2.ppm < 10000]
# # plot(irl_limit$datetime.utc,irl_limit$co2.ppm)
# 
# irl_filter = qa_data_list_revision$IndianRiverLagoon %>% 
#   filter(co2.ppm < 10,000 & datetime.utc > cutoff_date)
# 
# # plotting IRL data with filtered "cap" on co2 data
# # ggplot(irl_filter, aes(datetime.utc,co2.ppm,color = flags))+
# #   geom_point()+
# #   ylim(c(0,10000))
# 
# ## /testing

#Comment: co2.ppm values flagged as good
#Revision: no change needed

#Comment: co2.ppm values >1,000,000 flagged as good
#Revision: Flag co2.ppm values > 2,000 as suspect, NEED TO CONTACT IRL NEP !!! [AWM to SRP: was this resolved? !~!]
# ---- AWM: 12.11.25 re-wrote the following section to use dplyr: ----
qa_data_list$IndianRiverLagoon = qa_data_list$IndianRiverLagoon %>% 
  mutate(flags_revision = if_else(
    co2.ppm > 2000 & flags == 1, # if co2 > 2000 ppm & flags == 1...
    2,                           # then: make flags_revision = 2
    flags_revision               # else: keep the same
  )) %>% 
  #Comment: 10/18/2022 12:00 do.mgl of 44.42 flagged as good
  #Revision: Flagging do.mgl values > 20 as suspect "2"
  mutate(flags_revision = if_else(
    do.mgl > 20 & flags == 1,
    2,
    flags_revision
  )) %>% 
  #Comment: Sal.ppt values ~66 flagged as good
  #Revision: Salinity data > 40 should be flagged as suspect according to 7/9/25 email from Kristen Davis
  mutate(flags_revision = if_else(
    sal.ppt > 40 & flags == 1,
    2,
    flags_revision
  ))
# ---- AWM: end 12.11.25 edited block ----

#Comment: large number of do.mgl values at 0.00, these are flagged as good
#Revision: No change needed

#Comment: Negative depth values flagged as good
#Revision: SRP checked and no negative values flagged with 1; no change needed

#Comment:	9/19/2023 4:00 temp.c of 442.83 flagged as good, this pattern repeats with other unreasonably high temperature values
#Revision:SRP checked on and no temps >36 flagged with 1; no change needed

#Comment: ph values < 4.0 routinely flagged good
#Revision: SRP checked and no pH values <4 flagged with 1; no change needed

#Comment: ph values > 10.0 routinely flagged good
#Revision: SRP checked and no pH values >10 flagged with 1; no change needed




##### Long Island Sound ###############################################

qa_data_list$LongIslandSound$flags_revision <- qa_data_list$LongIslandSound$flags

#Comment: Flags not present for any data
#Revision: Data aleady delivered post-QC; no revision needed

# ---- AWM: 12.11.25 re-wrote the following section to use dplyr: ----
#Comment: Large number of unreasonably low ph and ph.T values
#Revision: Flag pH values <6 and >9 as suspect "2"
qa_data_list$LongIslandSound = qa_data_list$LongIslandSound %>% 
  mutate(flags_revision = if_else(
    ph.T < 6 & flags == 1,
    2,
    flags_revision
  )) %>% 
  mutate(flags_revision = if_else(
    ph.T > 9 & flags == 1,
    2,
    flags_revision
  )) %>% 
#Comment: Large number of negative do.mgl values
#Revision: Flag do.mgl values <0 as suspect
  mutate(flags_revision = if_else(
    do.mgl < 0 & flags == 1,
    2,
    flags_revision
  ))
# ---- AWM: end 12.11.25 edited block ----


##### Mobile Bay ###############################################

qa_data_list$Mobile$flags_revision <- qa_data_list$Mobile$flags

#Comment: Flag labels <-3> [STF] do not match description in NEP_data_column_descriptions.xlsx file
#Revision: 
# --> Andrew 12/5/25: [STF] refers to "catestrophric temperature sensor failure" which matches the temp.c values all = 50 C.
# Resolution: added description for [STF] among others in NEP_data_column_descriptions.xlsx file on O:drive (folder /5. .../ ). 

#Comment: Reasonable temp.c values marked with F_temp flag <4>
#Revision: no change needed

#Comment: Reasonable sal.ppt values marked with F_Sal flag <-3> [SSM] (CSM)
#Revision: no change needed

#Comment: Reasonable ph values marked with F_ph <-3> [GSM] and <-3> [STF]
#Revision: no change needed

#Comment: It would be good to simplify MOBILE BAY QA codes 
#Revision: no change needed

##### Morro ###############################################

qa_data_list$Morro$flags_revision <- qa_data_list$Morro$flags

#No issues identified by Pimenta

##### Narragansett ###############################################

qa_data_list$Narrgansett$flags_revision <- qa_data_list$Narrgansett$flags
# replace mispelled 'Narrgansett' with correctly spelled 'Narragansett':
qa_data_list$Narragansett <- qa_data_list$Narrgansett
qa_data_list[["Narrgansett"]] <-NULL
# re-alphabetize qa_data_list:
qa_data_list = qa_data_list[order(names(qa_data_list))]

#Comment:	Flags not present for any data
#Revision: No change needed

#Comment:	Sal.ppt values > 30 seems unlikely for site F4
#Revision: No change needed

#Comment:	Negative temperature values present


#Comment:	Depth.m values > 37 unlikely at site F3


#Comment:	Unreasonably high do.mgl values, many instances of > 50 mgl values


#Comment:	ph and phT values < 4 and > 9 present


##### NYNJH ###############################################

qa_data_list$NYNJH$flags_revision <- qa_data_list$NYNJH$flags
attr(qa_data_list$NYNJH$datetime.utc, "tzone") <- "UTC"

#Comment: Unreasonably high do.mgl values
#Revision: SRP checked - no change needed

#Comment:	Negative do.mgl values present
#Revision: SRP checked - no change needed

#Comment: When do.mgl values are > 80, it seems possible that do.mgl and do.pct values were switched.
#Revision: SRP checked - no change needed

#Comment:	flag.do.mgl displays R (reject) when do.mgl values look acceptable
#Revision: SRP checked - no change needed

#Comment:	flag.do.pct displays R (reject) when do.pct values look acceptable
#Revision: SRP checked - no change needed

#Comment:	flag.ph sal.ppt displays R (reject) when ph sal.ppt values look acceptable
#Revision: SRP checked - no change needed

#Comment:	flag.temp.c displays R (reject) when temp.c values look acceptable
#Revision: SRP checked - no change needed

#Comment:	negative ph values flagged with S (suspect) rather than R (reject)
#Revision: SRP checked - no change needed

# ---- AWM: 12.11.25 re-wrote the following section to use dplyr: ----
#Comment:	ph values > 9 flagged with A (accept)
#Revision: Flagging values <6 and >9 as suspect (2)
qa_data_list$NYNJH = qa_data_list$NYNJH %>% 
  mutate(flags_revision = if_else(
    ph.T < 6 & flags == 1,
    2,
    flags_revision
  )) %>% 
  mutate(flags_revision = if_else(
    ph.T > 9 & flags == 1,
    2,
    flags_revision
  ))
# ---- AWM: end 12.11.25 edited block ----

##### Pensacola ###############################################

qa_data_list$Pensacola$flags_revision <- qa_data_list$Pensacola$flags

#No issues identified with dataset


##### San Francisco ###############################################

qa_data_list$SanFrancisco$flags_revision <- qa_data_list$SanFrancisco$flags

#Comment: Note: the format of this dataset is likely the easiest to understand for someone unfamiliar with how the NEP data was collected. Data columns followed by flag columns make this easy to understand.
#Revision: no change needed

#Comment:	ph.tot.qc displays 3 (suspect) when ph values = 0
#Revision: no change needed
#Comment: ph.tot.qc displays 4 (fail) when ph values = ~
#Revision: no change needed
#Comment: sal.ppt.qc displays 1 (pass) when sal values > 40, these salinity values are unlikely at site: CARQ, 38.0657 -122.2302
#Revision: NEED TO COME BACK TO !!! [!~! was this addressed?] --> continued around line ~683


##### Tampa ###############################################

qa_data_list$Tampa$flags_revision <- qa_data_list$Tampa$flags

#Comment: Are flags 1 and 15 interchangeable? Reasonable values for sal.ppt, do.mgl, and ph.tot are flagged as both 1 and 15.

#Comment: qf.co2 displays 4 (????) when co2.ppm values = 0, if 4 means fail in this context, then proceed. It appears  from the “NEP_data_columns.xlsx” that every number from 1 – 15 might indicate pass. If this is the case then there are very larg amount of -99 and 0 values for: “temp.c", "depth.m", "sal.ppt", "ph.tot", "co2.ppm", "do.mgl" that need to be marked as failed. 
#Revision: NEED TO COME BACK TO !!! [!~! was this addressed?]


##### Tillamook ###############################################

qa_data_list$Tillamook$flags_revision <- qa_data_list$Tillamook$flags

#Comment: "flags_seafet", "flags_seaphox", "flags_samico2" displays 3 (fail) for a large number of measurements that look reasonable for all applicable columns. There may be another, internal reason that this data was flagged though. i.e. all data from 2018-05-10 02:15:00 displays 3 (fail) for flags_seafet. This block of data looks acceptable to me. 
#Revision: no change needed

#Comment: Columns: "ph.T", "sal.ppt", "temp.c", "depth.m", "do.mgl" has a lot of unreasonable, unflagged data but it is unclear how these parameters were collected.
#Revision: SRP checked - No change needed

# Tillamook data comprehensively reviewed by SRP and updated 12.9.2025. Replace previous Tillamook with this new data
data_path_tillamook = "O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/Tillamook final files/qa_data_list_tillamook.csv"
# Load necessary package
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

# Import the CSV file into R
data_tillamook <- read.csv(data_path_tillamook, stringsAsFactors = FALSE)

# Assume the timestamp column is named 'Timestamp'

# ---- AWM: 12.11.25 re-wrote the following section: ----
# Convert the 'Timestamp' column to POSIXct format with timezone set to PST (GMT -8)
data_tillamook$datetime_pst <- as.POSIXct(data_tillamook$datetime_pst, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

# create datetime_utc column
data_tillamook$datetime_utc = with_tz(data_tillamook$datetime_pst, tzone='UTC')
# ---- AWM: end 12.11.25 edited block ----

# Replace old tillamook data with new
qa_data_list$Tillamook <- data_tillamook



##### PugetSound ###############################################

qa_data_list$PugetSound$flags_revision <- qa_data_list$PugetSound$flags
#Comment:	No QA flag glossary given for this site
#Revision: Added to NEP_data_column_descriptions.xlsx

#Comment:	All data looks reasonable
#Revision: No change needed

#Comment:	sal.ppt vs. alk.umolkg, sal.ppt vs. DIC_UMOL_KG, do.mgl vs. pco2.ppm regressions all look good.
#Revision: No change needed


################################################################################
################################################################################
## Create new dataframe with "pass" data only based on flags_revision column ###
################################################################################

pass_data_list_revision <- list()

# Create new qa_data_list based on revisions
qa_data_list_revision <- qa_data_list


## Create cutoff date to filter data_list with, for 2015-present
cutoff_date = as.POSIXct('2014-12-31 23:59:59',format='%Y-%m-%d %H:%M:%S', tz='UTC')

####   Filter data with new flags_revision column ####
# Barnegat
pass_data_list_revision$Barnegat = qa_data_list_revision$Barnegat |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# CascoBay
pass_data_list_revision$Cascobay = qa_data_list_revision$Cascobay |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Coastalbend
pass_data_list_revision$Coastalbend = qa_data_list_revision$Coastalbend |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# DelawareInland
pass_data_list_revision$DelawareInland = qa_data_list_revision$DelawareInland |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# IndianRiverLagoon
pass_data_list_revision$IndianRiverLagoon = qa_data_list_revision$IndianRiverLagoon |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# LongIslandSound
pass_data_list_revision$LongIslandSound = qa_data_list_revision$LongIslandSound |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Mobile
pass_data_list_revision$Mobile = qa_data_list_revision$Mobile |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Morro
pass_data_list_revision$Morro = qa_data_list_revision$Morro |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Narragansett
pass_data_list_revision$Narragansett = qa_data_list_revision$Narragansett |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# NYNJH
pass_data_list_revision$NYNJH = qa_data_list_revision$NYNJH |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Pensacola
pass_data_list_revision$Pensacola = qa_data_list_revision$Pensacola |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# SanFrancisco
pass_data_list_revision$SanFrancisco = qa_data_list_revision$SanFrancisco |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Tampa
pass_data_list_revision$Tampa = qa_data_list_revision$Tampa |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)
# Tillamook
# Tillamook data comprehensively reviewed by SRP and updated 12.9.2025. Replace previous Tillamook with this new data
data_path_tillamook_pass = "O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/Tillamook final files/pass_data_list_tillamook.csv"

# Import the CSV file into R
data_tillamook_pass <- read.csv(data_path_tillamook_pass, stringsAsFactors = FALSE)

# ---- AWM: 12.11.25 re-wrote the following section: ----
# Convert the 'Timestamp' column to POSIXct format with timezone set to PST (GMT -8)
data_tillamook_pass$datetime_pst <- as.POSIXct(data_tillamook_pass$datetime_pst, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

# create datetime_utc column
data_tillamook_pass$datetime_utc = with_tz(data_tillamook_pass$datetime_pst, tzone='UTC')
# ---- AWM: end 12.11.25 edited block ----

# Replace old tillamook data with new
pass_data_list_revision$Tillamook <- data_tillamook_pass

pass_data_list_revision$Tillamook = pass_data_list_revision$Tillamook |>
  filter(datetime_pst > cutoff_date)
# PugetSound
pass_data_list_revision$PugetSound = qa_data_list_revision$PugetSound |>
  filter(datetime.utc > cutoff_date & flags_revision == 1)

###############################################################################
###########    Additional Edits by Andrew, Nov 20 2025     ##################
###############################################################################



#Comment: BPP real time 2022 in comment field vs. project field
#Revision: ASK ANDREW # solved
# --> Andrew, 12/5/25: Checked raw data. Some had this under 'Project' and others under 'Comments'. They should be merged under 'Project' with 'Comments' removed (all blank):
qa_data_list_revision$Barnegat = qa_data_list_revision$Barnegat %>% 
  mutate(Project = case_when(
    Project == '' ~ Comments,
    TRUE ~ Project
  )) %>% 
  select(-Comments)
pass_data_list_revision$Barnegat = pass_data_list_revision$Barnegat %>% 
  mutate(Project = case_when(
    Project == '' ~ Comments,
    TRUE ~ Project
  )) %>% 
  select(-Comments) 

#### Removing unnecessary co2.ppm columns: ####
# Remove co2.ppm columns for NEPs that don't have: Bar, Del, Cas, Mob, Pen
qa_data_list_revision$Cascobay = qa_data_list_revision$Cascobay %>% select(-co2.ppm)
pass_data_list_revision$Cascobay = pass_data_list_revision$Cascobay %>% select(-co2.ppm)
qa_data_list_revision$DelawareInland = qa_data_list_revision$DelawareInland %>% select(-co2.ppm)
pass_data_list_revision$DelawareInland = pass_data_list_revision$DelawareInland %>% select(-co2.ppm)
qa_data_list_revision$Mobile = qa_data_list_revision$Mobile %>% select(-co2.ppm)
pass_data_list_revision$Mobile = pass_data_list_revision$Mobile %>% select(-co2.ppm)
qa_data_list_revision$Pensacola = qa_data_list_revision$Pensacola %>% select(-co2.ppm)
pass_data_list_revision$Pensacola = pass_data_list_revision$Pensacola %>% select(-co2.ppm)
qa_data_list_revision$Narragansett = qa_data_list_revision$Narragansett %>% select(-co2.ppm)
pass_data_list_revision$Narragansett = pass_data_list_revision$Narragansett %>% select(-co2.ppm)
qa_data_list_revision$SanFrancisco = qa_data_list_revision$SanFrancisco %>% select(-co2.ppm)
pass_data_list_revision$SanFrancisco = pass_data_list_revision$SanFrancisco %>% select(-co2.ppm)

# Barnegat: chose not to include CO2Pro ppm data due to unreliable data quality
qa_data_list_revision$Barnegat = qa_data_list_revision$Barnegat %>% select(-co2.ppm,-sensor.CO2pro,-sensor.CO2Pro)
pass_data_list_revision$Barnegat = pass_data_list_revision$Barnegat %>% select(-co2.ppm,-sensor.CO2pro,-sensor.CO2Pro)

qa_data_list_revision$LongIslandSound = qa_data_list_revision$LongIslandSound %>% select(-co2.ppm,sensor.SAMICO2) 
pass_data_list_revision$LongIslandSound = pass_data_list_revision$LongIslandSound %>% select(-co2.ppm,sensor.SAMICO2)

qa_data_list_revision$Morro$sensor = 'Multiple'
pass_data_list_revision$Morro$sensor = 'Multiple'

#### Long Island Sound: ####
# Alkalinity taken discretely - needs to reflect that in sensor information:
qa_data_list_revision$LongIslandSound = qa_data_list_revision$LongIslandSound %>% 
  # if !is.na(alk.mgl), then set sensor.Discrete to 1
  mutate(
    sensor.Discrete = case_when(
      !is.na(alk.mgl) ~ 1,
      TRUE ~ 0)
  ) %>% 
  mutate(
    across( # where USGS took samples, make all non-YSI sensors == 0
      .cols = c(sensor.SeaFET,sensor.Hydrocat),
      .fns = ~ if_else(startsWith(site.code, 'USGS_'), 0, .)
    ),
    sensor.Discrete = case_when( # all USGS data discrete
      startsWith(site.code, 'USGS_') ~ 1,
      TRUE ~ sensor.Discrete
    ),
    sensor.YSI = case_when( # IEC data taken by YSI
      startsWith(site.code,'IEC_') & !is.na(ph.T) ~ 1,
      TRUE ~ sensor.YSI
    ),
    sensor.SeaFET = case_when( # IEC data is not from SeaFET
      startsWith(site.code,'IEC_') ~ 0,
      TRUE ~ sensor.SeaFET
    ),
    sensor.Hydrocat = case_when( # when !is.na(do.mgl) AND DO data not taken from YSI: Hydrocat used
      !is.na(do.mgl) & sensor.YSI == 0 ~ 1,
      TRUE ~ sensor.Hydrocat
    )
  )
pass_data_list_revision$LongIslandSound = pass_data_list_revision$LongIslandSound %>% 
  # if !is.na(alk.mgl), then set sensor.Discrete to 1
  mutate(
    sensor.Discrete = case_when(
      !is.na(alk.mgl) ~ 1,
      TRUE ~ 0)
  ) %>% 
  mutate(
    across( # where USGS took samples, make all non-YSI sensors == 0
      .cols = c(sensor.SeaFET,sensor.Hydrocat),
      .fns = ~ if_else(startsWith(site.code, 'USGS_'), 0, .)
    ),
    sensor.Discrete = case_when( # all USGS data discrete
      startsWith(site.code, 'USGS_') ~ 1,
      TRUE ~ sensor.Discrete
    ),
    sensor.YSI = case_when( # IEC data taken by YSI
      startsWith(site.code,'IEC_') & !is.na(ph.T) ~ 1,
      TRUE ~ sensor.YSI
    ),
    sensor.SeaFET = case_when( # IEC data is not from SeaFET
      startsWith(site.code,'IEC_') ~ 0,
      TRUE ~ sensor.SeaFET
    ),
    sensor.Hydrocat = case_when( # when !is.na(do.mgl) AND DO data not taken from YSI: Hydrocat used
      !is.na(do.mgl) & sensor.YSI == 0 ~ 1,
      TRUE ~ sensor.Hydrocat
    )
  )

#### Morro Bay:  ####
# adjust Morro Bay sensor naming:
qa_data_list_revision$Morro = qa_data_list_revision$Morro %>% 
  mutate(
    sensor = case_when(
      sensor.SeaFET == 1 & sensor.Optode == 0 ~ 'Sea-Bird SeaFET',
      sensor.SeaFET == 0 & sensor.Optode == 1 ~ 'Aandera Optode',
      sensor.SeaFET == 1 & sensor.Optode == 1 ~ 'Multiple',
      TRUE ~ NA_character_
    )
  )
pass_data_list_revision$Morro = pass_data_list_revision$Morro %>% 
  mutate(
    sensor = case_when(
      sensor.SeaFET == 1 & sensor.Optode == 0 ~ 'Sea-Bird SeaFET',
      sensor.SeaFET == 0 & sensor.Optode == 1 ~ 'Aandera Optode',
      sensor.SeaFET == 1 & sensor.Optode == 1 ~ 'Multiple',
      TRUE ~ NA_character_
    )
  )

# add in biofouled data
morro_biofoul = read_excel(paste0(Odrive_data_path,'Morro_Biofouling_Auto_Manual_Flags.xlsx'))
# 1. prepare biofouling data
morro_biofoul_processed = morro_biofoul %>% 
  mutate(
    # Create the start time column
    start_time_pdt = force_tz(ymd_hm(
      paste(Start_YYYY, Start_MM, Start_DD, Start_HH, Start_mm),
      tz = "America/Los_Angeles" # Ensure the timezone matches the 'data' dataframe
    ), tzone = "America/Los_Angeles"),
    # Create the end time column
    end_time_pdt = force_tz(ymd_hm(
      paste(End_YYYY, End_MM, End_DD, End_HH, End_mm),
      tz = "America/Los_Angeles" # Ensure the timezone matches the 'data' dataframe
    ), tzone = "America/Los_Angeles"),
    # Convert PDT times to UTC
    start_time_utc = with_tz(start_time_pdt, tzone = 'UTC'),
    end_time_utc = with_tz(end_time_pdt, tzone = 'UTC')
  ) %>%
  # Select only the necessary columns for joining and comparison
  select(site.code, start_time_utc, end_time_utc)


# 2.  through biofouling periods and update flags_revision accordingly
for (i in 1:nrow(morro_biofoul_processed)) {
  # Get the current biofouling record
  period <- morro_biofoul_processed[i, ]
  # Update 'flags_revision' to 2 for rows that match the site.code AND fall within the current start and end times
  qa_data_list_revision$Morro <- qa_data_list_revision$Morro %>%
    mutate(
      flags_revision = if_else(
        # if: 
        site.code == period$site.code & datetime.utc >= period$start_time_utc & datetime.utc <= period$end_time_utc & flags_revision == 1,
        # then:
        2, 
        # else:
        flags_revision # Keep the existing value 
      )
    )
  pass_data_list_revision$Morro <- pass_data_list_revision$Morro %>%
    mutate(
      flags_revision = if_else(
        # if: 
        site.code == period$site.code & datetime.utc >= period$start_time_utc & datetime.utc <= period$end_time_utc & flags_revision == 1,
        # then:
        2, 
        # else:
        flags_revision # Keep the existing value 
      )
    )
}




####  San Francisco Salinity Issue: continued from lines ~334-335 ####

# plot to view time-series of "passing" salinity data for SF:
#plot(pass_data_list_revision$SanFrancisco$datetime.utc,pass_data_list_revision$SanFrancisco$sal.ppt)


###############################################################################
###########    Additional Edits by Andrew, Dec 11-16 2025     #################
###############################################################################

####### Update column names for all NEPs when necessary to eliminate '.' in names, for easier use across R/Python/Matlab ######
colnames(qa_data_list_revision$Barnegat) = gsub('.','_',colnames(qa_data_list_revision$Barnegat), fixed=TRUE)
colnames(pass_data_list_revision$Barnegat) = gsub('.','_',colnames(pass_data_list_revision$Barnegat), fixed=TRUE)
colnames(qa_data_list_revision$Cascobay) = gsub('.','_',colnames(qa_data_list_revision$Cascobay), fixed=TRUE)
colnames(pass_data_list_revision$Cascobay) = gsub('.','_',colnames(pass_data_list_revision$Cascobay), fixed=TRUE)
colnames(qa_data_list_revision$Coastalbend) = gsub('.','_',colnames(qa_data_list_revision$Coastalbend), fixed=TRUE)
colnames(pass_data_list_revision$Coastalbend) = gsub('.','_',colnames(pass_data_list_revision$Coastalbend), fixed=TRUE)
colnames(qa_data_list_revision$DelawareInland) = gsub('.','_',colnames(qa_data_list_revision$DelawareInland), fixed=TRUE)
colnames(pass_data_list_revision$DelawareInland) = gsub('.','_',colnames(pass_data_list_revision$DelawareInland), fixed=TRUE)
colnames(qa_data_list_revision$IndianRiverLagoon) = gsub('.','_',colnames(qa_data_list_revision$IndianRiverLagoon), fixed=TRUE)
colnames(pass_data_list_revision$IndianRiverLagoon) = gsub('.','_',colnames(pass_data_list_revision$IndianRiverLagoon), fixed=TRUE)
colnames(qa_data_list_revision$LongIslandSound) = gsub('.','_',colnames(qa_data_list_revision$LongIslandSound), fixed=TRUE)
colnames(pass_data_list_revision$LongIslandSound) = gsub('.','_',colnames(pass_data_list_revision$LongIslandSound), fixed=TRUE)
colnames(qa_data_list_revision$Mobile) = gsub('.','_',colnames(qa_data_list_revision$Mobile), fixed=TRUE)
colnames(pass_data_list_revision$Mobile) = gsub('.','_',colnames(pass_data_list_revision$Mobile), fixed=TRUE)
colnames(qa_data_list_revision$Morro) = gsub('.','_',colnames(qa_data_list_revision$Morro), fixed=TRUE)
colnames(pass_data_list_revision$Morro) = gsub('.','_',colnames(pass_data_list_revision$Morro), fixed=TRUE)
colnames(qa_data_list_revision$Narragansett) = gsub('.','_',colnames(qa_data_list_revision$Narragansett), fixed=TRUE)
colnames(pass_data_list_revision$Narragansett) = gsub('.','_',colnames(pass_data_list_revision$Narragansett), fixed=TRUE)
colnames(qa_data_list_revision$NYNJH) = gsub('.','_',colnames(qa_data_list_revision$NYNJH), fixed=TRUE)
colnames(pass_data_list_revision$NYNJH) = gsub('.','_',colnames(pass_data_list_revision$NYNJH), fixed=TRUE)
colnames(qa_data_list_revision$Pensacola) = gsub('.','_',colnames(qa_data_list_revision$Pensacola), fixed=TRUE)
colnames(pass_data_list_revision$Pensacola) = gsub('.','_',colnames(pass_data_list_revision$Pensacola), fixed=TRUE)
colnames(qa_data_list_revision$PugetSound) = gsub('.','_',colnames(qa_data_list_revision$PugetSound), fixed=TRUE)
colnames(pass_data_list_revision$PugetSound) = gsub('.','_',colnames(pass_data_list_revision$PugetSound), fixed=TRUE)
colnames(qa_data_list_revision$SanFrancisco) = gsub('.','_',colnames(qa_data_list_revision$SanFrancisco), fixed=TRUE)
colnames(pass_data_list_revision$SanFrancisco) = gsub('.','_',colnames(pass_data_list_revision$SanFrancisco), fixed=TRUE)
colnames(qa_data_list_revision$Tampa) = gsub('.','_',colnames(qa_data_list_revision$Tampa), fixed=TRUE)
colnames(pass_data_list_revision$Tampa) = gsub('.','_',colnames(pass_data_list_revision$Tampa), fixed=TRUE)
colnames(qa_data_list_revision$Tillamook) = gsub('.','_',colnames(qa_data_list_revision$Tillamook), fixed=TRUE)
colnames(pass_data_list_revision$Tillamook) = gsub('.','_',colnames(pass_data_list_revision$Tillamook), fixed=TRUE)


#########################   Cleaning up Tillamook:     ########################

### Adding descriptive data:
## Create sensor columns in qa_data_list_revision:
# create sensor.SAMICO2 column if it doesn't already exist
if (!'sensor_SAMICO2' %in% colnames(qa_data_list_revision$Tillamook)) {
  qa_data_list_revision$Tillamook = qa_data_list_revision$Tillamook %>% 
    mutate(sensor_SAMICO2 = case_when(
      !is.na(pco2_uatm) ~ 1, 
      TRUE ~ 0
    ))
}
# create sensor.SeaFET column
if(!'sensor_SeaFET' %in% colnames(qa_data_list_revision$Tillamook)) {
  qa_data_list_revision$Tillamook$sensor_SeaFET = case_when(
    !is.na(qa_data_list_revision$Tillamook$temp_c_seafet) ~ 1,
    TRUE ~ 0
  ) 
}
# create sensor.SeapHOx column
if(!'sensor_SeapHOx' %in% colnames(qa_data_list_revision$Tillamook)) {
  qa_data_list_revision$Tillamook$sensor_SeapHOx = case_when(
    !is.na(qa_data_list_revision$Tillamook$temp_c_seaphox) ~ 1,
    TRUE ~ 0
  ) 
}
# create sensor.YSI column
if(!'sensor_YSI' %in% colnames(qa_data_list_revision$Tillamook)) {
  qa_data_list_revision$Tillamook$sensor_YSI = case_when(
    !is.na(qa_data_list_revision$Tillamook$temp_c_ysi) ~ 1,
    TRUE ~ 0
  ) 
}
### Create sensor columns in pass_data_list_revision:
if (!'sensor_SAMICO2' %in% colnames(pass_data_list_revision$Tillamook)) {
  pass_data_list_revision$Tillamook = pass_data_list_revision$Tillamook %>% 
    mutate(sensor_SAMICO2 = case_when(
      !is.na(pco2_uatm) ~ 1, 
      TRUE ~ 0
    ))
}
if(!'sensor_SeaFET' %in% colnames(pass_data_list_revision$Tillamook)) {
  pass_data_list_revision$Tillamook$sensor_SeaFET = case_when(
    !is.na(pass_data_list_revision$Tillamook$temp_c_seafet) ~ 1,
    TRUE ~ 0
  ) 
}
if(!'sensor_SeapHOx' %in% colnames(pass_data_list_revision$Tillamook)) {
  pass_data_list_revision$Tillamook$sensor_SeapHOx = case_when(
    !is.na(pass_data_list_revision$Tillamook$temp_c_seaphox) ~ 1,
    TRUE ~ 0
  ) 
}
if(!'sensor_YSI' %in% colnames(pass_data_list_revision$Tillamook)) {
  pass_data_list_revision$Tillamook$sensor_YSI = case_when(
    !is.na(pass_data_list_revision$Tillamook$temp_c_ysi) ~ 1,
    TRUE ~ 0
  ) 
}





##### Add in any columns to the Tillamook data that are now missing in qa_data_list_revision$Tillamook and pass_data_list_revision$Tillamook ######
# function to determine season
get_season = function(date) {
  month = month(date)
  if (month %in% c(12, 1, 2)) {
    return('DJF')
  } else if (month %in% c(3,4,5)) {
    return('MAM')
  } else if (month %in% c(6,7,8)) {
    return('JJA')
  } else if (month %in% c(9,10,11)) {
    return('SON')
  }
}

qa_data_list_revision$Tillamook = qa_data_list_revision$Tillamook %>% 
  mutate(
    NEP = 'Tillamook Bay',
    station_name = 'Garibaldi Dock',
    site_code = 'GD',
    season = sapply(datetime_utc,get_season),
    # Create overall measurement columns based on which, if any, sensors performed measurements:
    ph_T = case_when(
      !is.na(ph_ext_seaphox) ~ ph_ext_seaphox, # pH priority 1: SeapHOx
      !is.na(ph_ext_seafet) ~ ph_ext_seafet,  # pH priority 2: SeaFET
      !is.na(ph_ysi) & !is.na(sal_ppt_ysi) ~ ph_ysi + (sal_ppt_ysi * 0.016 / 35), # pH priority 3: YSI, converted to Total pH
      TRUE ~ NA_real_ # if all NA or no salinity to pair with YSI, then set to NA)
    ),
    sal_ppt = case_when(
      !is.na(sal_ppt_seaphox) ~ sal_ppt_seaphox,
      !is.na(sal_ppt_ysi) ~ sal_ppt_ysi,
      TRUE ~ NA_real_
    ),
    temp_c = case_when(
      !is.na(temp_c_seaphox) ~ temp_c_seaphox,
      !is.na(temp_c_ysi) ~ temp_c_ysi,
      TRUE ~ NA_real_
    ),
    depth_m = case_when(
      !is.na(depth_m_seaphox) ~ depth_m_seaphox,
      !is.na(depth_m_ysi) ~ depth_m_ysi,
      TRUE ~ NA_real_
    ),
    do_mgl = case_when(
      !is.na(do_mgl_seaphox) ~ do_mgl_seaphox,
      !is.na(do_mgl_ysi) ~ do_mgl_ysi,
      TRUE ~ NA_real_
    )
  )
pass_data_list_revision$Tillamook = pass_data_list_revision$Tillamook %>% 
  mutate(
    NEP = 'Tillamook Bay',
    station_name = 'Garibaldi Dock',
    site_code = 'GD',
    season = sapply(datetime_utc,get_season),
    # Create overall measurement columns based on which, if any, sensors performed measurements:
    ph_T = case_when(
      !is.na(ph_ext_seaphox) ~ ph_ext_seaphox, # pH priority 1: SeapHOx
      !is.na(ph_ext_seafet) ~ ph_ext_seafet,  # pH priority 2: SeaFET
      !is.na(ph_ysi) & !is.na(sal_ppt_ysi) ~ ph_ysi + (sal_ppt_ysi * 0.016 / 35), # pH priority 3: YSI, converted to Total pH
      TRUE ~ NA_real_ # if all NA or no salinity to pair with YSI, then set to NA)
    ),
    sal_ppt = case_when(
      !is.na(sal_ppt_seaphox) ~ sal_ppt_seaphox,
      !is.na(sal_ppt_ysi) ~ sal_ppt_ysi,
      TRUE ~ NA_real_
    ),
    temp_c = case_when(
      !is.na(temp_c_seaphox) ~ temp_c_seaphox,
      !is.na(temp_c_ysi) ~ temp_c_ysi,
      TRUE ~ NA_real_
    ),
    depth_m = case_when(
      !is.na(depth_m_seaphox) ~ depth_m_seaphox,
      !is.na(depth_m_ysi) ~ depth_m_ysi,
      TRUE ~ NA_real_
    ),
    do_mgl = case_when(
      !is.na(do_mgl_seaphox) ~ do_mgl_seaphox,
      !is.na(do_mgl_ysi) ~ do_mgl_ysi,
      TRUE ~ NA_real_
    )
  )


## save revised qa_data and pass_data #########################################
################################################################################
save(qa_data_list_revision,file="O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/qa_data_list_revision.Rdata")
save(pass_data_list_revision,file="O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/pass_data_list_revision.Rdata")

# save(qa_data_list_revision,file="C:/Users/spacella/OneDrive - Environmental Protection Agency (EPA)/NEP OA standards analysis/qa_data_list_revision.Rdata")
# save(pass_data_list_revision,file="C:/Users/spacella/OneDrive - Environmental Protection Agency (EPA)/NEP OA standards analysis/pass_data_list_revision.Rdata")
