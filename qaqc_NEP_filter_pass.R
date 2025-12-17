# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Apr 10, 2025
# Last updated: Apr 18, 2025

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    This R script performs the following: 
#                    -------------------------------------
#  1. Performs filters on the NEP data set 
#      - based on each NEP's specific QA/QC process, if any
#  2. Creates an output file containing the filtered dataset: pass_data_list.Rdata 
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#!! create initial 'pass_data_list' copy from 'qa_data_list'
#!! pass_data_list = qa_data_list
library(dplyr)
library(arsenal)

## Create cutoff date to filter data_list with, for 2015-present
cutoff_date = as.POSIXct('1999-12-31 23:59:59',format='%Y-%m-%d %H:%M:%S', tz='UTC')

#####  NEP Filters:  ####
# Barnegat
# re-make 'flags' column with test.Climatology columns removed:
# barnegat = barnegat %>% 
#   select(-starts_with('test.Climatology')) %>% 
#   mutate(flags = do.call(pmax, c(select(barnegat, starts_with('test.')), na.rm=TRUE)))   
pass_data_list$Barnegat = qa_data_list$Barnegat |>
  filter(datetime.utc > cutoff_date & flags == 1)


# Casco
# re-make 'flags' column with test.Climatology columns removed:      
# casco = casco %>% 
  # select(-starts_with('test.Climatology')) %>% 
  # mutate(flags = do.call(pmax, c(select(casco, starts_with('test.')), na.rm=TRUE)))   
pass_data_list$Cascobay = qa_data_list$Cascobay |> 
  filter(datetime.utc > cutoff_date & flags == 1)

# Coastal Bend
qa_data_list$Coastalbend = data_list$Coastalbend %>% 
  arrange(datetime.utc) %>% 
  mutate(flags = do.call(pmax, c(select(qa_data_list$Coastalbend, c('PH_FLAG','SAL_FLAG','CO2_FLAG','TEMP_FLAG')), na.rm=TRUE)))
pass_data_list$Coastalbend = qa_data_list$Coastalbend |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1)

# Delaware Inland Bays
pass_data_list$DelawareInland = qa_data_list$DelawareInland |> 
  filter(datetime.utc > cutoff_date & flags == 1)


# Indian River Lagoon
qa_data_list$IndianRiverLagoon = data_list$IndianRiverLagoon %>% 
  arrange(datetime.utc) %>% 
  filter(datetime.utc > cutoff_date) %>% 
  mutate(
    ph_flags = case_when(
      PH_FLAG == 'good' & ph.T > 5.5 & ph.T < 9.5 ~ 1, # incorporate our own gross range test here
      PH_FLAG == '' & ph.T > 5.5 & ph.T < 9.5 ~ 1,
      PH_FLAG == 'suspect' ~ 2,
      PH_FLAG == 'not evaluated' ~ 0,
      PH_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    ), 
    sal_flags = case_when(
      SAL_FLAG == 'good' | SAL_FLAG == '' ~ 1,
      SAL_FLAG == 'suspect' ~ 2,
      SAL_FLAG == 'not evaluated' ~ 0,
      SAL_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    ),
    temp_flags = case_when(
      TEMP_FLAG == 'good' | TEMP_FLAG == '' ~ 1,
      TEMP_FLAG == 'suspect' ~ 2,
      TEMP_FLAG == 'not evaluated' ~ 0,
      TEMP_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    ),
    do_flags = case_when(
      DO_FLAG == 'good' | DO_FLAG == '' ~ 1,
      DO_FLAG == 'suspect' ~ 2,
      DO_FLAG == 'not evaluated' ~ 0,
      DO_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    ),
    co2_flags = case_when(
      CO2_FLAG == 'good' | CO2_FLAG == '' ~ 1,
      CO2_FLAG == 'suspect' ~ 2,
      CO2_FLAG == 'not evaluated' ~ 0,
      CO2_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    ),
    pres_flags = case_when(
      PRESSURE_FLAG == 'good' | PRESSURE_FLAG == '' ~ 1,
      PRESSURE_FLAG == 'suspect' ~ 2,
      PRESSURE_FLAG == 'not evaluated' ~ 0,
      PRESSURE_FLAG == 'bad' ~ 3,
      TRUE ~ 2
    )
  ) %>% 
  # mutate(flags = do.call(pmax, c(select(qa_data_list$IndianRiverLagoon, c('ph_flags','temp_flags','sal_flags','do_flags','co2_flags','pres_flags')), na.rm=TRUE)))
  mutate(flags = pmax(ph_flags,temp_flags,sal_flags,do_flags,co2_flags,pres_flags, na.rm=TRUE))

pass_data_list$IndianRiverLagoon = qa_data_list$IndianRiverLagoon %>% 
  filter(datetime.utc > cutoff_date & flags == 1)

# ggplot()+
#   geom_point(data=qa_data_list$IndianRiverLagoon, aes(x=datetime.utc,y=ph.T), alpha=0.3, size=1, color='red')+
#   geom_point(data=pass_data_list$IndianRiverLagoon, aes(x=datetime.utc, y=ph.T),alpha=0.3, size=1, color='black')

# Long Island Sound
qa_data_list$LongIslandSound = data_list$LongIslandSound %>% 
  mutate(flags = 1)
pass_data_list$LongIslandSound = qa_data_list$LongIslandSound |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1) |> 
  filter(year(datetime.utc) != 2021) # bad year data 

# Mobile:
# ~~~ !!!   Do we want to include <4> flags as well? (4 = "before auto-QAQC", aka )
mobile_flags_pass = c('<0>','<0> (CAB)','<0> (CDF)','<0> (CND)','<0> (CRE)','<0> (CWE)','<0> [GIT] (CND)','<0> [GIT] (CRE)','<0> [GIT] (CWE)', '<0> (CDA)')
flag_columns = c('F_Temp','F_Sal','F_DO_mgl','F_Depth','F_pH')
qa_data_list$Mobile = data_list$Mobile |> 
  mutate(flags = 9) # Create flags column, set all to 9
# Create binary system of flags for Mobile: 1 = PASS, 0 = Other (suspect, fail, or otherwise not evaluated)
qa_data_list$Mobile[, flags := fifelse(
  rowSums(sapply(.SD, function(x) x %in% mobile_flags_pass)) == length(flag_columns),
  1,0),
  .SDcols = flag_columns]
# pass_data_list$Mobile = df
pass_data_list$Mobile = qa_data_list$Mobile |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1)

# Morro Bay:
morro_QC_cols = c('PH_ext_QC','DO_QC','PRES_QC','SAL_QC','TEMP_QC')
qa_data_list$Morro = data_list$Morro %>% 
  arrange(datetime.utc) %>% 
  mutate(across(all_of(morro_QC_cols), ~ case_when(
    . == 4 ~ 3, # convert 4 flag into 3 (FAIL)
    . == 3 ~ 2, # convert 3 flag into 2 (SUSPECT)
    . == 1 ~ 1, # keep PASS as is
    . %in% c(2,9) ~ 0, # convert 2,9 flags into 0 (NOT-EVALUATED)
    TRUE ~ NA_real_
  ), .names = 'adj_{.col}')) %>% 
  # compute maximum flag
  rowwise() %>% 
  mutate(flags = max(c_across(starts_with('adj_')), na.rm=TRUE)) %>% 
  ungroup() %>% 
  # remove temporary adj_ columns
  select(-starts_with('adj_'))
  # mutate(flags_QC = do.call(pmax, c(select(qa_data_list$Morro, c('PH_ext_QC','DO_QC','PRES_QC','SAL_QC','TEMP_QC')), na.rm=TRUE))) 

  #filter(datetime.utc > cutoff_date & PH_ext_QC == 1 & DO_QC == 1 & PRES_QC == 1 & SAL_QC == 1 & TEMP_QC == 1)

pass_data_list$Morro = qa_data_list$Morro |> 
  arrange(datetime.utc) |> 
  # filter(site.code == 'BM1') |> # (No longer filtering out site BS1)
  filter(datetime.utc > cutoff_date & flags == 1)

# Narragansett Bay
qa_data_list$Narrgansett = data_list$Narrgansett %>% 
  mutate(flags = 1)
pass_data_list$Narrgansett = qa_data_list$Narrgansett |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1)

# New York-New Jersey Harbor
qa_data_list$NYNJH = data_list$NYNJH %>% 
  arrange(datetime.utc) %>% 
  mutate(
    ph_flags = case_when(
      flag.ph == 'A' | flag.ph == 'C' ~ 1,
      flag.ph == 'S' | flag.ph == 'P' ~ 2,
      flag.ph == 'R' ~ 3,
      TRUE ~ 0
    ),
    do_flags = case_when(
      flag.do.mgl == 'A' | flag.do.mgl == 'C' ~ 1,
      flag.do.mgl == 'S' | flag.do.mgl == 'S' ~ 2,
      flag.do.mgl == 'R' ~ 3,
      TRUE ~ 0
    ),
    sal_flags = case_when(
      flag.sal.ppt == 'A' | flag.sal.ppt == 'C' ~ 1,
      flag.sal.ppt == 'S' | flag.sal.ppt == 'P' ~ 2,
      flag.sal.ppt == 'R' ~ 3,
      TRUE ~ 0
    ),
    temp_flags = case_when(
      flag.temp.c == 'A' | flag.temp.c == 'C' ~ 1,
      flag.temp.c == 'S' | flag.temp.c == 'S' ~ 2,
      flag.temp.c == 'R' ~ 3,
      TRUE ~ 0
    )
  ) %>% 
  mutate(flags = pmax(ph_flags,temp_flags,sal_flags,do_flags, na.rm=TRUE)) %>%
  mutate(flags = case_when(
    flags == 1 & sal.ppt >= 40 ~ 2,
    TRUE ~ flags
  ))

pass_data_list$NYNJH = qa_data_list$NYNJH %>% 
  filter(datetime.utc > cutoff_date & flags == 1) 


# Pensacola
# re-make 'flags' column with test.Climatology columns removed:      
# pensacola = pensacola %>% 
  # select(-starts_with('test.Climatology')) %>% 
  # mutate(flags = do.call(pmax, c(select(pensacola, starts_with('test.')), na.rm=TRUE)))   
pass_data_list$Pensacola = qa_data_list$Pensacola |> 
  filter(datetime.utc > cutoff_date & flags == 1)

#### <--- Catch up here
# QA data list was saved locally but not to O:drive 6.13.25
# Puget Sound
data_list$PugetSound = data_list$PugetSound %>% 
  arrange(datetime.utc) %>% 
  mutate(ph.T = ph.T_lueker)
qa_data_list$PugetSound = data_list$PugetSound %>% 
  mutate(flags = 1)
  
pass_data_list$PugetSound = qa_data_list$PugetSound |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1)

# San Francisco Bay
sf_QC_cols = c('ph.tot.qc','do.mgl.qc','sal.ppt.qc','temp.c.qc')
qa_data_list$SanFrancisco = data_list$SanFrancisco %>% 
  arrange(datetime.utc) %>% 
  mutate(across(all_of(sf_QC_cols),
        .fns = function(x) case_when(
          x == 4 ~ 3,
          x == 3 ~ 2,
          x == 1 ~ 1,
          x %in% c(2,9) ~ 0,
          is.na(x) ~ NA_real_,
          TRUE ~ NA_real_
        ), .names = 'adj_{.col}')) %>% 
  
  # mutate(across(all_of(sf_QC_cols), case_when(
  #   . == 4 ~ 3, # FAIL
  #   . == 3 ~ 2, # SUSPECT
  #   . == 1 ~ 1, # PASS
  #   . == 2 ~ 0, # NOT EVALUATED
  #   is.na(.) ~ NA_real_, # explicitly handle NA values
  #   TRUE ~ NA_real_
  # ), .names = 'adj_{.col}')) %>% # create temporary adj_ columns
  # compute highest flag
  rowwise() %>% 
  mutate(flags = max(c_across(starts_with('adj_')), na.rm=TRUE)) %>% 
  ungroup() %>% 
  # remove temporary adj_ columns
  select(-starts_with('adj_'))

pass_data_list$SanFrancisco = qa_data_list$SanFrancisco |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & flags == 1)
         # & ph.tot.qc == 1
         # & do.mgl.qc == 1 
         # # & PRES_QC == 1
         # & sal.ppt.qc == 1
         # & temp.c.qc == 1)


# <-----
# Tampa Bay
# flags:
# 1 = correct
# 2 = probably correct
# 3 = probably bad
# 4 = bad 
# 5 = modified as a result of QC
# 9 = value missing
  
tampa_QC_cols = c('QF_pHT','QF_SALINITY','QF_CO2','QF_OXYGEN','temp.c.qc')
qa_data_list$Tampa = data_list$Tampa %>% 
  arrange(datetime.utc) %>% 
  mutate(across(all_of(tampa_QC_cols), 
    .fns = function(x) case_when(
      x %in% c(1, 2, 15, 25) ~ 1, # Pass flags
      x %in% c(3, 35) ~ 2,
      x %in% c(4, 45) ~ 3,
      x == 9 ~ 0,
      is.na(x) ~ NA_real_,
      TRUE ~ NA_real_
    ), .names = 'adj_{.col}')) %>%  # put new QC columns into temporary adj_X columns
  # compute maximum flag
  rowwise() %>% 
  mutate(flags = max(c_across(starts_with('adj_')), na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-starts_with('adj_')) # remove temporary adj_X columns

pass_data_list$Tampa = qa_data_list$Tampa |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date, flags == 1) # |>
  # filter(QF_pHT == 1 | QF_pHT == 15) |> 
  # filter(QF_SALINITY == 1 | QF_SALINITY == 15) |> 
  # filter(QF_CO2 == 1 | QF_CO2 == 15) |> 
  # filter(QF_OXYGEN == 1 | QF_OXYGEN == 15) |> 
  # filter(temp.c.qc == 1 | temp.c.qc == 15)

# Tillamook Bay
# Check if data_list and qa_data_list versions of Tillamook are the same:

qa_data_list$Tillamook = data_list$Tillamook %>% 
  arrange(datetime.utc) %>% 
  rowwise() %>% 
  mutate(flags = max(c_across(starts_with('flags_')), na.rm=TRUE)) %>% 
  ungroup() 

pass_data_list$Tillamook = qa_data_list$Tillamook |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date) |> 
  filter(flags_seafet == 1 | is.na(flags_seafet)) |> 
  filter(flags_seaphox == 1 | is.na(flags_seaphox)) |> 
  filter(flags_ysi == 1 | is.na(flags_ysi)) |> 
  filter(flags_samico2 == 1 | is.na(flags_samico2))


#######################################################################
# Renaming 'Gulf of Mexico' to simply 'Gulf'
data_list$Coastalbend = data_list$Coastalbend %>% 
  mutate(region = 'Gulf')
data_list$Mobile = data_list$Mobile %>% 
  mutate(region = 'Gulf')
data_list$Pensacola = data_list$Pensacola %>% 
  mutate(region = 'Gulf')
data_list$Tampa = data_list$Tampa %>% 
  mutate(region = 'Gulf')
qa_data_list$Coastalbend = qa_data_list$Coastalbend %>% 
  mutate(region = 'Gulf')
qa_data_list$Mobile = qa_data_list$Mobile %>% 
  mutate(region = 'Gulf')
qa_data_list$Pensacola = qa_data_list$Pensacola %>% 
  mutate(region = 'Gulf')
qa_data_list$Tampa = qa_data_list$Tampa %>% 
  mutate(region = 'Gulf')
pass_data_list$Coastalbend = pass_data_list$Coastalbend %>% 
  mutate(region = 'Gulf')
pass_data_list$Mobile = pass_data_list$Mobile %>% 
  mutate(region = 'Gulf')
pass_data_list$Pensacola = pass_data_list$Pensacola %>% 
  mutate(region = 'Gulf')
pass_data_list$Tampa = pass_data_list$Tampa %>% 
  mutate(region = 'Gulf')

