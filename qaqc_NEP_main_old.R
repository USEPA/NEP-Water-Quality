# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Apr 10, 2025

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    This R script performs the following: 
#                    -------------------------------------
#  1. Loads in necessary packages
#  2. Defines necessary functions for QA'ing data
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#### Step 1. Load in necessary packages
library(tidyverse)
library(dplyr)
library(slider)
library(purrr)
library(fuzzyjoin)
library(zoo)
library(data.table)

# Step 2. Define necessary Functions:  ###################################
# Preliminary functions: ####
# function to determine season:
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
# Function to create season column in dataset (uses 'get_season()' function above):
make_season_column = function(dataset) {
  setDT(dataset)
  dataset[, season := sapply(datetime.utc,get_season)]
  setDF(dataset)
  return(dataset)
}
##### QA TEST FUNCTIONS: ####
# GROSS RANGE TEST #
gross_range_test = function(site_data, vars_to_test, user_thresholds, sensor_thresholds) {
  # Tests NEP data for values outside of expected range
  #  0 - Test not ran
  #  1 - Pass
  #  2 - Suspect - exceeds user thresholds (expected value)
  #  3 - Fail - exceeds sensor thresholds (theoretical possible range)
  # - - - - - - - - - - - - - - - - - -
  # Initialize test columns with 0 (test not ran)
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.GrossRange_{.col}')) # 0 = test not ran
  # Apply test logic
  data = data |> 
    mutate(across(all_of(vars_to_test), ~case_when(
      .x < sensor_thresholds[[cur_column()]]$min | .x > sensor_thresholds[[cur_column()]]$max ~ 3, # FAIL
      .x < user_thresholds[[cur_column()]]$min | .x > user_thresholds[[cur_column()]]$max ~ 2, # SUSPECT
      TRUE ~ 1 # PASS
    ), .names = 'test.GrossRange_{.col}')) # fill test.GrossRange_var column with test results
  # Create overall test.GrossRange column
  data = data |>
    mutate(test.GrossRange = do.call(pmax, c(select(data, starts_with('test.GrossRange_')), na.rm=TRUE)))
    # mutate(test.GrossRange = prioritize_values_vectorized(site_data,'test.GrossRange_'))
  return(data)
}
# SPIKE TEST #
spike_test = function(site_data, vars_to_test, spike_thresholds) {
  # Tests NEP data for a value spike relative to previous measurement
  #  0 - Test not ran
  #  0.5 - Insufficient data - no previous value to compare current value against
  #  1 - Pass
  #  2 - Suspect - exceeds low threshold value (spike_low_var)
  #  3 - Fail - exceeds high threshold value (spike_high_var)
  # - - - - - - - - - - - - - - - - - -
  # initialize test columns with 0 (test not ran)
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Spike_{.col}'))
  # Apply test logic
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ case_when(
      is.na(.x) | is.na(lag(.x)) ~ 0.5, # Insufficient data
      abs(.x - lag(.x)) > spike_thresholds[[cur_column()]]$high ~ 3, # FAIL
      abs(.x - lag(.x)) > spike_thresholds[[cur_column()]]$low ~ 2, # SUSPECT
      TRUE ~ 1 # PASS
    ), .names = 'test.Spike_{.col}'))
  # Create overall test.Spike column
  data = data |> 
    mutate(test.Spike = do.call(pmax, c(select(data, starts_with('test.Spike_')), na.rm=TRUE)))
    # mutate(test.Spike = prioritize_values_vectorized(site_data, 'test.Spike_'))
  return(data)
}
# FLATLINE TEST #
get_flatline_lengths = function(x) {
  # function which looks at a list of values and returns a list of how many times each value has been repeated
  # e.g. if x = c(1,2,2,5,5,5) this would return: c(1,1,2,1,2,3)
  n = length(x)
  flatline_lengths = rep(1,n) # create a list of 1's, equal in length to the list 'x'
  for (i in 2:n) {
    if (!is.na(x[i]) && !is.na(x[i-1]) && x[i] == x[i-1]) { # if the value is equal to the previous value: add 1 to the i'th flatline tracker
      flatline_lengths[i] = flatline_lengths[i-1]+1
    }
  }
  return(flatline_lengths)
}
flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail) {
  # Tests NEP data for consecutive unchanging values
  #  0 - Test not ran
  #  0.5 - Insufficient data (not enough previous entries)
  #  1 - Pass
  #  2 - Suspect - 3 or 4 equal repeated values
  #  3 - Fail - 5+ equal repeated values 
  # - - - - - - - - - - - - - - - - - -
  SUS_NUM = num_flatline_sus 
  FAIL_NUM = num_flatline_fail 
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Flatline_{.col}'))
  # Apply test logic:
  for (var in vars_to_test) {
    if (tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing flatline for:',var,'at',Sys.time()))
    }
    col = data[[var]]
    flatline_lengths = get_flatline_lengths(col)
    flag_col = case_when(
      row_number(data) < 5 ~ 0.5, # INSUFFICIENT DATA
      is.na(col) ~ 0,                   # TEST NOT RAN
      flatline_lengths >= FAIL_NUM ~ 3, # FAIL
      flatline_lengths >= SUS_NUM ~ 2, # SUSPECT
      TRUE ~ 1                         # PASS
    )
    data[[paste0('test.Flatline_',var)]] = flag_col
  }
  # create overall test.Flatline column
  data = data |> 
    mutate(test.Flatline = do.call(pmax, c(select(data, starts_with('test.Flatline_')), na.rm=TRUE)))
  return(data)
}
# CLIMATOLOGY TEST #
climatology_test = function(site_data, vars_to_test, seasonal_thresholds) {
  # Tests NEP data for seasonal-specific threshold exceedence
  #  0 - Test not ran
  #  1 - Pass
  #  2 - Suspect - exceeded threshold
  # - - - - - - - - - - - - - - - - - -
  # Debug
  if (!'season' %in% names(site_data)) {
    stop('Error: there is no season column in the dataset')
  }
  # initialize test columns with 0 
  site_data = site_data |>
    mutate(across(all_of(vars_to_test), ~ 0, .names='test.Climatology_{.col}'))
  # create and fill seasonal min/max columns:
  for (var in vars_to_test) {
    if (tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing climatology for:',var,'at',Sys.time()))
    }
    min_col = paste0(var, '_season_min')
    max_col = paste0(var, '_season_max')
    
    site_data[[min_col]] = sapply(site_data$season, function(s) {
      if (!is.null(seasonal_thresholds[[paste0(var, '_min')]][[s]])) {
        return(seasonal_thresholds[[paste0(var, '_min')]][[s]])
      } else {
        return(NA_real_)
      }
    })
    site_data[[max_col]] = sapply(site_data$season, function(s) {
      if (!is.null(seasonal_thresholds[[paste0(var, '_max')]][[s]])) {
        return(seasonal_thresholds[[paste0(var, '_max')]][[s]])
      } else {
        return(NA_real_)
      }
    })
  }
  # Apply test logic:
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ {
      min_threshold = get(paste0(cur_column(), '_season_min'))
      max_threshold = get(paste0(cur_column(), '_season_max'))
      case_when(
        is.na(.x) | is.na(season) ~ 0, # test not ran if NAs in data or season
        is.na(min_threshold) | is.na(max_threshold) ~ 0, # test not ran if no thresholds exist
        .x < min_threshold | .x > max_threshold ~ 2, # Suspect if exceed thresholds
        TRUE ~ 1 # Pass
      )
    }, .names = 'test.Climatology_{.col}'))
  # Compute overall test.Climatology as max of all test.Climatology_* columns:
  data = data |> 
    mutate(test.Climatology = do.call(pmax, c(select(data, starts_with('test.Climatology_')), na.rm=TRUE)))
  # Remove _season_min and _season_max columns before returning dataset:
  finished_data = data |> 
    select(-ends_with('_season_min'),-ends_with('_season_max'))
  return(finished_data)
}
# RATE OF CHANGE TEST: 3 functions #
# Rate of Change Function 1: interpolating data (into new data frame) to account for any missing timestamps - empty data is NA
interpolate_data = function(data, vars_to_test, time_interval=sample_interval) {
  # ensure datetime.utc is in POSIXct format
  sorted_data = data |> 
    drop_na(datetime.utc) # remove NA datetime values
  data_length = length(sorted_data$datetime.utc) # length of data
  time_min = sorted_data$datetime.utc[1]
  time_max = sorted_data$datetime.utc[data_length]
  all_times = seq(time_min, time_max, by=(time_interval*60)) # create adjusted time-series
  data_interp = data.frame(list(datetime.utc = all_times)) # put interpolated times into a data.frame
  
  # loop through each var and add it to the interpolated data-frame
  for (var in vars_to_test) {
    testdata = sorted_data |> 
      select(datetime.utc, paste0(var))
    data_interp = data_interp |> 
      left_join(testdata, join_by('datetime.utc'))
  }
  return(data_interp)
}

# Rate of Change Function 2: calculate rolling standard deviation and add to main data frame
calc_rolling_sd = function(data_interp, vars_to_test, time_interval=sample_interval, min_non_na = 20) {
  sampling_window = (60/time_interval)*24 # num of rows per 24-hours
  for (var in vars_to_test) {
    if (var %in% names(data_interp)) {
      sd_col_name = paste0('sd_',var)
      data_interp = data_interp |> 
        mutate(
          !!sd_col_name := slide_dbl(
            .x = data_interp[[var]],
            .f = ~ifelse(sum(!is.na(.x)) >= min_non_na, sd(.x, na.rm=TRUE), NA_real_),
            .before = sampling_window, # look back this many rows
            .complete = TRUE # only compute once >= sampling_window rows available
          )
        )
    } else {cat('Missing variable from interpolated data: ',var,'\n')}
  }
  return(data_interp)
}

# Rate of Change Function 3: perform rate-of-change test on primary data based on SD values in interpolated data
rate_change_test = function(data, data_interp, vars_to_test, num_sd_for_rate_change = 3) {
  # Tests NEP data for a value spike relative to standard deviation of previous time period (24 hours)
  #  0 - Test not ran
  #  0.5 - Insufficient data to perform test
  #  1 - Pass
  #  2 - Suspect - spike exceeds standard deviation of previous window
  # - - - - - - - - - - - - - - - - - -
  # initialize test columns with 0
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.RateChange_{.col}'))
  for (var in vars_to_test) {
    if (tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing rate change for:',var,'at',Sys.time()))
    }
    # dynamically match rolling SD values from data_interp:
    matched_sd_values = data_interp[[paste0('sd_',var)]][match(data$datetime.utc,data_interp$datetime.utc)]
    # apply rate of change test:
    data = data |> 
      mutate(!!paste0('test.RateChange_',var) := case_when(
        is.na(get(var)) | is.na(lag(get(var))) ~ 0.5, # Insufficient data
        is.na(matched_sd_values) ~ 0, # Test not run
        abs(get(var) - lag(get(var))) > num_sd_for_rate_change*matched_sd_values ~ 2, # Suspect
        TRUE ~ 1 # Pass
      ))
  }
  # Create overall test.RateChange column
  data = data |> 
    mutate(test.RateChange = do.call(pmax, c(select(data, starts_with('test.RateChange_')), na.rm=TRUE)))
  return(data)
}
# ATTENUATED SIGNAL TEST #
attenuated_signal_test = function(data, data_interp, vars_to_test, attenuated_signal_thresholds, time_window, time_interval = sample_interval) {
  # Tests NEP data for a near-flatline (change relative to a designated suspect and fail threshold ('attenuated_signal_thresholds'))
  # time_window: how far back (in hours, default = 24 hours) you want to assess the min/max range of values for the test
  # time_interval: the MINIMUM interval (in minutes, default = 15 min) between measurements in the dataset
  #    --> NOTE: Even if 90% of the dataset is a 15-min interval, if 10% is a 5-min interval, choose 5 minutes. This function will
  #              interpolate the data time-series and remove any rows that didn't take measurements. This ensures full data inclusion
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Test flags:
  #  0 - Test not ran
  #  1 - Pass - exceeds both suspect and fail thresholds for change
  #  2 - Suspect - exceeds fail threshold but does not exceed suspect threshold
  #  3 - Fail - does not exceed fail threshold
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # define number of rows to assess min and max values across
  #num_rows = (test_time * 60 / time_interval)
  num_rows = time_window*60/time_interval # rows to sample backwards across for test
  # define safe_min and safe_max functions to calculate min and max values across num_rows without creating -Inf 
  safe_max = function(x) if (all(is.na(x))) NA else max(x, na.rm=TRUE)
  safe_min = function(x) if (all(is.na(x))) NA else min(x, na.rm=TRUE)
  # initialize test columns with 0 (test not run)
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.AttenuatedSignal_{.col}'))
  for (var in vars_to_test) {
    fail_threshold = attenuated_signal_thresholds[[var]]$min_fail
    suspect_threshold = attenuated_signal_thresholds[[var]]$min_sus
    if (tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing attenuated signal for:',var,'at',Sys.time()))
    }
    # debug: ensure there are valid values
    if (all(is.na(data_interp[[var]]))) {
      warning(paste('All values of',var,'are NA in data_interp. Skipping.'))
      next # skip this variable if all values are NA
    }
    
    # compute rolling max-min difference over time_window:
    data_interp = data_interp |> 
      mutate(!!paste0(var,'_max') := rollapply(get(var), width=num_rows, FUN=safe_max, fill=NA, align='right'),
             !!paste0(var,'_min') := rollapply(get(var), width=num_rows, FUN=safe_min, fill=NA, align='right'))
    # match min and max values from data_interp to data based on datetime.utc
    max_col_values = data_interp[[paste0(var,'_max')]][match(data$datetime.utc, data_interp$datetime.utc)]
    min_col_values = data_interp[[paste0(var,'_min')]][match(data$datetime.utc, data_interp$datetime.utc)]
    # compute attenuated signal test
    data = data |> 
      mutate(!!paste0('test.AttenuatedSignal_',var) := case_when(
        is.na(get(var)) ~ 0, # test not run
        (max_col_values - min_col_values) < fail_threshold ~ 3, # Fail
        (max_col_values - min_col_values) < suspect_threshold ~ 2, # Suspect
        TRUE ~ 1 # Pass
      ))
  }
  # Compute overall test.AttenuatedSignal column
  data = data |> 
    mutate(test.AttenuatedSignal = do.call(pmax, c(select(data, starts_with('test.AttenuatedSignal_')), na.rm=TRUE)))
  return(data)
}


# _________________________________________________________ #
### QAQC Function (calls individual test functions)####
# _________________________________________________________ #
qaqc_nep = function(data, columns_to_qa, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_window,
                    time_interval, attenuated_signal_thresholds, num_sd_for_rate_change, num_flatline_sus, num_flatline_fail) {
# METADATA: ####
# Applies QARTOD testing across a single data-frame, assuming all data within the data-frame corresponds to a single NEP
# Assumed column names:
#    site.code - the code signature of that specific site within the NEP
#    datetime.utc - the date & time format used for time-sensitive testing
#    ph - pH on the Total Scale
#    temp.c - temperature in Celsius
#    sal.ppt - salinity in PSU (or parts-per-thousand)
#    do.mgl - dissolved oxygen in milligrams/liter
#    co2.ppm - dissolved CO2 in parts-per-million
  
# Flags:
#    0 = Test not yet performed (default)
#    0.5 = Test not performed (insufficient data)
#    1 = Pass
#    2 = Suspect
#    3 = Fail
# ___________________________________________________
  if (is.character(data$datetime.utc)) {
    data$datetime.utc = as.POSIXct(data$datetime.utc, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  }
  data = data %>% 
    arrange(datetime.utc)
  site_list = data |> 
    group_split(site.code)
  results_list = list()
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]
    site_code = unique(site_data$site.code)
    cat('Processing site:',site_code,'\n')
    ### Run QA tests ###
    # gross range:
    site_data = gross_range_test(site_data, vars_to_test, user_thresholds, sensor_thresholds)
    # spike:
    site_data = spike_test(site_data, vars_to_test, spike_thresholds)
    # flat line:
    site_data = flatline_test(site_data, vars_to_test, num_flatline_sus, num_flatline_fail)
    # climatology:
    site_data = climatology_test(site_data, vars_to_test, seasonal_thresholds)
    # rate of change:
    site_data_interp = interpolate_data(site_data, vars_to_test, time_interval) # interpolate missing timestamps and values per site
    data_interp = calc_rolling_sd(site_data_interp, vars_to_test,time_interval, min_non_na = 20)
    site_data = rate_change_test(site_data, data_interp, vars_to_test, num_sd_for_rate_change)
    # attenuated signal:
    site_data = attenuated_signal_test(site_data, data_interp, vars_to_test, attenuated_signal_thresholds, time_window, time_interval)
    
    results_list[[i]] = site_data
  }
  return(bind_rows(results_list))
}


#### ADDITIONAL FUNCTIONS: ####
#### Converting NBS-scale pH to Total-scale pH: ####
convert_ph_NBS_to_Total = function(data) {
  if (!'sensor.YSI' %in% names(data)) {
    data = data |> 
      mutate(ph.T = ph)
  } else {
    data = data |> 
      mutate(ph.T = case_when(
        sensor.YSI == 1 & !is.na(ph) & !is.na(sal.ppt) ~ ph + (sal.ppt*0.016/35),
        TRUE ~ ph #
      ))
  }
  return(data) 
}

