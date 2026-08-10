# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Feb 23, 2026

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
library(rlang)
library(purrr)
library(fuzzyjoin)
library(zoo)
library(data.table)
library(lubridate)
library(scales)
library(broom)
library(seacarb)
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

# Flatline Test 3.2 - Faster version than 3.1. [WORKS - fast!]
flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
  # start_time = Sys.time()
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for this version.")
  }
  
  library(data.table)
  
  # Convert to data.table and drop rows with missing timestamps
  data = as.data.table(site_data)
  data = data[!is.na(datetime.utc)]
  setorder(data, datetime.utc)
  
  sus_hours  = num_flatline_sus
  fail_hours = num_flatline_fail
  
  n = nrow(data)
  
  for (var in vars_to_test) {
    current_tol = flatline_thresholds[[var]]
    
    if (exists("progress_print_option") && tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing flatline for:', var, '| Threshold:', current_tol, '| at', Sys.time()))
    }
    
    x = data[[var]]
    t = data$datetime.utc
    
    flag_vec = rep(1, n)
    flag_vec[is.na(x)] = 0
    flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
    
    sus_end = logical(n)
    fail_end = logical(n)
    
    sus_start_idx = 1L
    fail_start_idx = 1L
    
    for (i in seq_len(n)) {
      while (sus_start_idx < i &&
             difftime(t[i], t[sus_start_idx], units = "hours") > sus_hours) {
        sus_start_idx = sus_start_idx + 1L
      }
      
      while (fail_start_idx < i &&
             difftime(t[i], t[fail_start_idx], units = "hours") > fail_hours) {
        fail_start_idx = fail_start_idx + 1L
      }
      
      # Suspect window
      vals = x[sus_start_idx:i]
      if (sum(!is.na(vals)) > 1) {
        rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
        if (!is.na(rng) && rng <= current_tol) {
          sus_end[i] = TRUE
        }
      }
      
      # Fail window
      vals = x[fail_start_idx:i]
      if (sum(!is.na(vals)) > 1) {
        rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
        if (!is.na(rng) && rng <= current_tol) {
          fail_end[i] = TRUE
        }
      }
    }
    
    # Paint suspect windows
    for (i in which(sus_end)) {
      start_i = i
      while (start_i > 1 &&
             difftime(t[i], t[start_i - 1], units = "hours") <= sus_hours) {
        start_i = start_i - 1L
      }
      flag_vec[start_i:i] = pmax(flag_vec[start_i:i], 2)
    }
    
    # Paint fail windows
    for (i in which(fail_end)) {
      start_i = i
      while (start_i > 1 &&
             difftime(t[i], t[start_i - 1], units = "hours") <= fail_hours) {
        start_i = start_i - 1L
      }
      flag_vec[start_i:i] = pmax(flag_vec[start_i:i], 3)
    }
    
    flag_vec[is.na(x)] = 0
    flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
    
    data[[paste0("test.Flatline_", var)]] = flag_vec
  }
  
  data[, test.Flatline := do.call(pmax, c(.SD, na.rm = TRUE)),
       .SDcols = patterns("^test\\.Flatline_")]
  
  # end_time = Sys.time()
  # print(end_time-start_time)
  return(as.data.frame(data))
}
# Flatline Test 3.1 - faster version of 3.0 by using data.table (WORKS, but still slow)
# flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
#   if (!requireNamespace("data.table", quietly = TRUE)) {
#     stop("Package 'data.table' is required for this version.")
#   }
#   
#   library(data.table)
#   
#   # Convert to data.table and sort by time, keeping NA timestamps at the end
#   data = as.data.table(site_data)
#   setorder(data, datetime.utc)
#   
#   sus_hours  = num_flatline_sus
#   fail_hours = num_flatline_fail
#   
#   n = nrow(data)
#   
#   # Identify rows with valid timestamps
#   valid_time_idx = which(!is.na(data$datetime.utc))
#   times_valid = data$datetime.utc[valid_time_idx]
#   
#   # Helper: locate the first row index within the valid-time subset
#   window_start_idx = function(times, end_time, hours_back) {
#     start_time = end_time - as.difftime(hours_back, units = "hours")
#     findInterval(start_time, times) + 1L
#   }
#   
#   for (var in vars_to_test) {
#     current_tol = flatline_thresholds[[var]]
#     
#     if (exists("progress_print_option") && tolower(progress_print_option) %in% c('y','yes')) {
#       print(paste('Processing flatline for:', var, '| Threshold:', current_tol, '| at', Sys.time()))
#     }
#     
#     x = data[[var]]
#     
#     flag_vec = rep(1, n)
#     flag_vec[is.na(x)] = 0
#     flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
#     
#     sus_end = logical(n)
#     fail_end = logical(n)
#     
#     # Work only on rows with valid timestamps
#     for (j in seq_along(valid_time_idx)) {
#       i = valid_time_idx[j]
#       
#       # Suspect window
#       s_local = window_start_idx(times_valid, data$datetime.utc[i], sus_hours)
#       
#       if (s_local <= j) {
#         idx_local = s_local:j
#         vals = x[valid_time_idx[idx_local]]
#         
#         if (sum(!is.na(vals)) > 1) {
#           rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
#           if (!is.na(rng) && rng <= current_tol) {
#             sus_end[i] = TRUE
#           }
#         }
#       }
#       
#       # Fail window
#       f_local = window_start_idx(times_valid, data$datetime.utc[i], fail_hours)
#       
#       if (f_local <= j) {
#         idx_local = f_local:j
#         vals = x[valid_time_idx[idx_local]]
#         
#         if (sum(!is.na(vals)) > 1) {
#           rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
#           if (!is.na(rng) && rng <= current_tol) {
#             fail_end[i] = TRUE
#           }
#         }
#       }
#     }
#     
#     # Paint suspect windows
#     idx_sus = which(sus_end)
#     if (length(idx_sus) > 0) {
#       for (i in idx_sus) {
#         sus_start = data$datetime.utc[i] - as.difftime(sus_hours, units = "hours")
#         idx = which(!is.na(data$datetime.utc) & data$datetime.utc >= sus_start & data$datetime.utc <= data$datetime.utc[i])
#         flag_vec[idx] = pmax(flag_vec[idx], 2)
#       }
#     }
#     
#     # Paint fail windows
#     idx_fail = which(fail_end)
#     if (length(idx_fail) > 0) {
#       for (i in idx_fail) {
#         fail_start = data$datetime.utc[i] - as.difftime(fail_hours, units = "hours")
#         idx = which(!is.na(data$datetime.utc) & data$datetime.utc >= fail_start & data$datetime.utc <= data$datetime.utc[i])
#         flag_vec[idx] = pmax(flag_vec[idx], 3)
#       }
#     }
#     
#     # Restore missing-value rule and first-4-row rule
#     flag_vec[is.na(x)] = 0
#     flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
#     
#     data[[paste0("test.Flatline_", var)]] = flag_vec
#   }
#   
#   data[, test.Flatline := do.call(pmax, c(.SD, na.rm = TRUE)),
#        .SDcols = patterns("^test\\.Flatline_")]
#   
#   return(as.data.frame(data))
# }
# Flatline Test 3.0: [WORKS] Fixing the previous 2.0 version which was incorrectly flagging data that should have passed: ####
# Time-based flatline test using datetime.utc
#  --> now looks back on the previous time windows designated by num_flatline_sus and num_flatline_fail
# flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
#   # Assumes site_data contains one site only
#   # Sort data in time order before applying rolling windows
#   data = site_data %>% arrange(datetime.utc)
#   
#   # Treat these inputs as time windows in hours
#   sus_hours  = num_flatline_sus
#   fail_hours = num_flatline_fail
#   
#   for (var in vars_to_test) {
#     current_tol = flatline_thresholds[[var]]
#     
#     if (exists("progress_print_option") && tolower(progress_print_option) %in% c('y','yes')) {
#       print(paste('Processing flatline for:', var, '| Threshold:', current_tol, '| at', Sys.time()))
#     }
#     
#     x = data[[var]] # Current variable values for the site
#     t = data$datetime.utc # Timestamp vector for the site
#     n = nrow(data) # Number of rows in this site's data
#     
#     flag_vec = rep(1, n) # Initialize output flag vector. Default is 1 (pass) for all rows.
#     flag_vec[is.na(x)] = 0 # Mark missing observations as 0 (test not applicable / not run on missing data)
#     
#     # Apply insufficient-data flag (0.5) to the first 4 rows of the site
#     # This preserves your existing logic for early rows
#     flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
#     
#     # Loop through each row and evaluate rolling windows ending at that row
#     for (i in seq_len(n)) {
#       
#       # Start time for the suspect window:
#       # look back sus_hours from the current timestamp
#       sus_start = t[i] - as.difftime(sus_hours, units = "hours")
#       
#       # Row indices that fall inside the suspect time window
#       # from sus_start up to the current time t[i]
#       sus_idx = which(t >= sus_start & t <= t[i])
#       
#       # Only evaluate if the window contains more than one row
#       if (length(sus_idx) > 1) {
#         # Values inside the suspect window
#         vals = x[sus_idx]
#         
#         # Skip if all values are missing
#         if (!all(is.na(vals))) {
#           # Range across the suspect window:
#           # max value minus min value
#           rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
#           
#           # If range does not exceed the tolerance, flag the entire window as suspect
#           if (!is.na(rng) && rng <= current_tol) {
#             flag_vec[sus_idx] = pmax(flag_vec[sus_idx], 2)
#           }
#         }
#       }
#       
#       # Start time for the fail window:
#       fail_start = t[i] - as.difftime(fail_hours, units = "hours") # look back fail_hours from the current timestamp
#       fail_idx = which(t >= fail_start & t <= t[i]) # Row indices that fall inside the fail time window
#       
#       # Only evaluate if the window contains more than one row
#       if (length(fail_idx) > 1) {
#         vals = x[fail_idx] # Values inside the fail window
#         # Skip if all values are missing
#         if (!all(is.na(vals))) {
#           # Range across the fail window:
#           # max value minus min value
#           rng = max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
#           
#           # If range does not exceed the tolerance, flag the entire window as fail
#           if (!is.na(rng) && rng <= current_tol) {
#             flag_vec[fail_idx] = pmax(flag_vec[fail_idx], 3) 
#           }
#         }
#       }
#     }
#     
#     # Keep missing values flagged as 0
#     # If a missing value is also in the first 4 rows, preserve 0.5 instead
#     flag_vec[is.na(x)] = 0
#     flag_vec[seq_len(min(4, n))] = pmax(flag_vec[seq_len(min(4, n))], 0.5)
#     
#     # Store the variable-specific flatline flag results in a new column
#     data[[paste0("test.Flatline_", var)]] = flag_vec
#   }
#   
#   # Combine variable-specific flatline flags into one overall flatline flag
#   # pmax() returns the highest severity flag across variables for each row
#   data = data %>%
#     mutate(test.Flatline = do.call(
#       pmax,
#       c(select(., starts_with("test.Flatline_")), na.rm = TRUE)
#     ))
#   
#   return(data)
# }

# Flatline Test 2.0 
# flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
#   # Tests NEP data for consecutive unchanging values
#   #  0 - Test not ran
#   #  0.5 - Insufficient data
#   #  1 - Pass
#   #  2 - Suspect
#   #  3 - Fail
#   SUS_NUM = num_flatline_sus 
#   FAIL_NUM = num_flatline_fail 
#   data = site_data 
#   
#   for (var in vars_to_test) {
#     current_tol = flatline_thresholds[[var]]
#     if (exists("progress_print_option") && tolower(progress_print_option) %in% c('y','yes')) {
#       print(paste('Processing flatline for:', var, '| Threshold:', current_tol, '| at', Sys.time()))
#     }
#     # We create a temporary column to calculate the run lengths
#     # consecutive_id increments every time the value changes beyond the tolerance
#     data = data %>% 
#       mutate(
#         # identify where the value changes more than the tolerance
#         diff_val = c(0,abs(diff(.data[[var]]))),
#         is_break = if_else(diff_val > current_tol | is.na(diff_val), 1, 0),
#         run_id = cumsum(is_break)
#       ) %>% 
#       group_by(run_id) %>% 
#       mutate(
#         total_run_len = n(),
#         # apply flag logic to whole group
#         flag_val = case_when(
#           # global_row_idx < 5 ~ 0.5, # insufficient data
#           is.na(.data[[var]]) ~ 0,
#           total_run_len >= FAIL_NUM ~ 3,
#           total_run_len >= SUS_NUM ~ 2,
#           TRUE ~ 1
#           )
#         ) %>% 
#       ungroup()
#     # Apply the "insufficient data" flag (0.5) to the first 4 rows globally
#     data = data %>% 
#       mutate(flag_val = if_else(row_number() < 5, 0.5, flag_val))
#     # assign to final column name
#     data[[paste0('test.Flatline_',var)]] = data$flag_val
#     # clean up temporary columns
#     data = data %>% select(-diff_val, -is_break, -run_id, -total_run_len, -flag_val)
#   }
#   # Create overall test.Flatline column
#   data = data %>% 
#     mutate(test.Flatline = do.call(pmax, c(select(., starts_with('test.Flatline_')), na.rm=TRUE)))
#   return(data)
# }



# # # Testing flatline 2.0:
# car_test = flatline_test(SF_car, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds)
# tib_test = flatline_test(SF_tib, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds)

# #### OLD (newish) Flatline Test 1.1 ####
# flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
#   # Tests NEP data for consecutive unchanging values
#   #  0 - Test not ran
#   #  0.5 - Insufficient data (not enough previous entries)
#   #  1 - Pass
#   #  2 - Suspect - 3 or 4 equal repeated values
#   #  3 - Fail - 5+ equal repeated values 
#   # - - - - - - - - - - - - - - - - - -
#   SUS_NUM = num_flatline_sus 
#   FAIL_NUM = num_flatline_fail 
#   data = site_data |> 
#     mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Flatline_{.col}'))
#   # Apply test logic:
#   for (var in vars_to_test) {
#     # DIRECT MATCHING: Extract threshold specifically assigned to this 'var'
#     current_tol = flatline_thresholds[[var]]
#     if (exists("progress_print_option") && tolower(progress_print_option) %in% c('y','yes')) {
#       print(paste('Processing flatline for:', var, '| Threshold:', current_tol, '| at', Sys.time()))
#     }
#     
#     col = data[[var]]
#     flatline_lengths = get_flatline_lengths(col, current_tol)
#     
#     flag_col = case_when(
#       row_number(data) < 5 ~ 0.5, # INSUFFICIENT DATA
#       is.na(col) ~ 0,                   # TEST NOT RAN
#       flatline_lengths >= FAIL_NUM ~ 3, # FAIL
#       flatline_lengths >= SUS_NUM ~ 2, # SUSPECT
#       TRUE ~ 1                         # PASS
#     )
#     data[[paste0('test.Flatline_',var)]] = flag_col
#   }
#   # create overall test.Flatline column
#   data = data |> 
#     mutate(test.Flatline = do.call(pmax, c(select(data, starts_with('test.Flatline_')), na.rm=TRUE)))
#   return(data)
# }

# # OLD FLATLINE TEST #####
# get_flatline_lengths = function(x) {
#   # function which looks at a list of values and returns a list of how many times each value has been repeated
#   # e.g. if x = c(1,2,2,5,5,5) this would return: c(1,1,2,1,2,3)
#   n = length(x)
#   flatline_lengths = rep(1,n) # create a list of 1's, equal in length to the list 'x'
#   for (i in 2:n) {
#     if (!is.na(x[i]) && !is.na(x[i-1]) && x[i] == x[i-1]) { # if the value is equal to the previous value: add 1 to the i'th flatline tracker 
#       flatline_lengths[i] = flatline_lengths[i-1]+1
#     }
#   }
#   return(flatline_lengths)
# }
# flatline_test = function(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_threshold) {
#   # Tests NEP data for consecutive unchanging values
#   #  0 - Test not ran
#   #  0.5 - Insufficient data (not enough previous entries)
#   #  1 - Pass
#   #  2 - Suspect - 3 or 4 equal repeated values
#   #  3 - Fail - 5+ equal repeated values 
#   # - - - - - - - - - - - - - - - - - -
#   SUS_NUM = num_flatline_sus 
#   FAIL_NUM = num_flatline_fail 
#   data = site_data |> 
#     mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Flatline_{.col}'))
#   # Apply test logic:
#   for (var in vars_to_test) {
#     if (tolower(progress_print_option) %in% c('y','yes')) {
#       print(paste('Processing flatline for:',var,'at',Sys.time()))
#     }
#     col = data[[var]]
#     flatline_lengths = get_flatline_lengths(col)
#     flag_col = case_when(
#       row_number(data) < 5 ~ 0.5, # INSUFFICIENT DATA
#       is.na(col) ~ 0,                   # TEST NOT RAN
#       flatline_lengths >= FAIL_NUM ~ 3, # FAIL
#       flatline_lengths >= SUS_NUM ~ 2, # SUSPECT
#       TRUE ~ 1                         # PASS
#     )
#     data[[paste0('test.Flatline_',var)]] = flag_col
#   }
#   # create overall test.Flatline column
#   data = data |> 
#     mutate(test.Flatline = do.call(pmax, c(select(data, starts_with('test.Flatline_')), na.rm=TRUE)))
#   return(data)
# }

# CLIMATOLOGY TEST #####
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


# NEW RATE OF CHANGE TEST: 3 functions # # Updated as of 3/3/2026
# Function 1: interpolate data
interpolate_data = function(data, vars_to_test, time_interval=sample_interval) {
  # 1. Round the raw data timestamps to the nearest minute to fix the "jitter"
  sorted_data = data |> 
    drop_na(datetime.utc) |> 
    mutate(datetime.utc = round(datetime.utc, "mins")) |>
    distinct(datetime.utc, .keep_all = TRUE) # Ensure no duplicate rows after rounding
  
  data_length = length(sorted_data$datetime.utc)
  time_min = sorted_data$datetime.utc[1]
  time_max = sorted_data$datetime.utc[data_length]
  
  # 2. Create the clean sequence (also rounded to be safe)
  all_times = seq(time_min, time_max, by=(time_interval*60))
  data_interp = data.frame(datetime.utc = all_times)
  
  # 3. Join the data - this will now work because both sides are rounded to the minute
  for (var in vars_to_test) {
    testdata = sorted_data |> 
      select(datetime.utc, all_of(var))
    
    data_interp = data_interp |> 
      left_join(testdata, by = 'datetime.utc')
  }
  return(data_interp)
}


# Rate of Change Function 2: Calculate rolling standard deviation
# Updated for faster initialization
calc_rolling_sd = function(data_interp, vars_to_test, time_interval=sample_interval, sd_window_hrs = 25, min_non_na = 20) {
  # Window for 25 hours
  sampling_window = (60 / time_interval) * sd_window_hrs 
  for (var in vars_to_test) {
    if (var %in% names(data_interp)) {
      sd_col_name = paste0('sd_', var)
      
      data_interp = data_interp |> 
        mutate(
          !!sd_col_name := slide_dbl(
            .x = !!sym(var), 
            .f = function(x) {
              if (sum(!is.na(x)) >= min_non_na) {
                return(sd(x, na.rm = TRUE))
              } else {
                return(NA_real_)
              }
            },
            .before = sampling_window,
            .complete = FALSE
          )
        )
    } else {
      message("Missing variable from interpolated data: ", var)
    }
  }
  return(data_interp)
}

# rate change fn 3.2.2
rate_change_test = function(data, data_interp, vars_to_test, num_sd_for_rate_change = 3) {
  # 1. Prepare a 'lookup' table from your interpolated SDs
  # We round to the nearest minute to handle the 1-2 second jitters
  sd_lookup_table = data_interp |>
    mutate(match_time = round(datetime.utc, "mins")) |>
    select(match_time, starts_with("sd_")) |>
    distinct(match_time, .keep_all = TRUE) # prevent many-to-many errors
  # 2. Join the SDs into your main data
  data_joined = data |>
    mutate(match_time = round(datetime.utc, "mins")) |>
    left_join(sd_lookup_table, by = "match_time")
  # 3. Run the test logic using the joined columns
  for (var in vars_to_test) {
    sd_col = paste0("sd_", var)
    test_col = paste0("test.RateChange_", var)
    
    data_joined = data_joined |> 
      mutate(!!test_col := case_when(
        is.na(.data[[var]]) | is.na(lag(.data[[var]])) ~ 0.5,
        is.na(.data[[sd_col]]) ~ 0, # This '0' now truly means "No SD found for this time"
        abs(.data[[var]] - lag(.data[[var]])) > num_sd_for_rate_change * .data[[sd_col]] ~ 2,
        TRUE ~ 1
      ))
  }
  # 4. Clean up and return
  data_joined = data_joined |>
    select(-match_time, -starts_with("sd_")) |>
    mutate(test.RateChange = do.call(pmax, c(select(data_joined, starts_with('test.RateChange_')), na.rm=TRUE)))
  
  return(data_joined)
}


# Previously: site_data = attenuated_signal_test(site_data, data_interp, vars_to_test, attenuated_signal_thresholds, time_window, time_interval)


# ATTENUATED SIGNAL TEST 2.0 - running against standard deviation of a prevailing time period, rather than a set +/- value

# attenuated_signal_test_sd = function(data, data_interp, vars_to_test, time_window=24, attsig_fail = 0.5, attsig_sus = 1.0, time_interval = sample_interval) {
#   # Tests NEP data for a near-flatline (change relative to a designated suspect and fail threshold ('attenuated_signal_thresholds'))
#   # time_window: how far back (in hours, default = 24 hours) you want to assess the min/max range of values for the test
#   # attsig_fail: the suspect threshold to be tested against (max-min not exceeding this many SDs will result in a fail flag)
#   # attsig_sus: the suspect threshold to be tested against (max-min not exceeding this many SDs will result in a suspect flag)
#   # time_interval: the MINIMUM interval (in minutes, default = 15 min) between measurements in the dataset
#   #    --> NOTE: Even if 90% of the dataset is a 15-min interval, if 10% is a 5-min interval, choose 5 minutes. This function will
#   #              interpolate the data time-series and remove any rows that didn't take measurements. This ensures full data inclusion
#   # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   # Test flags:
#   #  0 - Test not ran
#   #  1 - Pass - exceeds both suspect and fail thresholds for change
#   #  2 - Suspect - exceeds fail threshold but does not exceed suspect threshold
#   #  3 - Fail - does not exceed fail threshold
#   # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   # 1. Prepare Join Keys (Snap to 1-minute grid to handle jitter)
#   data_rounded = data %>%
#     mutate(join_time = lubridate::round_date(datetime.utc, "1 minute"))
#   
#   interp_rounded = data_interp %>%
#     mutate(join_time = lubridate::round_date(datetime.utc, "1 minute"))
#   
#   # 2. Calculate SDs using your provided function
#   # This creates the 'sd_ph', 'sd_temp.c' etc. columns
#   interp_rounded = calc_rolling_sd(interp_rounded, vars_to_test, 
#                                    time_interval = time_interval, 
#                                    sd_window_hrs = time_window)
#   
#   # 3. Calculate Rolling Range (Max - Min)
#   window_before = lubridate::hours(6)
#   
#   for (var in vars_to_test) {
#     message(paste("Calculating range for:", var))
#     
#     interp_rounded = interp_rounded %>%
#       mutate(
#         !!paste0(var, "_max") := slide_index_dbl(
#           .x = !!sym(var), 
#           .i = join_time, 
#           .f = ~if(all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE), 
#           .before = window_before
#         ),
#         !!paste0(var, "_min") := slide_index_dbl(
#           .x = !!sym(var), 
#           .i = join_time, 
#           .f = ~if(all(is.na(.x))) NA_real_ else min(.x, na.rm = TRUE), 
#           .before = window_before
#         )
#       ) %>%
#       mutate(!!paste0(var, "_diff") := !!sym(paste0(var, "_max")) - !!sym(paste0(var, "_min")))
#   }
#   
#   # 4. Join the SD and Diff columns back to the raw data
#   # We select 'join_time' plus any column starting with 'sd_' or ending in '_diff'
#   data_results = data_rounded %>%
#     left_join(
#       interp_rounded %>% select(join_time, starts_with("sd_"), ends_with("_diff")), 
#       by = "join_time"
#     )
#   
#   # 5. Run the Flagging Logic
#   for (var in vars_to_test) {
#     diff_col_name = paste0(var, "_diff")
#     sd_col_name = paste0("sd_", var)
#     
#     # Debug: Check if the column actually exists in the joined dataframe
#     if (!sd_col_name %in% names(data_results)) {
#       warning(paste("Column", sd_col_name, "not found after join. Skipping logic for", var))
#       next
#     }
#     
#     data_results = data_results %>%
#       mutate(!!paste0("test.AttenuatedSignal_", var) := case_when(
#         is.na(!!sym(var)) | is.na(!!sym(sd_col_name)) | is.na(!!sym(diff_col_name)) ~ 0,
#         !!sym(diff_col_name) < (0.5 * !!sym(sd_col_name)) ~ 3,
#         !!sym(diff_col_name) < (1.0 * !!sym(sd_col_name)) ~ 2,
#         TRUE ~ 1
#       ))
#   }
#   
#   # 6. Final Summary and Cleanup
#   data_results = data_results %>%
#     mutate(test.AttenuatedSignal = do.call(pmax, c(select(., starts_with("test.AttenuatedSignal_")), na.rm = TRUE))) %>%
#     # select(-join_time, -starts_with("sd_"), -ends_with("_diff"), -ends_with("_max"), -ends_with("_min"))
#   
#   return(data_results)
# }


# Attenuated Signal Test 3.0
dynamic_attenuated_test = function(data, data_interp, vars_to_test, thresholds, time_interval = sample_interval) {
  dt_raw = as.data.table(data)
  dt_interp = as.data.table(data_interp)
  
  setkey(dt_raw, datetime.utc)
  setkey(dt_interp, datetime.utc)
  
  n_rows = 12*60 / time_interval
  
  for (var in vars_to_test) {
    if (tolower(progress_print_option) %in% c('y','yes')) {
      print(paste('Processing attenuated signal for:',var,'at',Sys.time()))
    }
    
    sd_col = paste0('sd_', var)
    test_col = paste0('test.AttenuatedSignal_', var)
    
    # 1. Pull the pre-calculated 12-hour SD into the raw data table
    dt_raw[, current_sd := dt_interp[dt_raw, get(sd_col), on = .(datetime.utc), roll = "nearest"]]
    
    # 2. Logic: Compare the 12-hour SD directly to your thresholds
    # (Assuming thresholds are absolute values, e.g., 0.01 for pH)
    dt_raw[, (test_col) := 1] 
    dt_raw[current_sd < thresholds[[var]]$sus, (test_col) := 2]
    dt_raw[current_sd < thresholds[[var]]$fail, (test_col) := 3]
    
    # 3. Handle missing data
    dt_raw[is.na(get(var)) | is.na(current_sd), (test_col) := 0]
    
    # Cleanup temp column
    dt_raw[, current_sd := NULL]
  }
    
  #   # 1. Get the current SD to see if flat right now
  #   dt_raw[, sd_val_temp := dt_interp[dt_raw, get(sd_col), on = .(datetime.utc), roll = "nearest"]]
  #   
  #   # 2. Dynamic Baseline
  #   dt_interp[, rolling_median_sd := frollapply(get(sd_col), n=n_rows, FUN = median, na.rm=TRUE, align='right')]
  #   # Map rolling baseline back to dt_raw
  #   dt_raw[, typical_sd := dt_interp[dt_raw, rolling_median_sd, on = .(datetime.utc), roll='nearest']]
  #   
  #   # typical_sd = median(dt_interp[[sd_col]], na.rm = TRUE)
  #   # 
  #   # # Use your 'sus' and 'fail' column names here
  #   # fail_limit = typical_sd * thresholds[[var]]$fail
  #   # sus_limit = typical_sd * thresholds[[var]]$sus
  #   
  #   # 3. Logic: Default to 1, then apply 2, 3, and finally 0 for true missing data
  #   dt_raw[, (test_col) := 1] 
  #   dt_raw[sd_val_temp < (typical_sd * thresholds[[var]]$sus), (test_col) := 2]
  #   dt_raw[sd_val_temp < (typical_sd * thresholds[[var]]$fail), (test_col) := 3]
  #   # dt_raw[sd_val_temp < sus_limit, (test_col) := 2]
  #   # dt_raw[sd_val_temp < fail_limit, (test_col) := 3]
  #   
  #   # If the raw data is NA OR the rolling SD is NA, mark as 'Not Run'
  #   dt_raw[is.na(get(var)) | is.na(sd_val_temp), (test_col) := 0]
  #   
  #   dt_raw[, c('sd_val_temp','typical_sd') := NULL]
  #   dt_interp[, rolling_median_sd := NULL]
  # }
  
  # 5. Summary flag
  test_cols = grep("^test\\.AttenuatedSignal_", names(dt_raw), value = TRUE)
  dt_raw[, test.AttenuatedSignal := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = test_cols]
  
  return(as.data.frame(dt_raw))
}



# OLD ATTENUATED SIGNAL TEST #
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
qaqc_nep = function(data, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_window,
                    time_interval, attenuated_signal_thresholds, time_window_attsig=12 , num_sd_for_rate_change, num_flatline_sus, num_flatline_fail, flatline_thresholds) {
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
    site_data = flatline_test(site_data, vars_to_test, num_flatline_sus, num_flatline_fail, flatline_thresholds)
    # climatology:
    site_data = climatology_test(site_data, vars_to_test, seasonal_thresholds)
    # rate of change:
    site_data_interp = interpolate_data(site_data, vars_to_test, time_interval) # interpolate missing timestamps and values per site
    data_interp = calc_rolling_sd(site_data_interp, vars_to_test,time_interval, min_non_na = 20)
    site_data = rate_change_test(site_data, data_interp, vars_to_test, num_sd_for_rate_change)
    # attenuated signal:
    # site_data = attenuated_signal_test(site_data, data_interp, vars_to_test, attenuated_signal_thresholds, time_window, time_interval)
    site_rollingSD = calc_rolling_sd(site_data_interp, vars_to_test, time_interval, sd_window_hrs=time_window_attsig, min_non_na = 20)
    site_data = dynamic_attenuated_test(site_data, site_rollingSD, vars_to_test, attenuated_signal_thresholds)
    
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

