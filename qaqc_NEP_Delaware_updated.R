# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: June 9, 2025
# DATE OF LAST UPDATE: Apr 28, 2026 
# Updated by: Andrew Mandovi 

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                     What this Script Does: 
#                    ----------------------
#  1. Defines parameters and thresholds unique to Delaware Inland Bays before running 
#  2. Runs qaqc script for DELAWARE INLAND BAYS
#  3. Saves the results (optional)
#  *. This Script can be called from the qaqc_run_all.R script
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

cat('Processing NEP: Delaware Inland Bay \n')

##### Step 1. PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: ####

# For Gross-Range Test:
ph_user_min = 6
ph_user_max = 10
temp_user_min = 0
temp_user_max = 40
sal_user_min = 1
sal_user_max = 30
co2_user_min = 100
co2_user_max = 2500
do_user_min = 0
do_user_max = 30
# sensor min/max's
ph_sensor_min = 0
ph_sensor_max = 14
temp_sensor_min = -10
temp_sensor_max = 45
sal_sensor_min = -0.001
sal_sensor_max = 50
co2_sensor_min = 0
co2_sensor_max = 3500
do_sensor_min = 0
do_sensor_max = 35
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
  ph_max = list(DJF = 10, MAM = 10, JJA = 10, SON = 10),
  temp.c_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  temp.c_max = list(DJF = 40, MAM = 40, JJA = 40, SON = 40),
  sal.ppt_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  sal.ppt_max = list(DJF = 36, MAM = 36, JJA = 36, SON = 36),
  do.mgl_min = list(DJF = 0, MAM = 0, JJA = 0, SON = 0),
  do.mgl_max = list(DJF = 30, MAM = 30, JJA = 30, SON = 30),
  co2.ppm_min = list(DJF = 100, MAM = 100, JJA = 100, SON = 100),
  co2.ppm_max = list(DJF = 2000, MAM = 2000, JJA = 2000, SON = 2000)
)
# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
min_num_pts_rate_of_change = 3
sample_interval = 30 # minutes
# For Flatline Test:
# For Flatline Test:
num_flatline_sus = 24 # 12 hours
num_flatline_fail = 48 # 24 hours
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

#### Step 2: Running QA script for Delaware Inland Bays: ####

vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')
# RUN SCRIPT:
gonski = data_list$DelawareInland[site.code == 'USCG']
Delaware_noGonski = data_list$DelawareInland[site.code != 'USCG']

qa_delaware = qaqc_nep(Delaware_noGonski, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_window,
                       time_interval=sample_interval, attenuated_signal_thresholds, time_window_attsig, num_sd_for_rate_of_change, num_flatline_sus, num_flatline_fail,flatline_thresholds)

### CREATE 'flags' column to take the maximum (worst) flag across the row:
qa_delaware = qa_delaware |> 
  mutate(flags = do.call(pmax, c(select(qa_delaware, starts_with('test.')), na.rm=TRUE)))
# Create 'flags' column in Gonski data too
gonski = gonski %>% 
  mutate(flags = case_when(
    ph_flag == 'mf' ~ 3,
    TRUE ~ 1
  ))

# Combine just-now-QA/QC'd delaware data with data which had previously been QA/QC'd:
delaware_combined = rbind(gonski,qa_delaware, fill=TRUE)

qa_delaware = delaware_combined
qa_delaware = qa_delaware %>% 
  mutate(ph_flag = do.call(pmax, c(select(qa_delaware, ends_with('_ph')),na.rm=TRUE)),
         do_flag = do.call(pmax, c(select(qa_delaware, ends_with('_do.mgl')),na.rm=TRUE)),
         temp_flag = do.call(pmax,c(select(qa_delaware, ends_with('_temp.c')),na.rm=TRUE)),
         sal_flag = do.call(pmax,c(select(qa_delaware, ends_with('_sal.ppt')),na.rm=TRUE))
  )

qa_delaware = qa_delaware %>% select(-co2.ppm)

qa_delaware = qa_delaware %>% 
  rename_with(~ gsub('.','_',.x, fixed=TRUE))

qa_delaware = qa_delaware %>% mutate(flags_revision = flags)

qa_delaware_filtered = qa_delaware %>% 
  filter(flags == 1)

nep_unfiltered_data$DelawareInland = qa_delaware
nep_filtered_data$DelawareInland = qa_delaware_filtered

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
save(nep_unfiltered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_unfiltered_data_",timestamp,".Rdata"))
save(nep_filtered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_filtered_data_",timestamp,".Rdata"))


# qa_data_list$DelawareInland = delaware_combined
# qa_data_list$DelawareInland = qa_data_list$DelawareInland |> 
#   mutate(ph_flag = do.call(pmax, c(select(qa_data_list$DelawareInland, ends_with('_ph')),na.rm=TRUE)),
#          do_flag = do.call(pmax, c(select(qa_data_list$DelawareInland, ends_with('_do.mgl')),na.rm=TRUE)),
#          temp_flag = do.call(pmax,c(select(qa_data_list$DelawareInland, ends_with('_temp.c')),na.rm=TRUE)),
#          sal_flag = do.call(pmax,c(select(qa_data_list$DelawareInland, ends_with('_sal.ppt')),na.rm=TRUE))
#   )
#---------

#### Step 3: Saving Options ####

# if (interactive()) {
#   if (tolower(save_Odrive_option) %in% c('y','yes')) {
#     save_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/qa_delaware.Rdata'
#     cat('Saving qa_delaware to:',save_path,'\n')
#     save(qa_delaware, file=save_path)
#     cat('qa_delaware saved successfully to O:drive. \n')
#   } else {
#     cat('Skipped saving to O:drive. \n')
#   }
#   if (tolower(save_local_option) %in% c('y','yes')) {
#     save_path = getwd()
#     cat('Saving Delaware data locally to current directory \n')
#     save(qa_delaware, file = paste0(getwd(),'qa_delaware'))
#     cat('qa_delaware saved locally. \n')
#   }
# } else {
#   cat('Skipped saving locally. \n')
# }


# Time spent beneath thresholds
# Packages
library(dplyr)
library(lubridate)

# Optional pretty table export (if installed)
has_gt <- requireNamespace("gt", quietly = TRUE)
has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)

# Parse UTC helper
parse_utc_dt <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(with_tz(x, tzone = "UTC"))
  } else {
    dt <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
    if (all(is.na(dt))) dt <- as.POSIXct(x, tz = "UTC")
    dt
  }
}

# Event detector: contiguous periods below threshold (irregular sampling OK)
# gap_tol_factor bridges small gaps: event breaks if gap > gap_tol_factor * median sampling interval
make_events <- function(df, var, thr, min_duration = lubridate::dseconds(0), gap_tol_factor = 2) {
  stopifnot(all(c("site_code","datetime_utc", var) %in% names(df)))
  
  df %>%
    mutate(
      t = parse_utc_dt(datetime_utc),
      v = as.numeric(.data[[var]])
    ) %>%
    filter(!is.na(t)) %>%
    arrange(site_code, t) %>%
    group_by(site_code) %>%
    mutate(
      below = !is.na(v) & v < thr,
      dt    = as.numeric(difftime(t, lag(t), units = "secs")),
      med_dt = suppressWarnings(median(dt, na.rm = TRUE)),
      gap_tol = gap_tol_factor * med_dt,
      new_event = below & (is.na(lag(below)) | !lag(below) | dt > gap_tol),
      event_id  = cumsum(coalesce(new_event, FALSE)),
      event_id  = if_else(below, event_id, NA_integer_)
    ) %>%
    filter(below) %>%
    group_by(site_code, event_id) %>%
    summarize(
      start = first(t),
      end   = last(t),
      duration_secs = as.numeric(difftime(end, start, units = "secs")),
      n_points = n(),
      min_value = min(v, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Keep events ≥ min_duration
    filter(duration_secs >= as.numeric(min_duration))
}

# Summarize events into a per-site table, plus an "ALL" row
event_summary_table <- function(df, var, thr,
                                min_duration = lubridate::dseconds(0),
                                gap_tol_factor = 2) {
  ev <- make_events(df, var = var, thr = thr,
                    min_duration = min_duration, gap_tol_factor = gap_tol_factor)
  
  to_hours <- function(x) x / 3600
  
  if (nrow(ev) == 0) {
    # Return sites with zero events
    sites <- sort(unique(as.character(df$site_code)))
    site_tbl <- tibble(
      site_code = sites,
      variable = var, threshold = thr,
      n_events = 0L,
      mean_duration_h = NA_real_,
      median_duration_h = NA_real_,
      sd_duration_h = NA_real_,
      min_duration_h = NA_real_,
      max_duration_h = NA_real_,
      total_duration_h = 0
    )
    all_row <- tibble(
      site_code = "ALL",
      variable = var, threshold = thr,
      n_events = 0L,
      mean_duration_h = NA_real_,
      median_duration_h = NA_real_,
      sd_duration_h = NA_real_,
      min_duration_h = NA_real_,
      max_duration_h = NA_real_,
      total_duration_h = 0
    )
    bind_rows(site_tbl, all_row)
  } else {
    site_tbl <- ev %>%
      group_by(site_code) %>%
      summarize(
        n_events = n(),
        mean_duration_h = mean(to_hours(duration_secs)),
        median_duration_h = median(to_hours(duration_secs)),
        sd_duration_h = sd(to_hours(duration_secs)),
        min_duration_h = min(to_hours(duration_secs)),
        max_duration_h = max(to_hours(duration_secs)),
        total_duration_h = sum(to_hours(duration_secs)),
        .groups = "drop"
      ) %>%
      mutate(variable = var, threshold = thr) %>%
      select(site_code, variable, threshold, everything())
    
    all_row <- ev %>%
      summarize(
        site_code = "ALL",
        n_events = n(),
        mean_duration_h = mean(to_hours(duration_secs)),
        median_duration_h = median(to_hours(duration_secs)),
        sd_duration_h = sd(to_hours(duration_secs)),
        min_duration_h = min(to_hours(duration_secs)),
        max_duration_h = max(to_hours(duration_secs)),
        total_duration_h = sum(to_hours(duration_secs))
      ) %>%
      mutate(variable = var, threshold = thr) %>%
      select(site_code, variable, threshold, everything())
    
    bind_rows(site_tbl, all_row) %>%
      arrange(match(site_code, c(sort(unique(as.character(df$site_code))), "ALL")))
  }
}

# Pretty export with gt (if available)
export_event_summary_gt <- function(tbl, nep_name, out_file) {
  if (!(has_gt && has_webshot2)) {
    message("gt/webshot2 not available; printing to console instead.")
    print(tbl, n = Inf); return(invisible(NULL))
  }
  library(gt)
  gt_tbl <- gt(tbl) |>
    tab_header(
      title = paste0("Average time below threshold — ", nep_name),
      subtitle = "Per site (ALL = combined across sites)"
    ) |>
    fmt_number(columns = c(threshold), decimals = 3) |>
    fmt_number(columns = ends_with("_h"), decimals = 2) |>
    cols_label(
      site_code = "Site",
      variable = "Variable",
      threshold = "Threshold",
      n_events = "Events",
      mean_duration_h = "Mean dur (h)",
      median_duration_h = "Median dur (h)",
      sd_duration_h = "SD dur (h)",
      min_duration_h = "Min dur (h)",
      max_duration_h = "Max dur (h)",
      total_duration_h = "Total dur (h)"
    ) |>
    tab_spanner(label = "Durations (hours)",
                columns = c(mean_duration_h, median_duration_h, sd_duration_h,
                            min_duration_h, max_duration_h, total_duration_h))
  
  gtsave(gt_tbl, out_file)
  message(sprintf("Saved: %s", out_file))
}

# =====================
# Run for DelawareInland
# =====================
del <- nep_filtered_data[["DelawareInland"]]

# Check required columns
req <- c("site_code","datetime_utc","ph_T","do_mgl")
missing <- setdiff(req, names(del))
if (length(missing) > 0) {
  stop(sprintf("DelawareInland is missing required column(s): %s", paste(missing, collapse = ", ")))
}

# Compute per-site summaries for pH and DO (no min duration; gap tolerance = 2x median interval)
tab_ph <- event_summary_table(del, var = "ph_T",  thr = 6.5,
                              min_duration = lubridate::dseconds(0),
                              gap_tol_factor = 2)
tab_do <- event_summary_table(del, var = "do_mgl", thr = 2,
                              min_duration = lubridate::dseconds(0),
                              gap_tol_factor = 2)

# Combine and print
event_tbl <- bind_rows(tab_ph, tab_do) %>%
  mutate(variable = dplyr::recode(variable, ph_T = "pH (total)", do_mgl = "DO (mg/L)"))

print(event_tbl, n = Inf)
write_csv(event_tbl, "DelawareInland_time_below_threshold.csv")
