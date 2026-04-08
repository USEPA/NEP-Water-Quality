# Packages
library(data.table)
library(lubridate)
library(stringr)
library(fs)

# ---------------------------------------------------------------------------
# 1) Configure: folders, time zone, sensor preference
# ---------------------------------------------------------------------------
local_dirs <- c('C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/SF subfolder/pressure_raw')   # <-- change/add as needed
tz_local   <- "America/Los_Angeles"    # <-- set correctly for your logger local time
preferred_sensor_regex <- NULL         # e.g., "YSI" to prefer YSI in overlaps

# ---------------------------------------------------------------------------
# 2) Discover files (.txt/.csv/.dat), parse metadata robustly
#    - Extract first two date strings (7 or 8 digits) anywhere in the name
#    - Determine site token ("cma" or "rtc") and map rtc -> EOS
#    - Determine sensor from the pre-date tokens (everything except the site token)
#    - Prefer csv/txt over dat when duplicative by (site, sensor, start, end)
# ---------------------------------------------------------------------------

# Helper: find first two date-like tokens (7 or 8 digits). Coerce 7 digits -> YYYYMM0D
extract_dates <- function(s) {
  m <- gregexpr("\\d{7,8}", s, perl = TRUE)[[1]]
  if (length(m) < 2 || m[1] == -1) return(list(start = NA, end = NA, end_pos = NA_integer_))
  starts <- as.integer(m[1:2])
  lens   <- attr(m, "match.length")[1:2]
  d_raw  <- substring(s, starts, starts + lens - 1)
  # pad 7-digit dates (assumed YYYYMMd -> YYYYMM0d)
  d_fix <- vapply(d_raw, function(x) {
    if (nchar(x) == 7) paste0(substr(x, 1, 6), "0", substr(x, 7, 7)) else x
  }, character(1))
  d1 <- suppressWarnings(ymd(d_fix[1]))
  d2 <- suppressWarnings(ymd(d_fix[2]))
  list(start = d1, end = d2, end_pos = starts[2] + lens[2] - 1)
}

# Helper: parse site and sensor from the "prefix" (portion before the first date)
# Accepts both "site_sensor" and "sensor_site" ordering; site token = CMA or RTC
parse_site_sensor <- function(prefix_raw) {
  # normalize separators to underscores, lower-case
  prefix <- gsub("[\\s-]+", "_", tolower(prefix_raw))
  toks   <- unlist(strsplit(prefix, "_"))
  toks   <- toks[nzchar(toks)]
  if (length(toks) == 0) return(list(site_code = NA_character_, sensor_file = NA_character_))
  # find site token
  site_tok_idx <- which(toks %in% c("cma", "rtc"))
  if (length(site_tok_idx) == 0) {
    return(list(site_code = NA_character_, sensor_file = paste(toks, collapse = "_")))
  }
  # choose first occurrence if multiple
  site_tok <- toks[site_tok_idx[1]]
  site_code <- if (site_tok == "rtc") "EOS" else toupper(site_tok)  # map rtc -> EOS; cma -> CMA
  sensor_toks <- toks[-site_tok_idx[1]]
  sensor_file <- if (length(sensor_toks)) paste(sensor_toks, collapse = "_") else NA_character_
  list(site_code = site_code, sensor_file = sensor_file)
}

# Gather candidate files
files_all <- unlist(lapply(local_dirs, function(d) {
  if (!fs::dir_exists(d)) {
    warning(sprintf("Directory does not exist: %s", d))
    return(character())
  }
  fs::dir_ls(d, recurse = TRUE, type = "file", regexp = "\\.(csv|txt|dat)$", invert = FALSE)
}), use.names = FALSE)

if (length(files_all) == 0L) stop("No .csv/.txt/.dat files found in the provided local folders.")

# Build metadata table (create fname first; then stem)
meta <- data.table(path = files_all)

# Create fname first so it's available to subsequent steps
meta[, fname := basename(path)]

# Derive stem and other fields using existing columns
meta[, stem := sub("\\.(csv|txt|dat)$", "", fname, ignore.case = TRUE)]
meta[, ext := tolower(fs::path_ext(path))]
meta[, file_mtime := file.info(path)$mtime]

# Extract dates and prefix (portion up to 2nd date); allow notes after 2nd date
meta[, `:=`(
  dates   = lapply(stem, extract_dates),
  s_date  = as.Date(NA),
  e_date  = as.Date(NA),
  prefix  = NA_character_
)]

for (i in seq_len(nrow(meta))) {
  d <- meta$dates[[i]]
  meta$s_date[i] <- d$start
  meta$e_date[i] <- d$end
  if (!is.na(d$end_pos)) meta$prefix[i] <- substr(meta$stem[i], 1, d$end_pos)
}

# Parse site and sensor from prefix
parsed <- lapply(meta$prefix, parse_site_sensor)
meta[, site_code := vapply(parsed, `[[`, character(1), "site_code")]
meta[, sensor_file := vapply(parsed, `[[`, character(1), "sensor_file")]

# Keep only rows with parsed dates and known sites
meta <- meta[!is.na(s_date) & !is.na(e_date) & site_code %in% c("CMA", "EOS")]
if (nrow(meta) == 0L) stop("No files with recognizable dates and sites (CMA/EOS) were found.")

# Prefer CSV/TXT over DAT per (site_code, sensor_file, s_date, e_date)
meta[, ext_rank := fifelse(ext %in% c("csv", "txt"), 1L, 2L)]
setorder(meta, site_code, sensor_file, s_date, e_date, ext_rank, -file_mtime)
meta_unique <- meta[, .SD[1L], by = .(site_code, sensor_file, s_date, e_date)]

# Optional: inspect what was selected
# print(meta_unique[, .(site_code, sensor_file, s_date, e_date, ext, fname)])

# ---------------------------------------------------------------------------
# 3) Read a file: drop units row if present; expect Date/Time/Press/Depth
# ---------------------------------------------------------------------------
read_pressure_file <- function(path, site_code, sensor_file, tz_local = "UTC") {
  dt <- tryCatch(
    fread(path, header = TRUE, na.strings = c("", "NA", "NaN"),
          check.names = FALSE, showProgress = FALSE),
    error = function(e) {
      warning(sprintf("Failed to read %s (%s)", basename(path), e$message))
      return(NULL)
    }
  )
  if (is.null(dt) || nrow(dt) == 0L) return(NULL)
  
  # Require core headers
  needed <- c("Date", "Time", "Press", "Depth")
  if (!all(needed %in% names(dt))) {
    warning(sprintf("Skipping %s: missing one of required columns: %s",
                    basename(path), paste(setdiff(needed, names(dt)), collapse = ", ")))
    return(NULL)
  }
  
  # Drop units row if present (common: Y/M/D, HH:MM:SS, psir, meters)
  if (nrow(dt) >= 1L) {
    # Heuristic: units row if Press/Depth not numeric and contain unit strings
    p1 <- tolower(as.character(dt$Press[1]))
    d1 <- tolower(as.character(dt$Depth[1]))
    date1 <- tolower(as.character(dt$Date[1]))
    time1 <- tolower(as.character(dt$Time[1]))
    unit_like <- FALSE
    unit_like <- unit_like || grepl("psir|psi|psia|psig", p1)
    unit_like <- unit_like || grepl("meter|metre|^m$", d1)
    unit_like <- unit_like || grepl("y/?m/?d|yyyy|mm|dd", date1)
    unit_like <- unit_like || grepl("hh:?mm:?ss", time1)
    if (unit_like) dt <- dt[-1L]
  }
  if (nrow(dt) == 0L) return(NULL)
  
  # Coerce Press/Depth to numeric
  suppressWarnings({
    dt[, Press := as.numeric(Press)]
    dt[, Depth := as.numeric(Depth)]
  })
  
  # Build local datetime then convert to UTC
  dt[, datetime_local := as.POSIXct(paste(Date, Time), tz = tz_local, format = "%Y/%m/%d %H:%M:%S")]
  idx_na <- is.na(dt$datetime_local)
  if (any(idx_na)) {
    dt[idx_na, datetime_local := as.POSIXct(paste(gsub("/", "-", Date[idx_na]), Time[idx_na]),
                                            tz = tz_local, format = "%Y-%m-%d %H:%M:%S")]
  }
  dt[, datetime_utc := with_tz(datetime_local, "UTC")]
  
  # Keep only needed fields
  out <- dt[!is.na(datetime_utc), .(
    datetime_utc,
    press_psir = Press,
    depth_m    = Depth
  )]
  
  if (nrow(out) == 0L) return(NULL)
  
  # Attach metadata, de-dup within file by timestamp
  out[, `:=`(
    site_code            = site_code,
    pressure_sensor_file = sensor_file,
    file_path            = path
  )]
  setorder(out, datetime_utc)
  out <- unique(out, by = "datetime_utc")
  out
}

# Read all chosen files
pressure_list <- vector("list", nrow(meta_unique))
for (i in seq_len(nrow(meta_unique))) {
  pressure_list[[i]] <- read_pressure_file(
    path        = meta_unique$path[i],
    site_code   = meta_unique$site_code[i],
    sensor_file = meta_unique$sensor_file[i],
    tz_local    = tz_local
  )
}
pressure_dt <- rbindlist(pressure_list, use.names = TRUE, fill = TRUE)
if (nrow(pressure_dt) == 0L) stop("No pressure records were read.")

# ---------------------------------------------------------------------------
# 4) Deduplicate across files at the timestamp level (optional sensor pref)
# ---------------------------------------------------------------------------
# Attach mtime for tie-breaking
fi <- file.info(pressure_dt$file_path)
pressure_dt[, file_mtime := fi$mtime[match(file_path, rownames(fi))]]

pressure_dt[, sensor_rank := if (!is.null(preferred_sensor_regex)) {
  fifelse(grepl(preferred_sensor_regex, pressure_sensor_file, ignore.case = TRUE), 1L, 2L)
} else 1L]

setorder(pressure_dt, site_code, datetime_utc, sensor_rank, -file_mtime)
pressure_dt <- pressure_dt[, .SD[1L], by = .(site_code, datetime_utc)]
pressure_dt[, c("sensor_rank", "file_mtime") := NULL]

setkey(pressure_dt, site_code, datetime_utc)

# ---------------------------------------------------------------------------
# 5) Merge onto main data (sf_recombined_new)
# ---------------------------------------------------------------------------

# make sure sf_recombined_new is loaded in 
setDT(sf_recombined_new)
attr(sf_recombined_new$datetime_utc, "tzone") <- "UTC"

# Exact join
sf_exact <- copy(sf_recombined_new)
sf_exact[pressure_dt, `:=`(
  press_psir           = i.press_psir,
  depth_m              = i.depth_m,
  pressure_sensor_file = i.pressure_sensor_file
), on = .(site_code, datetime_utc)]

# Nearest join within tolerance
tolerance_secs <- 60
# Ensure sf_nearest has a row_id to map back
sf_nearest <- copy(sf_recombined_new)
setDT(sf_nearest)
sf_nearest[, row_id := .I]
attr(sf_nearest$datetime_utc, "tzone") <- "UTC"

# Join and explicitly return both time columns
nearest_map <- pressure_dt[
  sf_nearest,
  on = .(site_code, datetime_utc),
  roll = "nearest",
  .(
    row_id         = i.row_id,             # from sf
    site_code      = site_code,            # from pressure_dt
    sf_time        = i.datetime_utc,       # timestamp from sf
    pressure_time  = datetime_utc,         # matched pressure timestamp
    press_psir,
    depth_m,
    pressure_sensor_file
  )
]

# Compute time difference and apply tolerance
nearest_map[, time_diff_sec := abs(as.numeric(pressure_time - sf_time))]
nearest_map[time_diff_sec > tolerance_secs, `:=`(
  press_psir            = NA_real_,
  depth_m               = NA_real_,
  pressure_sensor_file  = NA_character_,
  pressure_time         = as.POSIXct(NA, tz = "UTC")
)]

# Attach results back to sf_nearest by row_id
setkey(nearest_map, row_id)
setkey(sf_nearest, row_id)
sf_nearest[nearest_map, `:=`(
  press_psir            = i.press_psir,
  depth_m               = i.depth_m,
  pressure_sensor_file  = i.pressure_sensor_file,
  matched_pressure_time = i.pressure_time,
  pressure_time_diff_s  = i.time_diff_sec
), on = .(row_id)]
sf_nearest[, row_id := NULL]

# Diagnostics
cat("Exact join non-NA pressure rows: ",
    sum(!is.na(sf_exact$press_psir)), "of", nrow(sf_exact), "\n")
cat("Nearest join non-NA pressure rows: ",
    sum(!is.na(sf_nearest$press_psir)), "of", nrow(sf_nearest), "\n")

# Optional: quick file coverage check by site
print(pressure_dt[, .(min_time = min(datetime_utc), max_time = max(datetime_utc), n = .N), by = site_code][order(site_code)])

# Final result:
sf_recombined_new_w_Press <- sf_nearest %>%   # or sf_exact if times align perfectly
  select(-c(press_psir, pressure_sensor_file, matched_pressure_time, pressure_time_diff_s)) %>% 
  relocate(depth_m, .before = 10)

