# Manual review of San Francisco NEP data by S. Pacella
# Last update: 3/18/2026
# Dependencies: requires 'sf_data' created by running qaqc_NEP_SanFrancisco pulled from github 3/18/26

# Break data up into CMS and EOS stations specifically
# Assumes sf_data is already loaded in the workspace.

# Set Working Directory: Adjust to local 
setwd('C:/Users/spacella/OneDrive - Environmental Protection Agency (EPA)/NEP OA standards analysis')

# -------------------------------
# Make a working copy of the data
# -------------------------------
#sf_nep_copy <- sf_recombined

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

# -------------------------------
# 0) Helpers
# -------------------------------

# Create output directory for figures
fig_dir <- "figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Unified save function
save_plot <- function(plot_obj, filename, width = 9, height = 6.5, dpi = 300) {
  ggsave(filename = file.path(fig_dir, filename),
         plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
}

# Publication theme
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}

# Compact point style for dense time series
pt_size <- 0.6
pt_alpha <- 0.6


# 
# # --------------------------------------------
# # 2) Convert ISO 8601 "Z" timestamps to POSIXct (UTC)
# #    - Handles timestamps with and without fractional seconds
# #    - Normalizes fractional seconds to exactly 3 digits (milliseconds)
# # --------------------------------------------
# ts <- trimws(as.character(sf_nep_copy$datetime_utc))
# dt <- as.POSIXct(rep(NA_character_, length(ts)), tz = "UTC")
# valid <- !is.na(ts) & nzchar(ts)
# 
# # Case 1: No fractional seconds
# noFrac <- valid & !grepl("\\.", ts)
# if (any(noFrac)) {
#   dt[noFrac] <- as.POSIXct(ts[noFrac], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
# }
# 
# # Case 2: With fractional seconds -> normalize to 3 fractional digits
# withFrac <- valid & grepl("\\.", ts)
# if (any(withFrac)) {
#   s <- ts[withFrac]
#   normalize_iso_ms <- function(s1) {
#     s1 <- trimws(s1)
#     if (is.na(s1) || s1 == "") return(NA_character_)
#     s_noZ <- sub("Z$", "", s1)
#     split_pos <- regexpr("\\.", s_noZ)
#     if (split_pos <= 0) return(paste0(s_noZ, "Z"))
#     left <- substr(s_noZ, 1, split_pos - 1)
#     frac <- substr(s_noZ, split_pos + 1, nchar(s_noZ))
#     # Keep only digits in fractional part
#     frac <- gsub("[^0-9].*$", "", frac)
#     if (nchar(frac) >= 3) {
#       frac3 <- substr(frac, 1, 3)
#     } else {
#       frac3 <- paste0(frac, paste0(rep("0", 3 - nchar(frac)), collapse = ""))
#     }
#     paste0(left, ".", frac3, "Z")
#   }
#   s_norm <- vapply(s, normalize_iso_ms, FUN.VALUE = character(1))
#   dt[withFrac] <- as.POSIXct(s_norm, format = "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
# }

# # Assign parsed timestamps
# sf_nep_copy$datetime_utc <- dt

# --------------------------------------------
# 3) Split stations to ease review (CMA and EOS)
# --------------------------------------------
#sf_nep_cma <- sf_nep_copy %>% filter(site_code == "CMA")
#sf_nep_eos <- sf_nep_copy %>% filter(site_code == "EOS")

# -------------------------
# 4) Add manual QC column defaults
# -------------------------
sf_nep_cma <- sf_nep_cma %>% mutate(flag_manual = flag_max)
sf_nep_eos <- sf_nep_eos %>% mutate(flag_manual = flag_max)
sf_nep_copy <- sf_recombined
# -------------------------
# 5) Exploratory Figures (ggplot, written to files)
# -------------------------

# Common scales and labels
scale_flags <- scale_color_brewer(palette = "Dark2", na.value = "grey70", name = "Flag")
scale_year  <- scale_color_viridis_d(option = "plasma", end = 0.9, name = "Year")

# Convenience: labellers
time_x <- scale_x_datetime(date_labels = "%Y-%m", date_breaks = "6 months", timezone = "UTC")

# 5.1 pH overview (2x2): CMA/EOS colored by flags, and with flags_2026==1 only
# p_cma_flags <- ggplot(sf_nep_cma %>% filter(site_code == "CMA"),
#                       aes(x = datetime_utc, y = ph, color = factor(flag_max))) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   labs(title = "CMA pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_cma_flags_zoom <- ggplot(sf_nep_cma %>% filter(site_code == "CMA"),
#                       aes(x = datetime_utc, y = ph, color = factor(flag_max))) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   coord_cartesian(ylim = c(6, 9)) +
#   labs(title = "CMA pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_cma_flags_zoom_pass <- ggplot(sf_nep_cma %>% filter(site_code == "CMA",flag_max == 1),
#                            aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   coord_cartesian(ylim = c(6, 9)) +
#   labs(title = "CMA pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# save_plot(p_cma_flags,
#           "cma_ph_flag_manual.png", width = 12, height = 9, dpi = 300)
# 
# save_plot((p_cma_flags) / (p_cma_flags_zoom),
#           "cma_ph_flag_manual_wzoom.png", width = 12, height = 9, dpi = 300)
# save_plot((p_cma_flags) / (p_cma_flags_zoom) / (p_cma_flags_zoom_pass),
#           "cma_ph_flag_manual_wzoom_pass.png", width = 12, height = 9, dpi = 300)

# p_cma_2026 <- ggplot(sf_nep_copy %>% filter(site_code == "CMA", flag_manual == 1),
#                      aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "CMA pH (flags_2026 == 1)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_eos_flags <- ggplot(sf_nep_copy %>% filter(site_code == "EOS"),
#                       aes(x = datetime_utc, y = ph, color = factor(flags))) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   labs(title = "EOS pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_eos_2026 <- ggplot(sf_nep_copy %>% filter(site_code == "EOS", flags_2026 == 1),
#                      aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "EOS pH (flags_2026 == 1)", x = NULL, y = "pH") +
#   theme_pub()
# 
# save_plot((p_cma_flags + p_cma_2026) / (p_eos_flags + p_eos_2026),
#           "01_ph_overview_2x2.png", width = 12, height = 9, dpi = 300)
# 
# # 5.2 CMA pH overlay: flags_2026==1 vs flags==1
# cma_ph_overlay <- bind_rows(
#   sf_nep_copy %>%
#     filter(site_code == "CMA", flags_2026 == 1) %>%
#     mutate(Filter = "flags_2026 == 1"),
#   sf_nep_copy %>%
#     filter(site_code == "CMA", flags == 1) %>%
#     mutate(Filter = "flags == 1")
# )
# 
# p_cma_ph_overlay <- ggplot(cma_ph_overlay,
#                            aes(x = datetime_utc, y = ph, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "CMA pH: flags_2026 vs flags", x = NULL, y = "pH") +
#   theme_pub()
# save_plot(p_cma_ph_overlay, "02_cma_pH_overlay.png")
# 
# # 5.3 CMA Salinity overlay: flags_2026==1 vs flags==1
# cma_sal_overlay <- bind_rows(
#   sf_nep_copy %>%
#     filter(site_code == "CMA", flags_2026 == 1) %>%
#     mutate(Filter = "flags_2026 == 1"),
#   sf_nep_copy %>%
#     filter(site_code == "CMA", flags == 1) %>%
#     mutate(Filter = "flags == 1")
# )
# 
# p_cma_sal_overlay <- ggplot(cma_sal_overlay,
#                             aes(x = datetime_utc, y = sal_ppt, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "CMA Salinity: flags_2026 vs flags", x = NULL, y = "Salinity (ppt)") +
#   theme_pub()
# save_plot(p_cma_sal_overlay, "03_cma_salinity_overlay.png")

# -------------------------
# 6) Manual flags for pH: CMA
# -------------------------
dt_cma <- sf_nep_cma$datetime_utc

# 6.1 Flag: 2015-05-12 18:43:00 to 2015-05-12 18:44:00 (UTC)
tStart <- as.POSIXct("2015-05-12 18:43:00", tz = "UTC")
tEnd   <- as.POSIXct("2015-05-12 18:44:00", tz = "UTC")
mask <- dt_cma >= tStart & dt_cma <= tEnd
sf_nep_cma$flag_manual[mask] <- 2

# 6.2 CMA Salinity scatter (subset, flags_manual vs flags)
# cma_sal_overlay_sub <- bind_rows(
#   sf_nep_cma %>% filter(flags_2026 == 1) %>% mutate(Filter = "flags_manual == 1"),
#   sf_nep_cma %>% filter(flags == 1)       %>% mutate(Filter = "flags == 1")
# )
# p_cma_sal_overlay_sub <- ggplot(cma_sal_overlay_sub,
#                                 aes(x = datetime_utc, y = sal_ppt, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "CMA Salinity (subset): flags_2026 vs flags", x = NULL, y = "Salinity (ppt)") +
#   theme_pub()
# save_plot(p_cma_sal_overlay_sub, "04_cma_salinity_overlay_subset.png")
# 
# # 6.3 CMA pH scatter (subset, flags_2026 vs flags)
# cma_ph_overlay_sub <- bind_rows(
#   sf_nep_cma %>% filter(flags_2026 == 1) %>% mutate(Filter = "flags_2026 == 1"),
#   sf_nep_cma %>% filter(flags == 1)       %>% mutate(Filter = "flags == 1")
# )
# p_cma_ph_overlay_sub <- ggplot(cma_ph_overlay_sub,
#                                aes(x = datetime_utc, y = ph, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "CMA raw pH (subset): flags_2026 vs flags", x = NULL, y = "pH") +
#   theme_pub()
# save_plot(p_cma_ph_overlay_sub, "05_cma_pH_overlay_subset.png")

# 6.4 Erratic CMA pH: Jun-Aug 2017 (inspect)
tStart <- as.POSIXct("2017-06-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2017-08-30 00:00:00", tz = "UTC")
mask_2017 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd

# p_cma_2017_ts <- ggplot(sf_nep_cma %>% filter(mask_2017),
#                         aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "CMA raw pH (Jun–Aug 2017)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_cma_2017_dovspH <- ggplot(sf_nep_cma %>% filter(mask_2017),
#                             aes(x = do_mgl, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   coord_cartesian(xlim = c(0, 15)) +
#   labs(title = "CMA: DO vs pH (Jun–Aug 2017)", x = "DO (mg/L)", y = "pH") +
#   theme_pub()
# 
# save_plot(p_cma_2017_ts / p_cma_2017_dovspH, "06_cma_2017_inspection.png", width = 9, height = 10)

# 6.5 Spikey pH: Jul 28–31 2021 -> flag suspect
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

tStart <- as.POSIXct("2022-10-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2024-08-01 00:00:00", tz = "UTC")
mask_2022 <- sf_nep_cma$datetime_utc >= tStart & sf_nep_cma$datetime_utc <= tEnd
sf_nep_cma$flag_manual[mask_2022] <- 2

# p_cma_flagmanual_zoom_pass <- ggplot(sf_nep_cma %>% filter(site_code == "CMA",flag_manual == 1),
#                                 aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   coord_cartesian(ylim = c(6, 9)) +
#   labs(title = "CMA pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_cma_flags_zoom_pass <- ggplot(sf_nep_cma %>% filter(site_code == "CMA",flags == 1),
#                                      aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_flags + time_x +
#   coord_cartesian(ylim = c(6, 9)) +
#   labs(title = "CMA pH (colored by flags)", x = NULL, y = "pH") +
#   theme_pub()
# 
# save_plot(p_cma_flagmanual_zoom_pass,
#           "cma_ph_flag_manual_zoom_pass.png", width = 12, height = 9, dpi = 300)
# 
# save_plot((p_cma_flagmanual_zoom_pass) /(p_cma_flags_zoom_pass),
#           "cma_ph_flagmanual_flag_zoom_pass.pdf", width = 12, height = 9, dpi = 300)
# 
# 
# save_plot((p_cma_flags) / (p_cma_flags_zoom),
#           "cma_ph_flag_manual_wzoom.png", width = 12, height = 9, dpi = 300)
# save_plot((p_cma_flags) / (p_cma_flags_zoom) / (p_cma_flags_zoom_pass),
#           "cma_ph_flag_manual_wzoom_pass.png", width = 12, height = 9, dpi = 300)



# -------------------------
# 7) EOS station
# -------------------------
dt_eos <- sf_nep_eos$datetime_utc

# 7.1 EOS Salinity overlay: flags_2026 vs flags
# eos_sal_overlay <- bind_rows(
#   sf_nep_eos %>% filter(flags_2026 == 1) %>% mutate(Filter = "flags_2026 == 1"),
#   sf_nep_eos %>% filter(flags == 1)       %>% mutate(Filter = "flags == 1")
# )
# 
# p_eos_sal_overlay <- ggplot(eos_sal_overlay,
#                             aes(x = datetime_utc, y = sal_ppt, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "EOS Salinity: flags_2026 vs flags", x = NULL, y = "Salinity (ppt)") +
#   theme_pub()
# save_plot(p_eos_sal_overlay, "07_eos_salinity_overlay.png")
# 
# # 7.2 EOS pH overlay: flags_2026 vs flags
# eos_ph_overlay <- bind_rows(
#   sf_nep_eos %>% filter(flags_2026 == 1) %>% mutate(Filter = "flags_2026 == 1"),
#   sf_nep_eos %>% filter(flags == 1)       %>% mutate(Filter = "flags == 1")
# )
# 
# p_eos_ph_overlay <- ggplot(eos_ph_overlay,
#                            aes(x = datetime_utc, y = ph, color = Filter)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_color_manual(values = c("flags_2026 == 1" = "black", "flags == 1" = "red")) +
#   time_x +
#   labs(title = "EOS pH: flags_2026 vs flags", x = NULL, y = "pH") +
#   theme_pub()
# save_plot(p_eos_ph_overlay, "08_eos_pH_overlay.png")
# 
# # 7.3 EOS: pH and DO time series (flags_2026 == 1)
# eos_2026 <- sf_nep_eos %>% filter(flags_2026 == 1)
# 
# p_eos_pH_ts <- ggplot(eos_2026, aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "EOS pH (flags_2026 == 1)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_eos_do_ts <- ggplot(eos_2026, aes(x = datetime_utc, y = do_mgl)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "steelblue4") +
#   time_x +
#   labs(title = "EOS DO (flags_2026 == 1)", x = NULL, y = "DO (mg/L)") +
#   theme_pub()
# 
# save_plot(p_eos_pH_ts / p_eos_do_ts, "09_eos_pH_DO_timeseries.png", width = 9, height = 10)
# 
# # 7.4 EOS: DO vs pH colored by year (flags_2026 == 1)
# eos_2026_year <- eos_2026 %>%
#   mutate(Year = factor(year(datetime_utc)))
# 
# p_eos_dovspH_year <- ggplot(eos_2026_year, aes(x = do_mgl, y = ph, color = Year)) +
#   geom_point(size = pt_size, alpha = pt_alpha) +
#   scale_year +
#   labs(title = "EOS: DO vs pH (colored by Year, flags_2026 == 1)", x = "DO (mg/L)", y = "pH") +
#   theme_pub()
# 
# save_plot(p_eos_dovspH_year, "10_eos_DO_vs_pH_byYear.png")

# 7.5 EOS: Flag suspect for 2015-01-01 to 2016-02-25
tStart <- as.POSIXct("2015-01-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2016-02-25 00:00:00", tz = "UTC")
mask <- dt_eos >= tStart & dt_eos <= tEnd
sf_nep_eos$flag_manual[mask] <- 2

# 7.6 EOS: Large change around Feb 2018 (inspect window)
tStart <- as.POSIXct("2017-11-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2018-05-01 00:00:00", tz = "UTC")
mask_2018 <- sf_nep_eos$datetime_utc >= tStart & sf_nep_eos$datetime_utc <= tEnd

# p_eos_2018_pH <- ggplot(sf_nep_eos %>% filter(mask_2018),
#                         aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "EOS pH (Nov 2017–May 2018)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_eos_2018_DO <- ggplot(sf_nep_eos %>% filter(mask_2018),
#                         aes(x = datetime_utc, y = do_mgl)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "steelblue4") +
#   time_x +
#   labs(title = "EOS DO (Nov 2017–May 2018)", x = NULL, y = "DO (mg/L)") +
#   theme_pub()
# 
# p_eos_2018_S <- ggplot(sf_nep_eos %>% filter(mask_2018),
#                        aes(x = datetime_utc, y = sal_ppt)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "darkorange3") +
#   time_x +
#   labs(title = "EOS Salinity (Nov 2017–May 2018)", x = NULL, y = "Salinity (ppt)") +
#   theme_pub()
# 
# save_plot(p_eos_2018_pH / p_eos_2018_DO / p_eos_2018_S,
#           "11_eos_2018_inspection.png", width = 9, height = 12)

# 7.7 EOS: Drop in pH July 2019 (inspect window)
tStart <- as.POSIXct("2019-06-01 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2019-10-01 00:00:00", tz = "UTC")
mask_2019 <- sf_nep_eos$datetime_utc >= tStart & sf_nep_eos$datetime_utc <= tEnd

# p_eos_2019_pH <- ggplot(sf_nep_eos %>% filter(mask_2019),
#                         aes(x = datetime_utc, y = ph)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "black") +
#   time_x +
#   labs(title = "EOS pH (Jun–Oct 2019)", x = NULL, y = "pH") +
#   theme_pub()
# 
# p_eos_2019_DO <- ggplot(sf_nep_eos %>% filter(mask_2019),
#                         aes(x = datetime_utc, y = do_mgl)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "steelblue4") +
#   time_x +
#   coord_cartesian(ylim = c(0, 15)) +
#   labs(title = "EOS DO (Jun–Oct 2019)", x = NULL, y = "DO (mg/L)") +
#   theme_pub()
# 
# p_eos_2019_S <- ggplot(sf_nep_eos %>% filter(mask_2019),
#                        aes(x = datetime_utc, y = sal_ppt)) +
#   geom_point(size = pt_size, alpha = pt_alpha, color = "darkorange3") +
#   time_x +
#   coord_cartesian(ylim = c(0, 40)) +
#   labs(title = "EOS Salinity (Jun–Oct 2019)", x = NULL, y = "Salinity (ppt)") +
#   theme_pub()
# 
# save_plot(p_eos_2019_pH / p_eos_2019_DO / p_eos_2019_S,
#           "12_eos_2019_inspection.png", width = 9, height = 12)

# EOS: Flag suspect pH data in 2019
tStart <- as.POSIXct("2019-07-04 00:00:00", tz = "UTC")
tEnd   <- as.POSIXct("2019-08-10 00:00:00", tz = "UTC")
mask <- dt_eos >= tStart & dt_eos <= tEnd
sf_nep_eos$flag_manual[mask] <- 2

tStart <- as.POSIXct("2019-06-22 17:30:00", tz = "UTC")
tEnd   <- as.POSIXct("2019-06-22 21:10:00", tz = "UTC")
mask <- dt_eos >= tStart & dt_eos <= tEnd
sf_nep_eos$flag_manual[mask] <- 2

# -------------------------------------------------
# 8) Recombine manual flags back into working copy
# -------------------------------------------------
sf_nep_copy <- sf_nep_copy %>%
  mutate(flag_manual = NA_integer_)

sf_nep_copy$flag_manual[sf_nep_copy$site_code == "CMA"] <- sf_nep_cma$flag_manual
sf_nep_copy$flag_manual[sf_nep_copy$site_code == "EOS"] <- sf_nep_eos$flag_manual

# -------------------------------------------------
# Outputs:
# - Figures are saved in the "figs" directory.
# - sf_nep_copy: working copy with parsed datetime_utc and flag_manual.
# - sf_nep_cma, sf_nep_eos: station subsets with updated flag_manual.
# -------------------------------------------------


# ------------------------------------------------------------
# Additional figures: visualize how flag_manual differs from flags_2026
# Append this block after recombining flag_manual into sf_nep_copy
# Requires packages and helpers from the earlier script (ggplot2, dplyr, lubridate,
# patchwork, theme_pub, save_plot, time_x, fig_dir).
# ------------------------------------------------------------

# Prepare comparison dataset (CMA and EOS)
flag_cmp <- sf_nep_copy %>%
  filter(site_code %in% c("CMA", "EOS")) %>%
  mutate(
    flag_2026   = flags_2026,
    flag_manual = flag_manual,
    change_bin = case_when(
      is.na(flag_2026) | is.na(flag_manual) ~ "Missing",
      flag_manual == flag_2026 ~ "No change",
      TRUE ~ "Changed"
    ),
    change_label = case_when(
      is.na(flag_2026) | is.na(flag_manual) ~ NA_character_,
      flag_manual == flag_2026 ~ "No change",
      TRUE ~ paste0(flag_2026, "\u2192", flag_manual)  # e.g., "1→2"
    )
  )

# Palette for change_bin
change_cols <- c("No change" = "grey60", "Changed" = "#D55E00", "Missing" = "grey85")

# 1) Bar chart: counts by station of Changed / No change / Missing
p_changes_by_site <- ggplot(flag_cmp %>%
                              mutate(change_bin = factor(change_bin, levels = c("No change", "Changed", "Missing"))) %>%
                              count(site_code, change_bin),
                            aes(x = site_code, y = n, fill = change_bin)) +
  geom_col(width = 0.7, color = "white") +
  scale_fill_manual(values = change_cols, name = "Status") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Manual flags vs. flags_2026: Status by Station",
       x = "Station", y = "Number of records") +
  theme_pub()
save_plot(p_changes_by_site, "13_flag_changes_by_station.png", width = 7.5, height = 5.5, dpi = 300)

# 2) Confusion heatmap: flags_2026 vs flag_manual (counts), faceted by station
confusion_counts <- flag_cmp %>%
  filter(!is.na(flag_2026), !is.na(flag_manual)) %>%
  count(site_code, flag_2026 = factor(flag_2026), flag_manual = factor(flag_manual))

p_confusion <- ggplot(confusion_counts,
                      aes(x = flag_2026, y = flag_manual, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(n)), size = 3.3, color = "black") +
  scale_fill_viridis_c(option = "magma", name = "Count") +
  facet_wrap(~ site_code, ncol = 2) +
  labs(title = "flags_2026 vs flag_manual (Counts)",
       x = "flags_2026", y = "flag_manual") +
  theme_pub(base_size = 11) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"))
save_plot(p_confusion, "14_flag_confusion_heatmap.png", width = 9, height = 6.5, dpi = 300)

# 3) pH time series with changed points highlighted; faceted by station
#    Grey points = all records; colored points = where flag changed
p_ts_changed <- ggplot(flag_cmp, aes(x = datetime_utc, y = ph)) +
  geom_point(size = 0.5, alpha = 0.35, color = "grey60", na.rm = TRUE) +
  geom_point(data = flag_cmp %>% filter(change_bin == "Changed"),
             aes(color = factor(flag_manual)),
             size = 0.9, alpha = 0.8, na.rm = TRUE) +
  scale_color_brewer(palette = "Set1", name = "flag_manual") +
  time_x +
  facet_wrap(~ site_code, ncol = 1, scales = "free_y") +
  labs(title = "pH time series (changed flags highlighted)",
       x = NULL, y = "pH") +
  theme_pub()
save_plot(p_ts_changed, "15_pH_timeseries_changed_flags.png", width = 10, height = 7.5, dpi = 300)

# 4) Year-month heatmap of changed points by station (temporal footprint)
changes_monthly <- flag_cmp %>%
  filter(change_bin == "Changed") %>%
  mutate(YearMonth = lubridate::floor_date(datetime_utc, unit = "month")) %>%
  count(site_code, YearMonth)

# Ensure complete months range for better visualization (optional)
if (nrow(changes_monthly) > 0) {
  range_months <- seq(min(changes_monthly$YearMonth, na.rm = TRUE),
                      max(changes_monthly$YearMonth, na.rm = TRUE),
                      by = "1 month")
  changes_monthly <- changes_monthly %>%
    tidyr::complete(site_code, YearMonth = range_months, fill = list(n = 0))
}

p_changes_calendar <- ggplot(changes_monthly,
                             aes(x = YearMonth, y = site_code, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Changed\ncount") +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(title = "Changed manual flags by month",
       x = "Year–Month", y = "Station") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_changes_calendar, "16_changed_flags_by_month.png", width = 10, height = 4.8, dpi = 300)

# 5) From-to change categories (e.g., "1→2") counts by station
from_to_counts <- flag_cmp %>%
  filter(!is.na(change_label), change_label != "No change") %>%
  count(site_code, change_label) %>%
  arrange(site_code, desc(n))

p_from_to <- ggplot(from_to_counts,
                    aes(x = reorder(change_label, -n), y = n, fill = site_code)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_brewer(palette = "Set2", name = "Station") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Manual flag changes (from → to)",
       x = "Change category", y = "Count") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p_from_to, "17_flag_change_categories.png", width = 9.5, height = 5.5, dpi = 300)

# ------------------------------------------------------------
# New figure: Overlay two pH time series
# 1) pH where flags_2026 == 1
# 2) pH where flag_manual == 1
# Faceted by station for clear comparison.
# Append this block after flag_manual has been recombined into sf_nep_copy.
# Relies on theme_pub(), save_plot(), and time_x from earlier script.
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Fallback point aesthetics (if not already defined)
if (!exists("pt_size"))  pt_size  <- 0.6
if (!exists("pt_alpha")) pt_alpha <- 0.6

# Build overlay dataset
pH_overlay <- bind_rows(
  sf_nep_copy %>%
    filter(flags_2026 == 1) %>%
    mutate(Series = "flags_2026 == 1"),
  sf_nep_copy %>%
    filter(flag_manual == 1) %>%
    mutate(Series = "flag_manual == 1")
) %>%
  filter(!is.na(datetime_utc), !is.na(ph)) %>%
  mutate(Series = factor(Series, levels = c("flags_2026 == 1", "flag_manual == 1")))

# Color palette for clarity and accessibility
series_cols <- c("flags_2026 == 1" = "#333333",  # near-black
                 "flag_manual == 1" = "#D55E00") # orange/red

p_ph_overlay <- ggplot(pH_overlay, aes(x = datetime_utc, y = ph, color = Series)) +
  geom_point(size = pt_size, alpha = pt_alpha, na.rm = TRUE) +
  scale_color_manual(values = series_cols, name = "Series") +
  time_x +
  facet_wrap(~ site_code, ncol = 1, scales = "free_y") +
  labs(
    title = "pH time series: flags_2026 == 1 vs flag_manual == 1",
    x = NULL, y = "pH"
  ) +
  theme_pub() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# Save figure
save_plot(p_ph_overlay, "18_pH_flags2026_vs_manual_overlay.png", width = 10, height = 7.5, dpi = 300)

# ------------------------------------------------------------
# Generated files:
# 13_flag_changes_by_station.png
# 14_flag_confusion_heatmap.png
# 15_pH_timeseries_changed_flags.png
# 16_changed_flags_by_month.png
# 17_flag_change_categories.png
# ------------------------------------------------------------