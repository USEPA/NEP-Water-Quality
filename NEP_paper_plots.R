setwd('C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/Figures/NEP_paper')
# Packages
# install.packages(c("tidyverse", "cowplot"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(cowplot)

data_list = nep_filtered_data
data_list_unfiltered = nep_unfiltered_data

# 5x3 NEP comparison plots of seasonal averages: pH vs DO & temperature vs salinity
# These plots consist of 15 panels (one for each NEP) and display the monthly median (solid line) and 10-90th percentile ranges (shaded areas) across 12 months
# 5x3 NEP plots of pH/DO and Sal/Temp "Original"  ###############

# Single-letter month labels for the x-axis
month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")

# Summarize one variable to monthly 10th/50th/90th for a single NEP df
# - Uses unordered month factor + complete() to cover all months
summarize_monthly <- function(df,
                              value_col,
                              datetime_col = "datetime_utc",
                              equal_site_weighting = FALSE,     # set TRUE to weight sites equally
                              site_id_cols = c("site_code", "station_name")) {
  
  if (!datetime_col %in% names(df)) {
    stop("'", datetime_col, "' column not found in the data frame.")
  }
  if (!inherits(df[[datetime_col]], "POSIXt")) {
    stop("'", datetime_col, "' must be POSIXct/POSIXlt. Convert before calling this function.")
  }
  
  if (!value_col %in% names(df)) {
    # Variable missing: return an all-month NA summary (keeps plotting consistent)
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  keep_cols <- c(datetime_col, value_col, site_id_cols[site_id_cols %in% names(df)])
  
  x <- df %>%
    select(all_of(keep_cols)) %>%
    filter(!is.na(.data[[value_col]])) %>%
    mutate(
      month = factor(lubridate::month(.data[[datetime_col]]),
                     levels = 1:12, labels = month.abb, ordered = FALSE)
    )
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  if (equal_site_weighting && all(site_id_cols %in% names(x))) {
    site_month <- x %>%
      group_by(across(all_of(site_id_cols)), month) %>%
      summarize(site_month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
    
    out <- site_month %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(site_month_median, 0.10, na.rm = TRUE, names = FALSE),
        median = median(site_month_median, na.rm = TRUE),
        p90    = quantile(site_month_median, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    out <- x %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(.data[[value_col]], 0.10, na.rm = TRUE, names = FALSE),
        median = median(.data[[value_col]], na.rm = TRUE),
        p90    = quantile(.data[[value_col]], 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  
  out %>%
    ungroup() %>%
    complete(
      month = factor(month.abb, levels = month.abb, ordered = FALSE),
      fill = list(p10 = NA_real_, median = NA_real_, p90 = NA_real_)
    ) %>%
    arrange(month)
}

# Build a dual-axis plot from two monthly summaries
# - Adds layers only for series with data
# - Puts an in-panel NEP tag centered at the top
# - Supports fixed axis limits and breaks for standardized scales
build_dual_axis_plot <- function(left_sum, right_sum,
                                 left_label, right_label,
                                 left_color = "#D55E00",
                                 right_color = "#0072B2",
                                 add_axis_titles = FALSE,
                                 panel_tag = NULL,
                                 left_limits = NULL,     # c(min, max) on left-axis units
                                 right_limits = NULL,    # c(min, max) on right-axis units
                                 left_breaks = NULL,
                                 right_breaks = NULL) {
  
  left_has  <- any(is.finite(left_sum$median))
  right_has <- any(is.finite(right_sum$median))
  
  # Use fixed ranges if supplied; otherwise compute from data
  # Map right-axis values to left-axis space via y_plot = a * y_right + b
  if (!is.null(left_limits) && !is.null(right_limits)) {
    left_min  <- left_limits[1]; left_max  <- left_limits[2]
    right_min <- right_limits[1]; right_max <- right_limits[2]
  } else {
    left_vals  <- c(left_sum$p10,  left_sum$median,  left_sum$p90)
    right_vals <- c(right_sum$p10, right_sum$median, right_sum$p90)
    left_min <- suppressWarnings(min(left_vals,  na.rm = TRUE)); left_max <- suppressWarnings(max(left_vals,  na.rm = TRUE))
    right_min <- suppressWarnings(min(right_vals, na.rm = TRUE)); right_max <- suppressWarnings(max(right_vals, na.rm = TRUE))
    if (!is.finite(left_min))  { left_min <- 0; left_max <- 1 }
    if (!is.finite(right_min)) { right_min <- 0; right_max <- 1 }
  }
  
  if (!is.finite(left_min) || !is.finite(left_max) || left_min == left_max ||
      !is.finite(right_min) || !is.finite(right_max) || right_min == right_max) {
    a <- 1; b <- 0
  } else {
    a <- (left_max - left_min) / (right_max - right_min)
    b <- left_min - a * right_min
  }
  
  # Transform right series to left-axis space
  right_plot <- right_sum %>%
    mutate(
      median_t = a * median + b,
      p10_t    = a * p10 + b,
      p90_t    = a * p90 + b
    )
  
  # Final plot
  p <- ggplot()
  
  # Left variable ribbon + line (if present)
  if (left_has) {
    p <- p +
      geom_ribbon(data = left_sum, aes(x = month, ymin = p10, ymax = p90, group = 1),
                  fill = left_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = left_sum, aes(x = month, y = median, group = 1),
                color = left_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  # Right variable ribbon + line (if present)
  if (right_has) {
    p <- p +
      geom_ribbon(data = right_plot, aes(x = month, ymin = p10_t, ymax = p90_t, group = 1),
                  fill = right_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = right_plot, aes(x = month, y = median_t, group = 1),
                color = right_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  # Axis titles: suppressed per-panel; added globally later
  y_left_title  <- if (add_axis_titles) left_label  else NULL
  y_right_title <- if (add_axis_titles) right_label else NULL
  
  # If fixed limits are supplied, use them; otherwise let ggplot pick
  p +
    scale_x_discrete(labels = month_letters, drop = FALSE) +
    scale_y_continuous(
      name   = y_left_title,
      limits = left_limits,
      breaks = left_breaks,
      sec.axis = sec_axis(~ (. - b) / a, name = y_right_title, breaks = right_breaks)
    ) +
    labs(x = NULL, title = NULL, tag = panel_tag) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y.left  = if (add_axis_titles) element_text(color = left_color)  else element_blank(),
      axis.title.y.right = if (add_axis_titles) element_text(color = right_color) else element_blank(),
      axis.text.y.left   = element_text(color = left_color),
      axis.text.y.right  = element_text(color = right_color),
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      plot.tag = element_text(size = 9, face = "bold", hjust = 0.5, vjust = 1),
      plot.tag.position = c(0.5, 0.98),   # centered at top
      plot.margin = margin(6, 4, 3, 4)
    )
}

# For a single NEP df, make the two requested plots
make_nep_plots <- function(df,
                           nep_name = NULL,
                           datetime_col = "datetime_utc",
                           equal_site_weighting = FALSE) {
  temp_sum <- summarize_monthly(df, "temp_c", datetime_col, equal_site_weighting)
  sal_sum  <- summarize_monthly(df, "sal_ppt", datetime_col, equal_site_weighting)
  ph_sum   <- summarize_monthly(df, "ph_T", datetime_col, equal_site_weighting)
  do_sum   <- summarize_monthly(df, "do_mgl", datetime_col, equal_site_weighting)
  
  # Temperature (left) vs Salinity (right) with standardized ranges
  p1 <- build_dual_axis_plot(
    left_sum  = temp_sum,
    right_sum = sal_sum,
    left_label  = "Temperature (°C)",
    right_label = "Salinity (ppt)",
    left_color  = "#D55E00",   # orange
    right_color = "#0072B2",   # blue
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(0, 35),
    right_limits = c(0, 40),
    left_breaks  = seq(0, 35, 5),
    right_breaks = seq(0, 40, 10)
  )
  
  # pH (left) vs Dissolved Oxygen (right) with standardized ranges and new colors
  p2 <- build_dual_axis_plot(
    left_sum  = ph_sum,
    right_sum = do_sum,
    left_label  = "pH (unitless)",
    right_label = "Dissolved Oxygen (mg/L)",
    left_color  = "#6A3D9A",   # purple for pH
    right_color = "#1B9E77",   # green for DO
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(6, 9),
    right_limits = c(0, 15),
    left_breaks  = 6:9,
    right_breaks = seq(0, 15, 5)
  )
  
  list(temp_sal = p1, ph_do = p2)
}

# Build plots for each NEP (assumes nep_filtered_data is a named list)
plots_by_nep <- purrr::imap(
  nep_filtered_data,
  ~ make_nep_plots(.x, nep_name = .y, datetime_col = "datetime_utc", equal_site_weighting = FALSE)
)

# Extract the two sets of plots (one per NEP)
temp_sal_plots <- purrr::map(plots_by_nep, "temp_sal")
ph_do_plots    <- purrr::map(plots_by_nep, "ph_do")

# Arrange into 5x3 grids with alignment
grid_temp_sal <- cowplot::plot_grid(plotlist = temp_sal_plots, ncol = 5, align = "hv")
grid_ph_do    <- cowplot::plot_grid(plotlist = ph_do_plots,  ncol = 5, align = "hv")

# Compose final figures with reserved margins to avoid overlap with global labels/titles
place_with_margins <- function(grid, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) {
  cowplot::ggdraw() +
    cowplot::draw_plot(grid, x = left, y = bottom, width = 1 - left - right, height = 1 - top - bottom)
}

temp_sal_canvas <- place_with_margins(grid_temp_sal, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) +
  cowplot::draw_label("Temperature (°C)", x = 0.03, y = 0.5, angle = 90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Salinity (ppt)",   x = 0.97, y = 0.5, angle = -90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Temperature vs Salinity — Monthly Climatology by NEP",
                      x = 0.5, y = 0.995, vjust = 1, fontface = "bold", size = 14)

ph_do_canvas <- place_with_margins(grid_ph_do, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) +
  cowplot::draw_label("pH (unitless)",           x = 0.03, y = 0.5, angle = 90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Dissolved Oxygen (mg/L)", x = 0.97, y = 0.5, angle = -90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("pH vs Dissolved Oxygen — Monthly Climatology by NEP",
                      x = 0.5, y = 0.995, vjust = 1, fontface = "bold", size = 14)

# Display
print(temp_sal_canvas)
print(ph_do_canvas)

# Save
ggsave("nep_temp_sal_5x3.png", temp_sal_canvas, width = 20, height = 12, dpi = 300)
ggsave("nep_ph_do_5x3.png",  ph_do_canvas,  width = 20, height = 12, dpi = 300)

# 5x3 NEP plots of pH/DO and Sal/Temp v2 ################

# Packages
# install.packages(c("tidyverse", "cowplot"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(cowplot)

# Single-letter month labels for the x-axis
month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")

# Summarize one variable to monthly 10th/50th/90th for a single NEP df
# - Filters out NA datetimes (prevents NA month group)
# - Uses unordered month factor + complete() to cover all 12 months (no 13th NA)
summarize_monthly <- function(df,
                              value_col,
                              datetime_col = "datetime_utc",
                              equal_site_weighting = FALSE,     # set TRUE to weight sites equally
                              site_id_cols = c("site_code", "station_name")) {
  
  if (!datetime_col %in% names(df)) {
    stop("'", datetime_col, "' column not found in the data frame.")
  }
  if (!inherits(df[[datetime_col]], "POSIXt")) {
    stop("'", datetime_col, "' must be POSIXct/POSIXlt. Convert before calling this function.")
  }
  
  if (!value_col %in% names(df)) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  keep_cols <- c(datetime_col, value_col, site_id_cols[site_id_cols %in% names(df)])
  
  x <- df %>%
    select(all_of(keep_cols)) %>%
    filter(!is.na(.data[[datetime_col]]), !is.na(.data[[value_col]])) %>%  # drop NA datetime & NA value
    mutate(
      month = factor(lubridate::month(.data[[datetime_col]]),
                     levels = 1:12, labels = month.abb, ordered = FALSE)
    )
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  if (equal_site_weighting && all(site_id_cols %in% names(x))) {
    site_month <- x %>%
      group_by(across(all_of(site_id_cols)), month) %>%
      summarize(site_month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
    
    out <- site_month %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(site_month_median, 0.10, na.rm = TRUE, names = FALSE),
        median = median(site_month_median, na.rm = TRUE),
        p90    = quantile(site_month_median, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    out <- x %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(.data[[value_col]], 0.10, na.rm = TRUE, names = FALSE),
        median = median(.data[[value_col]], na.rm = TRUE),
        p90    = quantile(.data[[value_col]], 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  
  out %>%
    ungroup() %>%
    complete(
      month = factor(month.abb, levels = month.abb, ordered = FALSE),
      fill = list(p10 = NA_real_, median = NA_real_, p90 = NA_real_)
    ) %>%
    arrange(month)
}

# Build a dual-axis plot from two monthly summaries
# - Adds layers only if data present
# - NEP tag centered at the top
# - Fixed axis limits and breaks for standardized scales
build_dual_axis_plot <- function(left_sum, right_sum,
                                 left_label, right_label,
                                 left_color = "#D55E00",
                                 right_color = "#0072B2",
                                 add_axis_titles = FALSE,
                                 panel_tag = NULL,
                                 left_limits = NULL,     # c(min, max) on left-axis units
                                 right_limits = NULL,    # c(min, max) on right-axis units
                                 left_breaks = NULL,
                                 right_breaks = NULL) {
  
  left_has  <- any(is.finite(left_sum$median))
  right_has <- any(is.finite(right_sum$median))
  
  # Define linear map right->left: y_plot = a * y_right + b
  if (!is.null(left_limits) && !is.null(right_limits)) {
    left_min  <- left_limits[1]; left_max  <- left_limits[2]
    right_min <- right_limits[1]; right_max <- right_limits[2]
  } else {
    left_vals  <- c(left_sum$p10,  left_sum$median,  left_sum$p90)
    right_vals <- c(right_sum$p10, right_sum$median, right_sum$p90)
    left_min <- suppressWarnings(min(left_vals,  na.rm = TRUE)); left_max <- suppressWarnings(max(left_vals,  na.rm = TRUE))
    right_min <- suppressWarnings(min(right_vals, na.rm = TRUE)); right_max <- suppressWarnings(max(right_vals, na.rm = TRUE))
    if (!is.finite(left_min))  { left_min <- 0; left_max <- 1 }
    if (!is.finite(right_min)) { right_min <- 0; right_max <- 1 }
  }
  
  if (!is.finite(left_min) || !is.finite(left_max) || left_min == left_max ||
      !is.finite(right_min) || !is.finite(right_max) || right_min == right_max) {
    a <- 1; b <- 0
  } else {
    a <- (left_max - left_min) / (right_max - right_min)
    b <- left_min - a * right_min
  }
  
  # Transform right series to left-axis space
  right_plot <- right_sum %>%
    mutate(
      median_t = a * median + b,
      p10_t    = a * p10 + b,
      p90_t    = a * p90 + b
    )
  
  p <- ggplot()
  
  if (left_has) {
    p <- p +
      geom_ribbon(data = left_sum, aes(x = month, ymin = p10, ymax = p90, group = 1),
                  fill = left_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = left_sum, aes(x = month, y = median, group = 1),
                color = left_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  if (right_has) {
    p <- p +
      geom_ribbon(data = right_plot, aes(x = month, ymin = p10_t, ymax = p90_t, group = 1),
                  fill = right_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = right_plot, aes(x = month, y = median_t, group = 1),
                color = right_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  y_left_title  <- if (add_axis_titles) left_label  else NULL
  y_right_title <- if (add_axis_titles) right_label else NULL
  
  p +
    scale_x_discrete(limits = month.abb, labels = month_letters, drop = FALSE, na.translate = FALSE) +
    scale_y_continuous(
      name   = y_left_title,
      limits = left_limits,
      breaks = left_breaks,
      sec.axis = sec_axis(~ (. - b) / a, name = y_right_title, breaks = right_breaks)
    ) +
    labs(x = NULL, title = NULL, tag = panel_tag) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y.left  = if (add_axis_titles) element_text(color = left_color)  else element_blank(),
      axis.title.y.right = if (add_axis_titles) element_text(color = right_color) else element_blank(),
      axis.text.y.left   = element_text(color = left_color),
      axis.text.y.right  = element_text(color = right_color),
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      plot.tag = element_text(size = 9, face = "bold", hjust = 0.5, vjust = 1),
      plot.tag.position = c(0.5, 0.98),   # centered at top
      plot.margin = margin(6, 4, 3, 4)
    )
}

# For a single NEP df, make the two requested plots with standardized ranges
make_nep_plots <- function(df,
                           nep_name = NULL,
                           datetime_col = "datetime_utc",
                           equal_site_weighting = FALSE) {
  temp_sum <- summarize_monthly(df, "temp_c", datetime_col, equal_site_weighting)
  sal_sum  <- summarize_monthly(df, "sal_ppt", datetime_col, equal_site_weighting)
  ph_sum   <- summarize_monthly(df, "ph_T", datetime_col, equal_site_weighting)
  do_sum   <- summarize_monthly(df, "do_mgl", datetime_col, equal_site_weighting)
  
  # Temperature (left) vs Salinity (right)
  p1 <- build_dual_axis_plot(
    left_sum  = temp_sum,
    right_sum = sal_sum,
    left_label  = "Temperature (°C)",
    right_label = "Salinity (ppt)",
    left_color  = "#D55E00",   # orange
    right_color = "#0072B2",   # blue
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(0, 35),
    right_limits = c(0, 40),
    left_breaks  = seq(0, 35, 5),
    right_breaks = seq(0, 40, 10)
  )
  
  # pH (left) vs DO (right) with new colors
  p2 <- build_dual_axis_plot(
    left_sum  = ph_sum,
    right_sum = do_sum,
    left_label  = "pH (unitless)",
    right_label = "Dissolved Oxygen (mg/L)",
    left_color  = "#6A3D9A",   # purple for pH
    right_color = "#1B9E77",   # green for DO
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(6, 9),
    right_limits = c(0, 15),
    left_breaks  = 6:9,
    right_breaks = seq(0, 15, 5)
  )
  
  list(temp_sal = p1, ph_do = p2)
}

# Build plots for each NEP (assumes nep_filtered_data is a named list)
plots_by_nep <- purrr::imap(
  nep_filtered_data,
  ~ make_nep_plots(.x, nep_name = .y, datetime_col = "datetime_utc", equal_site_weighting = FALSE)
)

# Extract sets
temp_sal_plots <- purrr::map(plots_by_nep, "temp_sal")
ph_do_plots    <- purrr::map(plots_by_nep, "ph_do")

# Arrange into 5x3 grids with alignment
grid_temp_sal <- cowplot::plot_grid(plotlist = temp_sal_plots, ncol = 5, align = "hv")
grid_ph_do    <- cowplot::plot_grid(plotlist = ph_do_plots,  ncol = 5, align = "hv")

# Compose final figures with reserved margins (so labels/titles don’t overlap panels)
place_with_margins <- function(grid, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) {
  cowplot::ggdraw() +
    cowplot::draw_plot(grid, x = left, y = bottom, width = 1 - left - right, height = 1 - top - bottom)
}

temp_sal_canvas <- place_with_margins(grid_temp_sal, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) +
  cowplot::draw_label("Temperature (°C)", x = 0.03, y = 0.5, angle = 90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Salinity (ppt)",   x = 0.97, y = 0.5, angle = -90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Temperature vs Salinity — Monthly Climatology by NEP",
                      x = 0.5, y = 0.995, vjust = 1, fontface = "bold", size = 14)

ph_do_canvas <- place_with_margins(grid_ph_do, left = 0.12, right = 0.12, top = 0.12, bottom = 0.08) +
  cowplot::draw_label("pH (unitless)",           x = 0.03, y = 0.5, angle = 90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("Dissolved Oxygen (mg/L)", x = 0.97, y = 0.5, angle = -90, vjust = 0.5, fontface = "bold") +
  cowplot::draw_label("pH vs Dissolved Oxygen — Monthly Climatology by NEP",
                      x = 0.5, y = 0.995, vjust = 1, fontface = "bold", size = 14)

# Display
print(temp_sal_canvas)
print(ph_do_canvas)

# Save
ggsave("nep_temp_sal_5x3_ii.png", temp_sal_canvas, width = 20, height = 12, dpi = 300)
ggsave("nep_ph_do_5x3_ii.png",  ph_do_canvas,  width = 20, height = 12, dpi = 300)


# v3 Troublehsooting:   ###########
# 
nep_index <- tibble::tibble(
  idx  = seq_along(nep_filtered_data),
  name = names(nep_filtered_data),
  nrow = vapply(nep_filtered_data, nrow, integer(1))
)
nep_unf_index <- tibble::tibble(
  idx  = seq_along(nep_unfiltered_data),
  name = names(nep_unfiltered_data),
  nrow = vapply(nep_unfiltered_data, nrow, integer(1))
)
nep_unf_index
# compare the data frames at positions 3 and 6



# quick fingerprints to see if they look the same even if not bit-identical
fp <- function(df) {
  c(
    rows = nrow(df),
    temp_n = sum(!is.na(df$temp_c)),
    sal_n  = sum(!is.na(df$sal_ppt)),
    ph_n   = sum(!is.na(df$ph_T)),
    do_n   = sum(!is.na(df$do_mgl)),
    dt_min = as.numeric(min(df$datetime_utc, na.rm = TRUE)),
    dt_max = as.numeric(max(df$datetime_utc, na.rm = TRUE))
  )
}
rbind(`idx 3` = fp(nep_filtered_data[[3]]),
      `idx 6` = fp(nep_filtered_data[[6]]))

# # # # # # 
# 5x3 NEP plots of pH/DO and Sal/Temp v3 ##########
# Packages
# install.packages(c("tidyverse", "cowplot"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(cowplot)

# Single-letter month labels for the x-axis
month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")

# Summarize one variable to monthly 10th/50th/90th for a single NEP df
# - Drops NA datetime/value rows (prevents NA month)
# - Uses unordered month factor + complete() to cover all 12 months (no NA level)
summarize_monthly <- function(df,
                              value_col,
                              datetime_col = "datetime_utc",
                              equal_site_weighting = FALSE,
                              site_id_cols = c("site_code", "station_name")) {
  
  if (!datetime_col %in% names(df)) {
    stop("'", datetime_col, "' column not found in the data frame.")
  }
  if (!inherits(df[[datetime_col]], "POSIXt")) {
    stop("'", datetime_col, "' must be POSIXct/POSIXlt.")
  }
  
  if (!value_col %in% names(df)) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  keep_cols <- c(datetime_col, value_col, site_id_cols[site_id_cols %in% names(df)])
  
  x <- df %>%
    select(all_of(keep_cols)) %>%
    filter(!is.na(.data[[datetime_col]]), !is.na(.data[[value_col]])) %>%
    mutate(
      month = factor(lubridate::month(.data[[datetime_col]]),
                     levels = 1:12, labels = month.abb, ordered = FALSE)
    )
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  if (equal_site_weighting && all(site_id_cols %in% names(x))) {
    site_month <- x %>%
      group_by(across(all_of(site_id_cols)), month) %>%
      summarize(site_month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
    
    out <- site_month %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(site_month_median, 0.10, na.rm = TRUE, names = FALSE),
        median = median(site_month_median, na.rm = TRUE),
        p90    = quantile(site_month_median, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    out <- x %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(.data[[value_col]], 0.10, na.rm = TRUE, names = FALSE),
        median = median(.data[[value_col]], na.rm = TRUE),
        p90    = quantile(.data[[value_col]], 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  
  out %>%
    ungroup() %>%
    complete(
      month = factor(month.abb, levels = month.abb, ordered = FALSE),
      fill = list(p10 = NA_real_, median = NA_real_, p90 = NA_real_)
    ) %>%
    arrange(month)
}

# Build a dual-axis plot from two monthly summaries
# - Connects months with group=1
# - NEP tag centered at top
# - Fixed axis limits/breaks for standardized scales
build_dual_axis_plot <- function(left_sum, right_sum,
                                 left_label, right_label,
                                 left_color = "#D55E00",
                                 right_color = "#0072B2",
                                 add_axis_titles = FALSE,
                                 panel_tag = NULL,
                                 left_limits = NULL,     # c(min, max) on left-axis units
                                 right_limits = NULL,    # c(min, max) on right-axis units
                                 left_breaks = NULL,
                                 right_breaks = NULL) {
  
  left_has  <- any(is.finite(left_sum$median))
  right_has <- any(is.finite(right_sum$median))
  
  # Define linear map right->left: y_plot = a * y_right + b
  if (!is.null(left_limits) && !is.null(right_limits)) {
    left_min  <- left_limits[1]; left_max  <- left_limits[2]
    right_min <- right_limits[1]; right_max <- right_limits[2]
  } else {
    left_vals  <- c(left_sum$p10,  left_sum$median,  left_sum$p90)
    right_vals <- c(right_sum$p10, right_sum$median, right_sum$p90)
    left_min <- suppressWarnings(min(left_vals,  na.rm = TRUE)); left_max <- suppressWarnings(max(left_vals,  na.rm = TRUE))
    right_min <- suppressWarnings(min(right_vals, na.rm = TRUE)); right_max <- suppressWarnings(max(right_vals, na.rm = TRUE))
    if (!is.finite(left_min))  { left_min <- 0; left_max <- 1 }
    if (!is.finite(right_min)) { right_min <- 0; right_max <- 1 }
  }
  
  if (!is.finite(left_min) || !is.finite(left_max) || left_min == left_max ||
      !is.finite(right_min) || !is.finite(right_max) || right_min == right_max) {
    a <- 1; b <- 0
  } else {
    a <- (left_max - left_min) / (right_max - right_min)
    b <- left_min - a * right_min
  }
  
  # Transform right series to left-axis space
  right_plot <- right_sum %>%
    mutate(
      median_t = a * median + b,
      p10_t    = a * p10 + b,
      p90_t    = a * p90 + b
    )
  
  p <- ggplot()
  
  if (left_has) {
    p <- p +
      geom_ribbon(data = left_sum, aes(x = month, ymin = p10, ymax = p90, group = 1),
                  fill = left_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = left_sum, aes(x = month, y = median, group = 1),
                color = left_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  if (right_has) {
    p <- p +
      geom_ribbon(data = right_plot, aes(x = month, ymin = p10_t, ymax = p90_t, group = 1),
                  fill = right_color, alpha = 0.20, inherit.aes = FALSE, na.rm = TRUE) +
      geom_line(data = right_plot, aes(x = month, y = median_t, group = 1),
                color = right_color, linewidth = 1, inherit.aes = FALSE, na.rm = TRUE)
  }
  
  y_left_title  <- if (add_axis_titles) left_label  else NULL
  y_right_title <- if (add_axis_titles) right_label else NULL
  
  p +
    scale_x_discrete(limits = month.abb, labels = month_letters, drop = FALSE, na.translate = FALSE) +
    scale_y_continuous(
      name   = y_left_title,
      limits = left_limits,
      breaks = left_breaks,
      sec.axis = sec_axis(~ (. - b) / a, name = y_right_title, breaks = right_breaks)
    ) +
    labs(x = NULL, title = NULL, tag = panel_tag) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y.left  = if (add_axis_titles) element_text(color = left_color)  else element_blank(),
      axis.title.y.right = if (add_axis_titles) element_text(color = right_color) else element_blank(),
      axis.text.y.left   = element_text(color = left_color),
      axis.text.y.right  = element_text(color = right_color),
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      plot.tag = element_text(size = 11, face = "bold", hjust = 0.5, vjust = 1),
      plot.tag.position = c(0.5, 0.98),   # centered at top
      plot.margin = margin(6, 4, 3, 4)
    )
}

# For a single NEP df, make the two requested plots with standardized ranges
make_nep_plots <- function(df,
                           nep_name = NULL,
                           datetime_col = "datetime_utc",
                           equal_site_weighting = FALSE) {
  temp_sum <- summarize_monthly(df, "temp_c", datetime_col, equal_site_weighting)
  sal_sum  <- summarize_monthly(df, "sal_ppt", datetime_col, equal_site_weighting)
  ph_sum   <- summarize_monthly(df, "ph_T", datetime_col, equal_site_weighting)
  do_sum   <- summarize_monthly(df, "do_mgl", datetime_col, equal_site_weighting)
  
  # Temperature (left) vs Salinity (right)
  p1 <- build_dual_axis_plot(
    left_sum  = temp_sum,
    right_sum = sal_sum,
    left_label  = "Temperature (°C)",
    right_label = "Salinity (ppt)",
    left_color  = "#D55E00",   # orange
    right_color = "#0072B2",   # blue
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(0, 35),
    right_limits = c(0, 40),
    left_breaks  = seq(0, 35, 5),
    right_breaks = seq(0, 40, 10)
  )
  
  # pH (left) vs DO (right) with new colors
  p2 <- build_dual_axis_plot(
    left_sum  = ph_sum,
    right_sum = do_sum,
    left_label  = "pH (unitless)",
    right_label = "Dissolved Oxygen (mg/L)",
    left_color  = "#6A3D9A",   # purple for pH
    right_color = "#1B9E77",   # green for DO
    add_axis_titles = FALSE,
    panel_tag = nep_name,
    left_limits  = c(6, 9),
    right_limits = c(0, 15),
    left_breaks  = 6:9,
    right_breaks = seq(0, 15, 5)
  )
  
  list(temp_sal = p1, ph_do = p2)
}

# Build plots for each NEP (assumes nep_filtered_data is a named list)
plots_by_nep <- purrr::imap(
  nep_filtered_data,
  ~ make_nep_plots(.x, nep_name = .y, datetime_col = "datetime_utc", equal_site_weighting = FALSE)
)

# Extract sets
temp_sal_plots <- purrr::map(plots_by_nep, "temp_sal")
ph_do_plots    <- purrr::map(plots_by_nep, "ph_do")

# Arrange into 5x3 grids
grid_temp_sal <- cowplot::plot_grid(plotlist = temp_sal_plots, ncol = 5, align = "hv")
grid_ph_do    <- cowplot::plot_grid(plotlist = ph_do_plots,  ncol = 5, align = "hv")

# Compose final figures with white background and tighter outer margins
outer <- list(left = 0.04, right = 0.04, top = 0.04, bottom = 0.04)

white_canvas <- function() {
  cowplot::ggdraw() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
# Colors (keep consistent with what you use in build_dual_axis_plot)
col_temp <- "#D55E00"  # Temp (left)
col_sal  <- "#0072B2"  # Salinity (right)
col_ph   <- "#6A3D9A"  # pH (left)
col_do   <- "#1B9E77"  # DO (right)

temp_sal_canvas <- white_canvas() +
  cowplot::draw_plot(grid_temp_sal,
                     x = outer$left, y = outer$bottom,
                     width = 1 - outer$left - outer$right,
                     height = 1 - outer$top - outer$bottom) +
  cowplot::draw_label("Temperature (°C)", x = outer$left/2, y = 0.5,
                      angle = 90, vjust = 0.5, fontface = "bold", color=col_temp) +
  cowplot::draw_label("Salinity (ppt)",   x = 1 - outer$right/2, y = 0.5,
                      angle = -90, vjust = 0.5, fontface = "bold", color=col_sal) +
  cowplot::draw_label("Temperature vs Salinity — Monthly Climatology by NEP",
                      x = 0.5, y = 1 - outer$top/2, vjust = 0.5, fontface = "bold", size = 18)

ph_do_canvas <- white_canvas() +
  cowplot::draw_plot(grid_ph_do,
                     x = outer$left, y = outer$bottom,
                     width = 1 - outer$left - outer$right,
                     height = 1 - outer$top - outer$bottom) +
  cowplot::draw_label("pH (Total scale)",           x = outer$left/2, y = 0.5,
                      angle = 90, vjust = 0.5, fontface = "bold", color=col_ph) +
  cowplot::draw_label("Dissolved Oxygen (mg/L)", x = 1 - outer$right/2, y = 0.5,
                      angle = -90, vjust = 0.5, fontface = "bold", color=col_do) +
  cowplot::draw_label("pH vs Dissolved Oxygen — Monthly Climatology by NEP",
                      x = 0.5, y = 1 - outer$top/2, vjust = 0.5, fontface = "bold", size = 18)

# Display
print(temp_sal_canvas)
print(ph_do_canvas)

# Save (white background ensured)
ggsave("nep_temp_sal_5x3_III.png", temp_sal_canvas, width = 20, height = 12, dpi = 300, bg = "white")
ggsave("nep_ph_do_5x3_III.png",  ph_do_canvas,  width = 20, height = 12, dpi = 300, bg = "white")



## < ----- Single-NEP Faceted plot creation: ----
df = nep_filtered_data$LongIslandSound
# long format
long_df <- df %>%
  select(datetime_utc, temp_c, sal_ppt, ph_T, do_mgl) %>%
  pivot_longer(cols = c(temp_c, sal_ppt, ph_T, do_mgl),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(datetime_utc), !is.na(value)) %>%
  mutate(variable = factor(variable, levels = levels_order))

# 1) Overlay plot (standardized to z-scores so all variables share one y-axis)
overlay_z <- long_df %>%
  group_by(variable) %>%
  mutate(z = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = datetime_utc, y = z, color = variable)) +
  geom_point(size = 0.6, alpha = 0.5) +
  scale_color_manual(values = pal, labels = var_labels, breaks = levels_order, name = NULL) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                   expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "Standardized value (z-score)",
       title = "Long Island Sound — Temperature, Salinity, pH, DO (standardized)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
# print(overlay_z)

# 2) Faceted plot (preserves native units; still uses colors for quick ID)
faceted <- long_df %>%
  ggplot(aes(x = datetime_utc, y = value, color = variable)) +
  geom_point(size = 0.6, alpha = 0.5) +
  scale_color_manual(values = pal, labels = var_labels, breaks = levels_order, name = NULL) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                   expand = expansion(mult = c(0.01, 0.01))) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = as_labeller(var_labels)) +
  labs(x = NULL, y = NULL,
       title = "Long Island Sound — Temperature, Salinity, pH, DO (faceted by variable)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "grey95")
  )
# print(faceted)
ggsave('LIS_TS_allParam.png', faceted, width = 20, height = 12, dpi = 300, bg = "white" )

# <----  Multi-parameter time-series ----
# Ensure the expected columns exist
stopifnot(all(c("datetime_utc","temp_c","sal_ppt","ph_T","do_mgl") %in% names(df)))
stopifnot(inherits(df$datetime_utc, "POSIXt"))

# Desired top-to-bottom order
levels_order <- c("temp_c", "sal_ppt", "ph_T", "do_mgl", "co2_ppm")

# Color palette and nice labels
pal <- c(
  temp_c = "#D55E00",  # orange
  sal_ppt = "#0072B2", # blue
  ph_T = "#6A3D9A",    # purple
  do_mgl = "#1B9E77",  # green
  co2_ppm = "#E31A1C"  # red
)
var_labels <- c(
  temp_c = "Temperature (°C)",
  sal_ppt = "Salinity (PSU)",
  ph_T = "pH",
  do_mgl = "Dissolved Oxygen (mg/L)",
  co2_ppm = "pCO2 (ppm)"
)

# Function: build a faceted time-series per NEP using scattermore when appropriate
make_faceted_ts_fast <- function(df, nep_name,
                                 datetime_col = "datetime_utc",
                                 vars = c("temp_c","sal_ppt","ph_T","do_mgl","co2_ppm"),
                                 prefer_scattermore = TRUE,
                                 n_threshold_scattermore = 5e5,
                                 point_size = 1.0,
                                 alpha_range_sites = c(0.25, 0.85),
                                 shade_sites = TRUE) {
  stopifnot(datetime_col %in% names(df))
  if (!inherits(df[[datetime_col]], "POSIXt")) {
    stop(sprintf("Column '%s' must be POSIXct/POSIXlt", datetime_col))
  }
  
  vars_avail <- intersect(vars, names(df))
  if (length(vars_avail) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste(nep_name, "\n(no data)"),
                 size = 6, color = "grey40") +
        theme_void()
    )
  }
  
  n_sites <- if ("site_code" %in% names(df)) dplyr::n_distinct(df$site_code, na.rm = TRUE) else NA_integer_
  
  keep_cols <- unique(c(datetime_col, "site_code", vars_avail[vars_avail %in% names(df)]))
  long_df <- df %>%
    select(all_of(keep_cols)) %>%
    pivot_longer(cols = all_of(intersect(vars_avail, names(df))),
                 names_to = "variable", values_to = "value") %>%
    filter(!is.na(.data[[datetime_col]]), !is.na(value)) %>%
    mutate(
      variable = factor(variable, levels = levels_order[levels_order %in% vars_avail])
    )
  
  if (nrow(long_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste(nep_name, "\n(no valid data)"),
                 size = 6, color = "grey40") +
        theme_void()
    )
  }
  
  # NEW: define breaks/labels/values for exactly the variables present (in desired order)
  breaks_use <- levels(droplevels(long_df$variable))        # e.g., c("temp_c","sal_ppt",...)
  values_use <- pal[breaks_use]
  labels_use <- var_labels[breaks_use]
  
  if (shade_sites && "site_code" %in% names(long_df)) {
    site_levels <- sort(unique(long_df$site_code))
    alphas <- if (length(site_levels) == 1) {
      mean(alpha_range_sites)
    } else {
      seq(alpha_range_sites[1], alpha_range_sites[2], length.out = length(site_levels))
    }
    alpha_map <- setNames(alphas, site_levels)
    long_df$site_alpha <- alpha_map[as.character(long_df$site_code)]
  } else {
    long_df$site_alpha <- 0.5
  }
  
  use_scattermore <- prefer_scattermore &&
    requireNamespace("scattermore", quietly = TRUE) &&
    nrow(long_df) >= n_threshold_scattermore
  
  p <- ggplot(long_df, aes(x = .data[[datetime_col]], y = value, color = variable))
  
  if (use_scattermore) {
    p <- p +
      scattermore::geom_scattermore(
        aes(alpha = site_alpha),
        pointsize = point_size,
        pixels = c(2048, 1024),
        na.rm = TRUE
      ) +
      scale_alpha_identity(guide = "none")
  } else {
    p <- p +
      geom_point(aes(alpha = site_alpha), size = 0.6, na.rm = TRUE) +
      scale_alpha_identity(guide = "none")
  }
  
  p +
    scale_color_manual(
      values = values_use,
      labels = labels_use,
      breaks = breaks_use,
      limits = breaks_use,
      name   = NULL
    ) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                     expand = expansion(mult = c(0.01, 0.01))) +
    facet_wrap(~ variable, ncol = 1, scales = "free_y",
               labeller = as_labeller(var_labels)) +
    labs(
      x = NULL, y = NULL,
      title = paste0(nep_name, " — All measured variables"),
      subtitle = if (!is.na(n_sites)) paste0("Sites: ", n_sites,". Shading for different sites if applicable.") else NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold")
    )
}

# Build plots for all NEPs (replace 'nep_filtered_data' with data list)
all_faceted_fast <- imap(nep_filtered_data, ~ {
  make_faceted_ts_fast(.x, nep_name = .y, datetime_col = "datetime_utc",
                       prefer_scattermore = TRUE, n_threshold_scattermore = 5e5,
                       shade_sites = TRUE, alpha_range_sites = c(0.25, 0.85),
                       point_size = 1.0)
})

# Example: print one
# print(all_faceted_fast[["LongIslandSound"]])

# Save them
out_dir <- "nep_faceted_timeseries_scattermore"
dir.create(out_dir, showWarnings = FALSE)
walk2(names(all_faceted_fast), all_faceted_fast, ~ {
  ggsave(
    filename = file.path(out_dir, paste0(.x, "_faceted_timeseries.png")),
    plot = .y, width = 10, height = 11, dpi = 300, bg = "white"
  )
})


# <---- 5x3 plots for Salinity levels: ----
# Packages
# install.packages(c("tidyverse", "cowplot"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(cowplot)

# Choose which dataset to plot (must contain pH, DO, datetime, and a salinity class column)
# data_to_plot <- nep_filtered_data
data_to_plot <- nep_filtered_data

# Salinity regimes (ordered fresh -> saline) and safe names for files
regime_levels <- c("Tidal fresh", "Oligohaline", "Mesohaline", "Polyhaline", "Euhaline")
regime_safe   <- c("tidal_fresh", "oligohaline", "mesohaline", "polyhaline", "euhaline")

# Single-letter month labels
month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")

# Colors (original variable scheme)
col_ph <- "#6A3D9A"   # pH (purple)
col_do <- "#1B9E77"   # DO (green)

# Helper: pick the salinity-class column present (sal_class_medium preferred, else sal_class_median)
detect_regime_col <- function(df) {
  if ("sal_class_median" %in% names(df)) return("sal_class_median")
  stop("No 'sal_class_median' found in the data frame.")
}


# Summarize monthly climatology for a single variable and single regime
summarize_monthly_for_regime <- function(df,
                                         value_col,
                                         regime_name,
                                         datetime_col = "datetime_utc",
                                         regime_col = detect_regime_col(df),
                                         equal_site_weighting = FALSE,
                                         site_col = "site_code") {
  stopifnot(datetime_col %in% names(df))
  if (!inherits(df[[datetime_col]], "POSIXt")) stop("'", datetime_col, "' must be POSIXct/POSIXlt.")
  if (!(value_col %in% names(df)) || !(regime_col %in% names(df))) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  x <- df %>%
    select(all_of(c(datetime_col, value_col, regime_col, site_col))) %>%
    filter(!is.na(.data[[datetime_col]]),
           !is.na(.data[[value_col]]),
           !is.na(.data[[regime_col]]),
           .data[[regime_col]] == regime_name) %>%
    mutate(month = factor(lubridate::month(.data[[datetime_col]]),
                          levels = 1:12, labels = month.abb, ordered = FALSE))
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  if (equal_site_weighting && (site_col %in% names(x))) {
    site_month <- x %>%
      group_by(.data[[site_col]], month) %>%
      summarize(site_month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
    
    out <- site_month %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(site_month_median, 0.10, na.rm = TRUE, names = FALSE),
        median = median(site_month_median, na.rm = TRUE),
        p90    = quantile(site_month_median, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    out <- x %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(.data[[value_col]], 0.10, na.rm = TRUE, names = FALSE),
        median = median(.data[[value_col]], na.rm = TRUE),
        p90    = quantile(.data[[value_col]], 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  
  out %>%
    ungroup() %>%
    complete(month = factor(month.abb, levels = month.abb),
             fill = list(p10 = NA_real_, median = NA_real_, p90 = NA_real_)) %>%
    arrange(month)
}

# Build dual-axis pH (left) vs DO (right) overlapping in one panel (robust)
build_dual_axis_ph_do <- function(ph_sum, do_sum, nep_name,
                                  left_limits = c(6, 9),  left_breaks = 6:9,
                                  right_limits = c(0, 15), right_breaks = seq(0, 15, 5),
                                  left_color = col_ph, right_color = col_do) {
  # Ensure finite, ascending limits
  safe_limits <- function(lim, default) {
    if (!is.numeric(lim) || length(lim) != 2 || any(!is.finite(lim))) return(default)
    if (lim[1] == lim[2]) lim <- c(lim[1], lim[2] + 1e-6)
    if (lim[1] > lim[2]) lim <- rev(lim)
    lim
  }
  left_limits  <- safe_limits(left_limits,  c(0, 1))
  right_limits <- safe_limits(right_limits, c(0, 1))
  
  # Map right->left (y_plot = a * y_right + b) using fixed limits only
  a <- (left_limits[2] - left_limits[1]) / (right_limits[2] - right_limits[1])
  b <- left_limits[1] - a * right_limits[1]
  if (!is.finite(a) || !is.finite(b) || abs(a) < .Machine$double.eps) { a <- 1; b <- 0 }
  
  # Transform DO onto left-axis space
  do_t <- do_sum %>%
    mutate(
      median_t = a * median + b,
      p10_t    = a * p10 + b,
      p90_t    = a * p90 + b
    )
  
  ph_has <- any(is.finite(ph_sum$median))
  do_has <- any(is.finite(do_sum$median))
  
  # If neither has data, render a clean placeholder but KEEP axes (so no sec_axis errors)
  if (!ph_has && !do_has) {
    return(
      ggplot(tibble(month = factor(month.abb, levels = month.abb))) +
        aes(x = month, y = 0, group = 1) +
        geom_blank() +
        annotate("text", x = 6.5, y = mean(left_limits), label = "No data", color = "grey40") +
        scale_x_discrete(limits = month.abb, labels = month_letters,
                         drop = FALSE, na.translate = FALSE) +
        scale_y_continuous(
          name = NULL,
          limits = left_limits, breaks = left_breaks,
          sec.axis = sec_axis(~ (. - b) / a, name = NULL, breaks = right_breaks)
        ) +
        labs(x = NULL, title = NULL, tag = nep_name) +
        theme_bw(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_blank(),
          axis.title.y.left  = element_blank(),
          axis.title.y.right = element_blank(),
          axis.text.y.left   = element_text(color = left_color),
          axis.text.y.right  = element_text(color = right_color),
          axis.ticks.y.left  = element_line(color = left_color),
          axis.ticks.y.right = element_line(color = right_color),
          plot.tag = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1),
          plot.tag.position = c(0.5, 0.985),
          plot.margin = margin(6, 4, 3, 4)
        )
    )
  }
  
  ggplot() +
    # pH ribbon + line (left)
    { if (ph_has)
      geom_ribbon(data = ph_sum, aes(x = month, ymin = p10, ymax = p90, group = 1),
                  fill = left_color, alpha = 0.20, inherit.aes = FALSE) } +
    { if (ph_has)
      geom_line(data = ph_sum, aes(x = month, y = median, group = 1),
                color = left_color, linewidth = 0.9, inherit.aes = FALSE) } +
    # DO ribbon + line (transformed to left axis; dashed)
    { if (do_has)
      geom_ribbon(data = do_t, aes(x = month, ymin = p10_t, ymax = p90_t, group = 1),
                  fill = right_color, alpha = 0.20, inherit.aes = FALSE) } +
    { if (do_has)
      geom_line(data = do_t, aes(x = month, y = median_t, group = 1),
                color = right_color, linewidth = 0.9, linetype = "22", inherit.aes = FALSE) } +
    scale_x_discrete(limits = month.abb, labels = month_letters,
                     drop = FALSE, na.translate = FALSE) +
    scale_y_continuous(
      name = NULL,
      limits = left_limits, breaks = left_breaks,
      sec.axis = sec_axis(~ (. - b) / a, name = NULL, breaks = right_breaks)
    ) +
    labs(x = NULL, title = NULL, tag = nep_name) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      axis.title.y.left  = element_blank(),
      axis.title.y.right = element_blank(),
      axis.text.y.left   = element_text(color = left_color),
      axis.text.y.right  = element_text(color = right_color),
      axis.ticks.y.left  = element_line(color = left_color),
      axis.ticks.y.right = element_line(color = right_color),
      plot.tag = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1),
      plot.tag.position = c(0.5, 0.985),
      plot.margin = margin(6, 4, 3, 4)
    )
}

# Build five 5×3 figures (one per salinity class), overlapping pH (left) and DO (right)
make_regime_figures_ph_do_dual <- function(data_list,
                                           regimes = regime_levels,
                                           regimes_safe = regime_safe,
                                           datetime_col = "datetime_utc",
                                           equal_site_weighting = FALSE,
                                           file_prefix = "nep_ph_do_dual_") {
  figs <- list()
  for (i in seq_along(regimes)) {
    reg_name <- regimes[i]
    reg_safe <- regimes_safe[i]
    
    # Safely build per-NEP panels for this regime
    safe_build <- purrr::safely(function(df, nep_name) {
      ph_sum <- summarize_monthly_for_regime(df, "ph_T",  reg_name,
                                             datetime_col = datetime_col,
                                             regime_col   = detect_regime_col(df),
                                             equal_site_weighting = equal_site_weighting,
                                             site_col = "site_code")
      do_sum <- summarize_monthly_for_regime(df, "do_mgl", reg_name,
                                             datetime_col = datetime_col,
                                             regime_col   = detect_regime_col(df),
                                             equal_site_weighting = equal_site_weighting,
                                             site_col = "site_code")
      build_dual_axis_ph_do(ph_sum, do_sum, nep_name,
                            left_limits = c(6, 9),  left_breaks = 6:9,
                            right_limits = c(0, 15), right_breaks = seq(0, 15, 5),
                            left_color = col_ph, right_color = col_do)
    })
    
    panels <- imap(data_list, ~ {
      res <- safe_build(.x, .y)
      if (!is.null(res$error)) {
        message(sprintf("Panel error for NEP '%s' in regime '%s': %s", .y, reg_name, res$error$message))
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste(.y, "\nError"), size = 4, color = "grey40") +
            theme_void()
        )
      }
      res$result
    })
    
    # Arrange into 5×3 grid
    grid <- cowplot::plot_grid(plotlist = panels, ncol = 5, align = "hv")
    
    # Compose with white canvas and global labels/title
    outer <- list(left = 0.10, right = 0.10, top = 0.08, bottom = 0.06)
    white_canvas <- function() {
      cowplot::ggdraw() +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    }
    
    title_text <- paste0("pH (solid, purple) and DO (dashed, green) — ", reg_name, " (Monthly Climatology)")
    
    fig <- white_canvas() +
      cowplot::draw_plot(grid,
                         x = outer$left, y = outer$bottom,
                         width = 1 - outer$left - outer$right,
                         height = 1 - outer$top - outer$bottom) +
      cowplot::draw_label("pH (unitless)",           x = outer$left/2, y = 0.5,
                          angle = 90, vjust = 0.5, fontface = "bold", color = col_ph) +
      cowplot::draw_label("Dissolved Oxygen (mg/L)", x = 1 - outer$right/2, y = 0.5,
                          angle = -90, vjust = 0.5, fontface = "bold", color = col_do) +
      cowplot::draw_label(title_text,
                          x = 0.5, y = 1 - outer$top/2, vjust = 0.5, fontface = "bold", size = 14)
    
    figs[[reg_name]] <- fig
    
    # Save
    ggsave(paste0(file_prefix, reg_safe, "_5x3.png"),
           fig, width = 20, height = 12, dpi = 300, bg = "white")
  }
  figs
}

# Run: create and save all 5 regime figures (5×3 each) with overlapping pH/DO
regime_figs <- make_regime_figures_ph_do_dual(
  data_list = data_to_plot,
  regimes = regime_levels,
  regimes_safe = regime_safe,
  datetime_col = "datetime_utc",
  equal_site_weighting = FALSE,
  file_prefix = "nep_ph_do_dual_"
)

# <---- Reducing down to 3 salinity regimes ----
# Three-class regime order (display order)
regime_levels_3 <- c("Oligo-mesohaline", "Polyhaline", "Euhaline")

# Helper to recode a single data frame in-place (if sal_class_median exists)
recode_sal_3 <- function(df, col = "sal_class_median") {
  if (!(col %in% names(df))) return(df)
  df %>%
    mutate(
      !!col := trimws(as.character(.data[[col]])),
      !!col := case_when(
        .data[[col]] %in% c("Tidal fresh", "Oligohaline", "Mesohaline") ~ "Oligo-mesohaline",
        .data[[col]] == "Polyhaline" ~ "Polyhaline",
        .data[[col]] == "Euhaline"   ~ "Euhaline",
        TRUE ~ NA_character_
      ),
      !!col := factor(.data[[col]], levels = regime_levels_3)
    )
}

# Apply to whichever list(s) you use
# If you’ll plot the unfiltered set:
nep_unfiltered_data <- map(nep_unfiltered_data, recode_sal_3)
nep_filtered_data <- map(nep_filtered_data, recode_sal_3)
# Example: view one in-session
# print(regime_figs[["Mesohaline"]])

# Choose which dataset to plot
data_to_plot <- nep_filtered_data  #

# Regimes and safe file names
regime_levels <- c("Oligo-mesohaline", "Polyhaline", "Euhaline")
regime_safe   <- c("oligo_mesohaline", "polyhaline", "euhaline")

# Monthly climatology for one variable within one regime
summarize_monthly_for_regime <- function(df, value_col, regime_name,
                                         datetime_col = "datetime_utc",
                                         regime_col = "sal_class_median",
                                         equal_site_weighting = FALSE,
                                         site_col = "site_code") {
  if (!(datetime_col %in% names(df)) || !inherits(df[[datetime_col]], "POSIXt"))
    stop("datetime_utc must exist and be POSIXct/POSIXlt.")
  if (!(value_col %in% names(df)) || !(regime_col %in% names(df))) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  x <- df %>%
    select(all_of(c(datetime_col, value_col, regime_col, site_col))) %>%
    filter(!is.na(.data[[datetime_col]]),
           !is.na(.data[[value_col]]),
           !is.na(.data[[regime_col]]),
           .data[[regime_col]] == regime_name) %>%
    mutate(month = factor(lubridate::month(.data[[datetime_col]]),
                          levels = 1:12, labels = month.abb, ordered = FALSE))
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(month.abb, levels = month.abb),
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  
  if (equal_site_weighting && (site_col %in% names(x))) {
    site_month <- x %>%
      group_by(.data[[site_col]], month) %>%
      summarize(site_month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
    
    out <- site_month %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(site_month_median, 0.10, na.rm = TRUE, names = FALSE),
        median = median(site_month_median, na.rm = TRUE),
        p90    = quantile(site_month_median, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    out <- x %>%
      group_by(month) %>%
      summarize(
        p10    = quantile(.data[[value_col]], 0.10, na.rm = TRUE, names = FALSE),
        median = median(.data[[value_col]], na.rm = TRUE),
        p90    = quantile(.data[[value_col]], 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  
  out %>%
    ungroup() %>%
    complete(month = factor(month.abb, levels = month.abb),
             fill = list(p10 = NA_real_, median = NA_real_, p90 = NA_real_)) %>%
    arrange(month)
}

# Dual-axis pH (left) vs DO (right) for one NEP
build_dual_axis_ph_do <- function(ph_sum, do_sum, nep_name,
                                  left_limits = c(6, 9),  left_breaks = 6:9,
                                  right_limits = c(0, 15), right_breaks = seq(0, 15, 5),
                                  left_color = col_ph, right_color = col_do) {
  # Safe, fixed transform right->left
  a <- (left_limits[2] - left_limits[1]) / (right_limits[2] - right_limits[1])
  b <- left_limits[1] - a * right_limits[1]
  if (!is.finite(a) || !is.finite(b) || abs(a) < .Machine$double.eps) { a <- 1; b <- 0 }
  
  do_t <- do_sum %>%
    mutate(median_t = a * median + b,
           p10_t    = a * p10 + b,
           p90_t    = a * p90 + b)
  
  ph_has <- any(is.finite(ph_sum$median))
  do_has <- any(is.finite(do_sum$median))
  
  ggplot() +
    # pH
    { if (ph_has)
      geom_ribbon(data = ph_sum, aes(x = month, ymin = p10, ymax = p90, group = 1),
                  fill = left_color, alpha = 0.20, inherit.aes = FALSE) } +
    { if (ph_has)
      geom_line(data = ph_sum, aes(x = month, y = median, group = 1),
                color = left_color, linewidth = 0.9, inherit.aes = FALSE) } +
    # DO (transformed to left axis)
    { if (do_has)
      geom_ribbon(data = do_t, aes(x = month, ymin = p10_t, ymax = p90_t, group = 1),
                  fill = right_color, alpha = 0.20, inherit.aes = FALSE) } +
    { if (do_has)
      geom_line(data = do_t, aes(x = month, y = median_t, group = 1),
                color = right_color, linewidth = 0.9, linetype = "22", inherit.aes = FALSE) } +
    scale_x_discrete(limits = month.abb, labels = month_letters,
                     drop = FALSE, na.translate = FALSE) +
    scale_y_continuous(
      name = NULL,
      limits = left_limits, breaks = left_breaks,
      sec.axis = sec_axis(~ (. - b) / a, name = NULL, breaks = right_breaks)
    ) +
    labs(x = NULL, title = NULL, tag = nep_name) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_blank(),
      axis.title.y.left  = element_blank(),
      axis.title.y.right = element_blank(),
      axis.text.y.left   = element_text(color = left_color),
      axis.text.y.right  = element_text(color = right_color),
      axis.ticks.y.left  = element_line(color = left_color),
      axis.ticks.y.right = element_line(color = right_color),
      plot.tag = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1),
      plot.tag.position = c(0.5, 0.985),
      plot.margin = margin(6, 4, 3, 4)
    )
}

# Build and save one 5x3 figure per regime
make_regime_figures_ph_do_dual <- function(data_list,
                                           regimes = regime_levels,
                                           regimes_safe = regime_safe,
                                           datetime_col = "datetime_utc",
                                           equal_site_weighting = FALSE,
                                           file_prefix = "nep_ph_do_dual_") {
  figs <- list()
  for (i in seq_along(regimes)) {
    reg_name <- regimes[i]
    reg_safe <- regimes_safe[i]
    
    panels <- imap(data_list, ~ {
      ph_sum <- summarize_monthly_for_regime(.x, "ph_T",  reg_name, datetime_col,
                                             regime_col = "sal_class_median",
                                             equal_site_weighting = equal_site_weighting,
                                             site_col = "site_code")
      do_sum <- summarize_monthly_for_regime(.x, "do_mgl", reg_name, datetime_col,
                                             regime_col = "sal_class_median",
                                             equal_site_weighting = equal_site_weighting,
                                             site_col = "site_code")
      build_dual_axis_ph_do(ph_sum, do_sum, nep_name = .y,
                            left_limits = c(6, 9),  left_breaks = 6:9,
                            right_limits = c(0, 15), right_breaks = seq(0, 15, 5),
                            left_color = col_ph, right_color = col_do)
    })
    
    grid <- cowplot::plot_grid(plotlist = panels, ncol = 5, align = "hv")
    
    # Compose with white canvas and global labels/title
    outer <- list(left = 0.10, right = 0.10, top = 0.08, bottom = 0.06)
    white_canvas <- function() {
      cowplot::ggdraw() +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    }
    
    title_text <- paste0("pH (solid, purple) and DO (dashed, green) — ", reg_name, " (Monthly Climatology)")
    
    fig <- white_canvas() +
      cowplot::draw_plot(grid,
                         x = outer$left, y = outer$bottom,
                         width = 1 - outer$left - outer$right,
                         height = 1 - outer$top - outer$bottom) +
      cowplot::draw_label("pH (unitless)",           x = outer$left/2, y = 0.5,
                          angle = 90, vjust = 0.5, fontface = "bold", color = col_ph) +
      cowplot::draw_label("Dissolved Oxygen (mg/L)", x = 1 - outer$right/2, y = 0.5,
                          angle = -90, vjust = 0.5, fontface = "bold", color = col_do) +
      cowplot::draw_label(title_text,
                          x = 0.5, y = 1 - outer$top/2, vjust = 0.5, fontface = "bold", size = 14)
    
    figs[[reg_name]] <- fig
    
    ggsave(paste0(file_prefix, reg_safe, "_5x3.png"),
           fig, width = 20, height = 12, dpi = 300, bg = "white")
  }
  figs
}

# Run for the 3-class scheme
regime_figs <- make_regime_figures_ph_do_dual(
  data_list = data_to_plot,
  regimes = regime_levels,                         # c("Oligo-mesohaline","Polyhaline","Euhaline")
  regimes_safe = regime_safe,                      # c("oligo_mesohaline","polyhaline","euhaline")
  datetime_col = "datetime_utc",
  equal_site_weighting = FALSE,
  file_prefix = "nep_ph_do_dual_"
)


# <---- Making more NEP plots ----
# Choose the list to plot
data_list <- nep_filtered_data   # or nep_unfiltered_data

# Helpers
month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
var_cols <- list(temp_c = "#D55E00", sal_ppt = "#0072B2", ph_T = "#6A3D9A", do_mgl = "#1B9E77")

# pCO2 column detector
detect_pco2_col <- function(df) {
  if ("pco2_ppm" %in% names(df)) "pco2_ppm"
  else if ("co2_ppm" %in% names(df)) "co2_ppm"
  else NULL
}

# Safe white canvas with outer margins for global labels/titles
white_canvas <- function() {
  cowplot::ggdraw() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}

# <-- Data availability heatmap (variables x time by NEP)----
# v2 - making sure pCO2 shows up in Tillamook
# Variables to show
vars <- c("temp_c","sal_ppt","ph_T","do_mgl","pco2")  # pco2 will be detected

avail_long <- purrr::imap(data_list, function(df, nep) {
  if (!("datetime_utc" %in% names(df))) return(NULL)
  
  vmap <- c(temp_c = "temp_c",
            sal_ppt = "sal_ppt",
            ph_T    = "ph_T",
            do_mgl  = "do_mgl")
  pco2_col <- detect_pco2_col(df)
  if (!is.null(pco2_col)) vmap <- c(vmap, pco2 = pco2_col)
  
  present <- unname(vmap)[unname(vmap) %in% names(df)]
  if (length(present) == 0) return(NULL)
  
  df %>%
    mutate(year_month = lubridate::floor_date(datetime_utc, "month")) %>%
    group_by(year_month) %>%
    summarize(across(all_of(present), ~ any(!is.na(.)), .names = "{.col}"), .groups = "drop") %>%
    pivot_longer(cols = all_of(present), names_to = "col", values_to = "has") %>%
    mutate(variable = recode(col, !!!setNames(names(vmap), unname(vmap))),
           nep = nep) %>%
    select(nep, year_month, variable, has)
}) %>% dplyr::bind_rows()

if (!is.null(avail_long) && nrow(avail_long) > 0) {
  avail_long <- avail_long %>%
    mutate(nep = factor(nep, levels = rev(sort(unique(nep)))),
           variable = factor(variable,
                             levels = c("temp_c","sal_ppt","ph_T","do_mgl","pco2")))
}

var_labels <- c(
  temp_c  = "Temperature (°C)",
  sal_ppt = "Salinity (ppt)",
  ph_T    = "pH (total-scale)",
  do_mgl  = "Dissolved Oxygen (mg/L)",
  pco2    = "pCO2"  # generic label (mixed units across NEPs)
)

p_avail <- ggplot(avail_long, aes(x = year_month, y = nep, fill = has)) +
  geom_tile() +
  scale_fill_manual(values = c(`TRUE` = "#2C7BB6", `FALSE` = "grey90"), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
  labs(x = NULL, y = NULL, title = "Data availability by NEP and month") +
  facet_wrap(~ variable, ncol = 1, scales = "free_x", labeller = as_labeller(var_labels)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "grey95"))




ggsave("fig_data_availability.png", p_avail, width = 12, height = 10, dpi = 300, bg = "white")


# <-- Site x Time "barcode" plot: similar to heatmap but for each measurement/site within an NEP: ---- 
# Find NEP name by pattern (case-insensitive; first match)
find_nep <- function(pattern, names_vec = names(data_list)) {
  m <- grep(pattern, names_vec, ignore.case = TRUE, value = TRUE)
  if (length(m) == 0) stop(sprintf("NEP matching '%s' not found in names(data_list).", pattern))
  m[[1]]
}

make_barcode <- function(df, nep_name, vars = c("temp_c","sal_ppt","ph_T","do_mgl")) {
  have <- intersect(vars, names(df))
  if (!all(c("datetime_utc","site_code") %in% names(df)) || length(have)==0) {
    return(ggplot() + theme_void() + ggtitle(paste(nep_name, "(no data)")))
  }
  x <- df %>%
    transmute(site_code = as.character(site_code),
              year_month = floor_date(datetime_utc, "month")) %>%
    bind_cols(df[have]) %>%
    mutate(across(all_of(have), ~ !is.na(.))) %>%
    group_by(site_code, year_month) %>%
    summarize(across(all_of(have), any, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = all_of(have), names_to = "variable", values_to = "has")
  
  ggplot(x, aes(x = year_month, y = reorder(site_code, site_code), fill = has)) +
    geom_tile() +
    scale_fill_manual(values = c(`TRUE` = "#2C7BB6", `FALSE` = "grey95"), guide = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
    labs(x = NULL, y = NULL, title = nep_name) +
    facet_wrap(~ variable, ncol = 1, labeller = as_labeller(c(
      temp_c  = "Temperature (°C)",
      sal_ppt = "Salinity (ppt)",
      ph_T    = "pH (total-scale)",
      do_mgl  = "Dissolved Oxygen (mg/L)"
    ))) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(), strip.background = element_rect(fill = "grey95"))
}

nep_narr <- find_nep("Narragansett")
nep_irl  <- find_nep("Indian River Lagoon|IndianRiverLagoon|Indian River")

p_bar_narr <- make_barcode(data_list[[nep_narr]], nep_narr)
p_bar_irl  <- make_barcode(data_list[[nep_irl]],  nep_irl)

ggsave("fig_barcode_narragansett.png", p_bar_narr, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("fig_barcode_indian_river_lagoon.png", p_bar_irl, width = 10, height = 8, dpi = 300, bg = "white")

# ========== 6) Temperature–Salinity diagrams (5×3 grid) ==========
plots_ts <- imap(data_list, ~ {
  df <- .x; nm <- .y
  if (!all(c("temp_c","sal_ppt") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no Temp/Sal)")))
  }
  ggplot(df, aes(x = sal_ppt, y = temp_c)) +
    stat_binhex(bins = 40, na.rm = TRUE) +
    scale_fill_viridis_c(option = "C", trans = "log10", name = "count", na.value = "grey90") +
    coord_cartesian(xlim = c(0,40), ylim = c(0,35)) +
    labs(x = "Salinity (ppt)", y = "Temperature (°C)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})
grid_ts <- cowplot::plot_grid(plotlist = plots_ts, ncol = 5, align = "hv")
ggsave("fig_ts_5x3.png", grid_ts, width = 20, height = 12, dpi = 300, bg = "white")

# ========== 7) Monthly diel climatology (Tillamook, Mobile Bay, Barnegat) ==========
# v2:
# Optional: local time mapping (leave empty for UTC defaults; add specific NEPs as needed)
tz_map <- c(
  Tillamook = "America/Los_Angeles",
  Mobile    = "America/Chicago",
  Barnegat  = "America/New_York"
)

# Summarize monthly diel climatology (median + 10–90%) with optional equal site weighting
summarize_diel_monthly <- function(df, value_col,
                                   datetime_col = "datetime_utc",
                                   tz = NULL,
                                   equal_site_weighting = FALSE,
                                   site_col = "site_code") {
  if (!(datetime_col %in% names(df)) || !(value_col %in% names(df))) {
    return(tibble(month = factor(character(), levels = month.abb), hour = numeric(),
                  p10 = numeric(), median = numeric(), p90 = numeric()))
  }
  dt <- df[[datetime_col]]
  if (!inherits(dt, "POSIXt")) stop("datetime_utc must be POSIXct/POSIXlt")
  if (!is.null(tz)) dt <- lubridate::with_tz(dt, tz)
  
  x <- df %>%
    mutate(.dt = dt) %>%
    transmute(
      month = factor(lubridate::month(.dt), levels = 1:12, labels = month.abb, ordered = FALSE),
      hour  = lubridate::hour(.dt),
      val   = .data[[value_col]],
      site  = if ("site_code" %in% names(df)) as.character(.data[[site_col]]) else NA_character_
    ) %>%
    filter(is.finite(val))
  
  if (nrow(x) == 0) {
    return(tibble(month = factor(character(), levels = month.abb), hour = numeric(),
                  p10 = numeric(), median = numeric(), p90 = numeric()))
  }
  
  if (equal_site_weighting && "site" %in% names(x)) {
    sm <- x %>%
      group_by(site, month, hour) %>%
      summarize(m = median(val, na.rm = TRUE), .groups = "drop")
    sm %>%
      group_by(month, hour) %>%
      summarize(
        p10    = quantile(m, 0.10, na.rm = TRUE, names = FALSE),
        median = median(m, na.rm = TRUE),
        p90    = quantile(m, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  } else {
    x %>%
      group_by(month, hour) %>%
      summarize(
        p10    = quantile(val, 0.10, na.rm = TRUE, names = FALSE),
        median = median(val, na.rm = TRUE),
        p90    = quantile(val, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
}

# Panel plotter that handles empty summaries gracefully
plot_diel <- function(sum_df, ylab, color, y_limits, y_breaks, title = NULL) {
  if (nrow(sum_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data", color = "grey40") +
        theme_void() + ggtitle(title %||% "")
    )
  }
  
  ggplot(sum_df) +
    geom_ribbon(aes(x = hour, ymin = p10, ymax = p90), fill = color, alpha = 0.2, na.rm = TRUE) +
    geom_line(aes(x = hour, y = median), color = color, linewidth = 0.9, na.rm = TRUE) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24), expand = c(0, 0)) +
    scale_y_continuous(ylab, limits = y_limits, breaks = y_breaks) +
    facet_wrap(~ month, ncol = 4, drop = TRUE) +
    labs(title = title) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey95"))
}

# Run for all NEPs and save
dir.create("fig_diel_all", showWarnings = FALSE)
for (nep in names(data_list)) {
  df <- data_list[[nep]]
  tz <- if (!is.null(tz_map) && nep %in% names(tz_map)) unname(tz_map[nep]) else NULL
  
  ph_sum <- summarize_diel_monthly(df, "ph_T", tz = tz)
  do_sum <- summarize_diel_monthly(df, "do_mgl", tz = tz)
  
  p_ph <- plot_diel(ph_sum, "pH (total-scale)", "#6A3D9A", c(6, 9), 6:9, paste(nep, "— pH diel by month"))
  p_do <- plot_diel(do_sum, "Dissolved Oxygen (mg/L)", "#1B9E77", c(0, 15), seq(0, 15, 5), paste(nep, "— DO diel by month"))
  
  ggsave(file.path("fig_diel_all", paste0("diel_ph_", gsub("\\s+", "_", nep), ".png")),
         p_ph, width = 10, height = 8, dpi = 300, bg = "white")
  ggsave(file.path("fig_diel_all", paste0("diel_do_", gsub("\\s+", "_", nep), ".png")),
         p_do, width = 10, height = 8, dpi = 300, bg = "white")
}

# original:
tz_map <- c(
  Tillamook  = "America/Los_Angeles",
  Mobile     = "America/Chicago",
  MobileBay  = "America/Chicago",
  Barnegat   = "America/New_York",
  `Barnegat Bay` = "America/New_York"
)
summarize_diel_monthly <- function(df, value_col, datetime_col = "datetime_utc", tz = NULL,
                                   equal_site_weighting = FALSE, site_col = "site_code") {
  if (!(datetime_col %in% names(df)) || !(value_col %in% names(df))) {
    return(tibble(month = factor(month.abb, levels = month.abb), hour = 0:23,
                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  }
  dt <- df[[datetime_col]]
  if (!inherits(dt, "POSIXt")) stop("datetime_utc must be POSIXct")
  if (!is.null(tz)) dt <- with_tz(dt, tz)
  
  x <- df %>%
    mutate(.dt = dt) %>%
    transmute(month = factor(month(.dt), levels = 1:12, labels = month.abb, ordered = FALSE),
              hour  = hour(.dt),
              val   = .data[[value_col]],
              site  = if ("site_code" %in% names(df)) as.character(site_code) else NA_character_) %>%
    filter(!is.na(val))
  
  if (nrow(x) == 0) return(tibble(month = factor(month.abb, levels = month.abb), hour = 0:23,
                                  p10 = NA_real_, median = NA_real_, p90 = NA_real_))
  
  if (equal_site_weighting && "site" %in% names(x)) {
    sm <- x %>% group_by(site, month, hour) %>% summarize(m = median(val, na.rm = TRUE), .groups = "drop")
    sm %>% group_by(month, hour) %>%
      summarize(p10 = quantile(m, 0.10, na.rm = TRUE, names = FALSE),
                median = median(m, na.rm = TRUE),
                p90 = quantile(m, 0.90, na.rm = TRUE, names = FALSE), .groups = "drop")
  } else {
    x %>% group_by(month, hour) %>%
      summarize(p10 = quantile(val, 0.10, na.rm = TRUE, names = FALSE),
                median = median(val, na.rm = TRUE),
                p90 = quantile(val, 0.90, na.rm = TRUE, names = FALSE), .groups = "drop")
  }
}
plot_diel <- function(sum_df, ylab, color, y_limits, y_breaks, title = NULL) {
  ggplot(sum_df) +
    geom_ribbon(aes(x = hour, ymin = p10, ymax = p90), fill = color, alpha = 0.2, na.rm = TRUE) +
    geom_line(aes(x = hour, y = median), color = color, linewidth = 0.9, na.rm = TRUE) +
    scale_x_continuous(breaks = c(0,6,12,18,24), limits = c(0,24), expand = c(0,0)) +
    scale_y_continuous(ylab, limits = y_limits, breaks = y_breaks) +
    facet_wrap(~ month, ncol = 4) +
    labs(title = title) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"))
}

neps_diel <- c(find_nep("Tillamook"), find_nep("Mobile"), find_nep("Barnegat"))
for (nep in neps_diel) {
  df <- data_list[[nep]]
  tz <- tz_map[[nep]] %||% NULL
  
  ph_sum <- summarize_diel_monthly(df, "ph_T", tz = tz)
  do_sum <- summarize_diel_monthly(df, "do_mgl", tz = tz)
  
  p_ph <- plot_diel(ph_sum, "pH (total-scale)", var_cols$ph_T, c(6,9), 6:9, paste(nep, "— pH diel by month"))
  p_do <- plot_diel(do_sum, "Dissolved Oxygen (mg/L)", var_cols$do_mgl, c(0,15), seq(0,15,5), paste(nep, "— DO diel by month"))
  
  ggsave(paste0("fig_diel_ph_", gsub("\\s+","_",nep), ".png"), p_ph, width = 10, height = 8, dpi = 300, bg = "white")
  ggsave(paste0("fig_diel_do_", gsub("\\s+","_",nep), ".png"), p_do, width = 10, height = 8, dpi = 300, bg = "white")
}


# ========== 8) Diel amplitude by month and regime (same 3 NEPs) ==========
### v2: 
daily_range_summary_simple <- function(df, value_col, datetime_col = "datetime_utc", site_col = "site_code") {
  if (!all(c(datetime_col, value_col) %in% names(df))) return(tibble())
  if (!inherits(df[[datetime_col]], "POSIXct")) stop("datetime_utc must be POSIXct/POSIXlt")
  df %>%
    filter(is.finite(.data[[value_col]]), !is.na(.data[[datetime_col]])) %>%
    mutate(date = as_date(.data[[datetime_col]]),
           month = factor(month(.data[[datetime_col]]), levels = 1:12, labels = month.abb, ordered = FALSE)) %>%
    group_by(.data[[site_col]], date, month, .add = TRUE) %>%
    summarize(daily_range = max(.data[[value_col]], na.rm = TRUE) - min(.data[[value_col]], na.rm = TRUE),
              .groups = "drop")
}

build_range_plot_simple <- function(df, value_col, ylab, title_nm) {
  sm <- daily_range_summary_simple(df, value_col)
  if (nrow(sm) == 0) return(ggplot() + theme_void() + ggtitle(paste(title_nm, "(no data)")))
  ggplot(sm, aes(x = month, y = daily_range)) +
    geom_violin(fill = "grey80", color = NA, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.8) +
    labs(x = NULL, y = ylab, title = title_nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

plots_ph_rng <- imap(data_list, ~ build_range_plot_simple(.x, "ph_T",  "Daily pH range (unitless)", .y))
plots_do_rng <- imap(data_list, ~ build_range_plot_simple(.x, "do_mgl","Daily DO range (mg/L)",    .y))

grid_ph_rng <- cowplot::plot_grid(plotlist = plots_ph_rng, ncol = 5, align = "hv")
grid_do_rng <- cowplot::plot_grid(plotlist = plots_do_rng, ncol = 5, align = "hv")

ggsave("fig_diel_range_ph_5x3.png", grid_ph_rng, width = 20, height = 12, dpi = 300, bg = "white")
ggsave("fig_diel_range_do_5x3.png", grid_do_rng, width = 20, height = 12, dpi = 300, bg = "white")

### original:
daily_range_summary <- function(df, value_col, datetime_col = "datetime_utc",
                                regime_col = "sal_class_median", site_col = "site_code") {
  if (!all(c(datetime_col, value_col, regime_col) %in% names(df))) return(tibble())
  if (!inherits(df[[datetime_col]], "POSIXct")) stop("datetime_utc must be POSIXct")
  df %>%
    filter(!is.na(.data[[datetime_col]]), !is.na(.data[[value_col]]), !is.na(.data[[regime_col]])) %>%
    mutate(date = as_date(.data[[datetime_col]]),
           month = factor(month(.data[[datetime_col]]), levels = 1:12, labels = month.abb, ordered = FALSE)) %>%
    group_by(.data[[site_col]], .data[[regime_col]], date, month) %>%
    summarize(daily_range = max(.data[[value_col]], na.rm = TRUE) - min(.data[[value_col]], na.rm = TRUE),
              .groups = "drop")
}
plot_range_by_month_regime <- function(sum_df, regime_col = "sal_class_median", ylab = "", palette = "Set2") {
  if (nrow(sum_df) == 0) return(ggplot() + theme_void() + ggtitle("No data"))
  ggplot(sum_df, aes(x = month, y = daily_range, fill = !!sym(regime_col))) +
    geom_violin(scale = "width", trim = TRUE, color = NA, alpha = 0.5) +
    geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.8) +
    scale_fill_brewer(palette = palette, name = "Regime") +
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}
for (nep in neps_diel) {
  df <- data_list[[nep]]
  ph_rng <- daily_range_summary(df, "ph_T")
  do_rng <- daily_range_summary(df, "do_mgl")
  p_ph_rng <- plot_range_by_month_regime(ph_rng, ylab = "Daily pH range (unitless)")
  p_do_rng <- plot_range_by_month_regime(do_rng, ylab = "Daily DO range (mg/L)")
  ggsave(paste0("fig_diel_range_ph_", gsub("\\s+","_",nep), ".png"), p_ph_rng, width = 10, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("fig_diel_range_do_", gsub("\\s+","_",nep), ".png"), p_do_rng, width = 10, height = 6, dpi = 300, bg = "white")
}
# 8.2
daily_range_summary <- function(df, value_col, datetime_col = "datetime_utc",
                                regime_col = "sal_class_median", site_col = "site_code") {
  if (!all(c(datetime_col, value_col, regime_col) %in% names(df))) return(tibble())
  if (!inherits(df[[datetime_col]], "POSIXct")) stop("datetime_utc must be POSIXct")
  df %>%
    filter(!is.na(.data[[datetime_col]]), !is.na(.data[[value_col]]), !is.na(.data[[regime_col]])) %>%
    mutate(date = as_date(.data[[datetime_col]]),
           month = factor(month(.data[[datetime_col]]), levels = 1:12, labels = month.abb, ordered = FALSE)) %>%
    group_by(.data[[site_col]], .data[[regime_col]], date, month) %>%
    summarize(daily_range = max(.data[[value_col]], na.rm = TRUE) - min(.data[[value_col]], na.rm = TRUE),
              .groups = "drop")
}

build_range_plot_for_nep <- function(df, value_col, ylab) {
  sm <- daily_range_summary(df, value_col)
  if (nrow(sm) == 0) return(ggplot() + theme_void() + ggtitle("(no data)"))
  ggplot(sm, aes(x = month, y = daily_range, fill = .data[["sal_class_median"]])) +
    geom_violin(scale = "width", trim = TRUE, color = NA, alpha = 0.5) +
    geom_boxplot(width = 0.12, outlier.size = 0.5, alpha = 0.8) +
    scale_fill_brewer(palette = "Set2", name = "Regime") +
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "none")
}

plots_ph_rng <- imap(data_list, ~ build_range_plot_for_nep(.x, "ph_T", "Daily pH range (unitless)") + ggtitle(.y))
plots_do_rng <- imap(data_list, ~ build_range_plot_for_nep(.x, "do_mgl", "Daily DO range (mg/L)") + ggtitle(.y))

grid_ph_rng <- cowplot::plot_grid(plotlist = plots_ph_rng, ncol = 5, align = "hv")
grid_do_rng <- cowplot::plot_grid(plotlist = plots_do_rng, ncol = 5, align = "hv")

ggsave("fig_diel_range_ph_5x3.png", grid_ph_rng, width = 20, height = 12, dpi = 300, bg = "white")
ggsave("fig_diel_range_do_5x3.png", grid_do_rng, width = 20, height = 12, dpi = 300, bg = "white")

# ========== 9) Hypoxia occurrence by month/NEP (5×3 grid) ==========
# v3
hypoxia_threshold <- 3  # mg/L

plots_hypoxia <- imap(data_list, ~ {
  df <- .x; nm <- .y
  if (!all(c("datetime_utc","do_mgl") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no DO)")))
  }
  
  x <- df %>%
    filter(!is.na(datetime_utc)) %>%
    mutate(do_valid = ifelse(is.finite(do_mgl) & do_mgl > -90, do_mgl, NA_real_),
           month    = factor(month(datetime_utc), levels = 1:12, labels = month.abb, ordered = FALSE),
           hypoxic  = do_valid < hypoxia_threshold) %>%
    group_by(month) %>%
    summarize(
      n_valid = sum(!is.na(do_valid)),
      frac    = ifelse(n_valid > 0, mean(hypoxic, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  
  ggplot(x, aes(x = month, y = frac)) +
    geom_col(fill = "#1B9E77", na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = NULL, title = nm) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
})

grid_hypoxia <- cowplot::plot_grid(plotlist = plots_hypoxia, ncol = 5, align = "hv")

# Add overall title above the grid
final_hypoxia <- cowplot::ggdraw() +
  cowplot::draw_plot(grid_hypoxia, x = 0.03, y = 0.03, width = 0.94, height = 0.92) +
  cowplot::draw_label("Fraction of Hypoxia observations (DO < 3 mg/L)",
                      x = 0.5, y = 0.98, vjust = 1, fontface = "bold", size = 16)

ggsave("fig_hypoxia_frac_5x3.png", final_hypoxia, width = 20, height = 12, dpi = 300, bg = "white")
# v2
hypoxia_threshold <- 3  # mg/L
plots_hypoxia <- imap(data_list, ~ {
  df <- .x; nm <- .y
  if (!all(c("datetime_utc","do_mgl") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no DO)")))
  }
  x <- df %>%
    filter(!is.na(datetime_utc), !is.na(do_mgl)) %>%
    mutate(do_valid = ifelse(do_mgl <= 1, NA_real_, do_mgl)) %>%  # convert -99 (and similar) to NA
    mutate(month = factor(month(datetime_utc), levels = 1:12, labels = month.abb, ordered = FALSE),
           hypoxic = do_valid < hypoxia_threshold) %>%
    group_by(month) %>%
    summarize(
      n_valid = sum(!is.na(do_valid)),
      frac = ifelse(n_valid > 0, mean(hypoxic, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  
  ggplot(x, aes(x = month, y = frac)) +
    geom_col(fill = "#1B9E77") +
    # scale_y_continuous("Hypoxia fraction (below 3 mg/L)", limits = c(0,1), labels = scales::percent_format(accuracy = 1), na.translate = FALSE) +
    ylim(0,0.2)+
    labs(x = NULL, title = nm, y = 'Fraction of DO obs < 3 mg/L') +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})

grid_hypoxia <- cowplot::plot_grid(plotlist = plots_hypoxia, title='Test', ncol = 5, align = "hv")+
  cowplot::draw_label("Fraction of Hypoxia observations (DO < 3 mg/L)",
                      x = 0.5, y = 0.98, vjust = 1, fontface = "bold", size = 16)
ggsave("fig_hypoxia_frac_5x3.png", grid_hypoxia, width = 20, height = 12, dpi = 300, bg = "white")

# original: 
plots_hypoxia <- imap(data_list, ~ {
  df <- .x; nm <- .y
  if (!all(c("datetime_utc","do_mgl") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no DO)")))
  }
  x <- df %>%
    filter(!is.na(datetime_utc), !is.na(do_mgl)) %>%
    mutate(month = factor(month(datetime_utc), levels = 1:12, labels = month.abb, ordered = FALSE),
           hypoxic = do_mgl < hypoxia_threshold) %>%
    group_by(month) %>%
    summarize(frac = mean(hypoxic), n = n(), .groups = "drop")
  ggplot(x, aes(x = month, y = frac)) +
    geom_col(fill = var_cols$do_mgl) +
    scale_y_continuous("Hypoxia fraction (<3 mg/L)", limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})
grid_hypoxia <- cowplot::plot_grid(plotlist = plots_hypoxia, ncol = 5, align = "hv")
ggsave("fig_hypoxia_frac_5x3.png", grid_hypoxia, width = 20, height = 12, dpi = 300, bg = "white")

# ========== 11) pH–DO phase plots colored by month (5×3) ==========
# Seasonal 12-color palette (color-blind mindful)
season_anchor <- c("#2C7BB6", "#1B9E77", "#FEC44F", "#984EA3")  # Winter→Spring→Summer→Fall
season_pal_12 <- grDevices::colorRampPalette(season_anchor)(12)
names(season_pal_12) <- month.abb

# Build a single plot with legend to extract
build_ph_do_month_plot <- function(df, nm, show_legend = FALSE) {
  if (!all(c("ph_T","do_mgl","datetime_utc") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no pH/DO)")))
  }
  df2 <- df %>%
    filter(!is.na(ph_T), !is.na(do_mgl), !is.na(datetime_utc)) %>%
    mutate(month = factor(month(datetime_utc), levels = 1:12, labels = month.abb, ordered = FALSE))
  ggplot(df2, aes(x = do_mgl, y = ph_T, color = month)) +
    geom_point(alpha = 0.15, size = 0.3, na.rm = TRUE) +
    scale_color_manual(values = season_pal_12, name = "Month", drop = FALSE, guide = if (show_legend) "legend" else "none") +
    coord_cartesian(xlim = c(0, 15), ylim = c(6, 9)) +
    labs(x = "Dissolved Oxygen (mg/L)", y = "pH (total-scale)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
}

# Extract shared legend from one representative NEP
first_nm <- names(data_list)[[1]]
legend_plot <- build_ph_do_month_plot(data_list[[first_nm]], first_nm, show_legend = TRUE)
legend_g <- cowplot::get_legend(legend_plot + theme(legend.position = "bottom"))

# Build all panels without legends
plots_ph_do_month <- imap(data_list, ~ build_ph_do_month_plot(.x, .y, show_legend = FALSE))
grid_ph_do_month <- cowplot::plot_grid(plotlist = plots_ph_do_month, ncol = 5, align = "hv")
final_ph_do_month <- cowplot::plot_grid(grid_ph_do_month, legend_g, ncol = 1, rel_heights = c(1, 0.08))

ggsave("fig_ph_do_phase_month_5x3.png", final_ph_do_month, width = 20, height = 13, dpi = 300, bg = "white")
# ========== 11b) pH–DO phase plots colored by temperature quartiles (5×3) ==========

# Build one plot for legend extraction
build_ph_do_tquart_plot <- function(df, nm, show_legend = FALSE) {
  if (!all(c("ph_T","do_mgl","temp_c") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no pH/DO/Temp)")))
  }
  df2 <- df %>%
    filter(!is.na(ph_T), !is.na(do_mgl), !is.na(temp_c))
  qs <- quantile(df2$temp_c, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  df2 <- df2 %>%
    mutate(tq = cut(temp_c, qs, include.lowest = TRUE, labels = c("Q1","Q2","Q3","Q4")))
  ggplot(df2, aes(x = do_mgl, y = ph_T, color = tq)) +
    geom_point(alpha = 0.15, size = 0.3, na.rm = TRUE) +
    scale_color_brewer(palette = "Set1", name = "Temperature\nquartile", guide = if (show_legend) "legend" else "none") +
    coord_cartesian(xlim = c(0, 15), ylim = c(6, 9)) +
    labs(x = "Dissolved Oxygen (mg/L)", y = "pH (total-scale)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
}

legend_plot_q <- build_ph_do_tquart_plot(data_list[[first_nm]], first_nm, show_legend = TRUE)
legend_q <- cowplot::get_legend(legend_plot_q + theme(legend.position = "bottom"))

plots_ph_do_tquart <- imap(data_list, ~ build_ph_do_tquart_plot(.x, .y, show_legend = FALSE))
grid_ph_do_tquart <- cowplot::plot_grid(plotlist = plots_ph_do_tquart, ncol = 5, align = "hv")
final_ph_do_tquart <- cowplot::plot_grid(grid_ph_do_tquart, legend_q, ncol = 1, rel_heights = c(1, 0.08))

ggsave("fig_ph_do_phase_tquart_5x3.png", final_ph_do_tquart, width = 20, height = 13, dpi = 300, bg = "white")

# original
plots_ph_do_tquart <- imap(data_list, ~ {
  df <- .x; nm <- .y
  if (!all(c("ph_T","do_mgl","temp_c") %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle(paste(nm, "(no pH/DO/Temp)")))
  }
  df2 <- df %>%
    filter(!is.na(ph_T), !is.na(do_mgl), !is.na(temp_c))
  qs <- quantile(df2$temp_c, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  df2 <- df2 %>%
    mutate(tq = cut(temp_c, qs, include.lowest = TRUE, labels = c("Q1","Q2","Q3","Q4")))
  ggplot(df2, aes(x = do_mgl, y = ph_T, color = tq)) +
    geom_point(alpha = 0.15, size = 0.3, na.rm = TRUE) +
    scale_color_brewer(palette = "Set1", name = "Temp quartile") +
    coord_cartesian(xlim = c(0, 15), ylim = c(6, 9)) +
    labs(x = "Dissolved Oxygen (mg/L)", y = "pH (total-scale)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"),
          legend.position = "none")
})
grid_ph_do_tquart <- cowplot::plot_grid(plotlist = plots_ph_do_tquart, ncol = 5, align = "hv")
ggsave("fig_ph_do_phase_tquart_5x3.png", grid_ph_do_tquart, width = 20, height = 12, dpi = 300, bg = "white")

# ========== 14) pH–pCO2 plots for NEPs with both ==========

# v3: add in Tillamook
detect_pco2_col <- function(df) {
  if ("pco2_uatm" %in% names(df)) return("pco2_uatm")
  if ("pco2_ppm" %in% names(df))  return("pco2_ppm")
  if ("co2_ppm" %in% names(df))   return("co2_ppm")
  NULL
}

# Convenience: detect the unit label from the chosen pCO2 column
pco2_unit_label <- function(colname) {
  if (is.null(colname)) return(NULL)
  if (identical(colname, "pco2_uatm")) "µatm" else "ppm"
}

plots_ph_pco2 <- purrr::imap(data_list, ~ {
  df <- .x; nm <- .y
  pco2_col <- detect_pco2_col(df)
  if (is.null(pco2_col) || !("ph_T" %in% names(df))) return(NULL)
  df2 <- df %>% dplyr::filter(!is.na(.data[[pco2_col]]), !is.na(ph_T))
  if (nrow(df2) == 0) return(NULL)
  unit_lab <- pco2_unit_label(pco2_col)
  x_lab <- if (!is.null(unit_lab)) paste0("pCO2 (", unit_lab, ")") else "pCO2"
  
  ggplot(df2, aes(x = .data[[pco2_col]], y = ph_T)) +
    stat_binhex(bins = 40, na.rm = TRUE) +
    scale_fill_viridis_c(option = "C", trans = "log10", name = "count", na.value = "grey90") +
    coord_cartesian(ylim = c(6, 9),xlim=c(0,3500)) +
    labs(x = x_lab, y = "pH (total-scale)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})
plots_ph_pco2 <- purrr::compact(plots_ph_pco2)

if (length(plots_ph_pco2) > 0) {
  # choose a compact grid (same logic as before)
  choose_ncol <- function(n) if (n >= 13) 5 else if (n >= 10) 4 else if (n >= 7) 3 else if (n >= 4) 3 else 1
  n <- length(plots_ph_pco2); ncol <- choose_ncol(n); nrow <- ceiling(n / ncol)
  grid_ph_pco2 <- cowplot::plot_grid(plotlist = plots_ph_pco2, ncol = ncol, align = "hv")
  ggsave("fig_ph_pco2_compact.png", grid_ph_pco2, width = ncol * 4, height = nrow * 3.5, dpi = 300, bg = "white")
}

# v2:
plots_ph_pco2 <- imap(data_list, ~ {
  df <- .x; nm <- .y
  pco2_col <- detect_pco2_col(df)
  if (is.null(pco2_col) || !("ph_T" %in% names(df))) return(NULL)
  df2 <- df %>% filter(!is.na(.data[[pco2_col]]), !is.na(ph_T))
  if (nrow(df2) == 0) return(NULL)
  ggplot(df2, aes(x = .data[[pco2_col]], y = ph_T)) +
    stat_binhex(bins = 40, na.rm = TRUE) +
    scale_fill_viridis_c(option = "C", trans = "log10", name = "count", na.value = "grey90") +
    coord_cartesian(ylim = c(6, 9)) +
    labs(x = "pCO2 (ppm)", y = "pH (total-scale)", title = nm) +
    xlim(0,3000)+
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})
plots_ph_pco2 <- purrr::compact(plots_ph_pco2)
n <- length(plots_ph_pco2)
if (n > 0) {
  # Choose a compact grid (up to 5 columns; otherwise try to square it)
  choose_ncol <- function(n) {
    if (n >= 13) 5 else if (n >= 10) 4 else if (n >= 7) 3 else if (n >= 4) 2 else 1
  }
  ncol <- choose_ncol(n)
  nrow <- ceiling(n / ncol)
  
  grid_ph_pco2 <- cowplot::plot_grid(plotlist = plots_ph_pco2, ncol = ncol, align = "hv")
  
  # Size proportional to grid to reduce white space
  unit_w <- 4    # inches per column
  unit_h <- 3.5  # inches per row
  width  <- ncol * unit_w
  height <- nrow * unit_h
  
  ggsave("fig_ph_pco2_compact.png", grid_ph_pco2, width = width, height = height, dpi = 300, bg = "white")
} 
# original: 
plots_ph_pco2 <- imap(data_list, ~ {
  df <- .x; nm <- .y
  pco2_col <- detect_pco2_col(df)
  if (is.null(pco2_col) || !("ph_T" %in% names(df))) return(NULL)
  df2 <- df %>% filter(!is.na(.data[[pco2_col]]), !is.na(ph_T))
  if (nrow(df2) == 0) return(NULL)
  ggplot(df2, aes(x = .data[[pco2_col]], y = ph_T)) +
    stat_binhex(bins = 40, na.rm = TRUE) +
    scale_fill_viridis_c(option = "C", trans = "log10", name = "count", na.value = "grey90") +
    coord_cartesian(ylim = c(6, 9)) +
    labs(x = "pCO2 (ppm)", y = "pH (total-scale)", title = nm) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 10, face = "bold"))
})
plots_ph_pco2 <- compact(plots_ph_pco2)  # drop NEPs without pCO2/pH
grid_ph_pco2 <- cowplot::plot_grid(plotlist = plots_ph_pco2, ncol = 5, align = "hv")
ggsave("fig_ph_pco2_5x3.png", grid_ph_pco2, width = 28, height = 12, dpi = 300, bg = "white")

  
  

# =========== Additional plots: ----
# Optional: cap the number of points sampled per NEP for plotting speed (set to Inf to keep all)
sample_per_nep <- 1000000  # e.g., draw up to 50k points per NEP; increase/decrease as needed

# Helper: detect a pCO2 column in a data frame
detect_pco2_col <- function(df) {
  if ("pco2_ppm" %in% names(df)) "pco2_ppm" else if ("co2_ppm" %in% names(df)) "co2_ppm" else NULL
}

# Build a single long data frame for jitter plotting for a given variable
# var_name can be "ph_T" or "pco2" (special case that detects per-NEP column)
build_jitter_df <- function(data_list, var_name) {
  out <- imap(data_list, function(df, nep_nm) {
    # Determine the column to use
    if (var_name == "pco2") {
      vcol <- detect_pco2_col(df)
      if (is.null(vcol)) return(NULL)
    } else {
      vcol <- var_name
      if (!(vcol %in% names(df))) return(NULL)
    }
    
    # Pull values; drop NAs and keep finite
    vals <- df[[vcol]]
    if (is.null(vals)) return(NULL)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(NULL)
    
    # Optional sampling for speed
    n <- length(vals)
    if (is.finite(sample_per_nep) && n > sample_per_nep) {
      idx <- sample.int(n, sample_per_nep)
      vals <- vals[idx]
    }
    
    tibble(NEP = nep_nm, value = vals)
  }) %>% bind_rows()
  
  if (is.null(out) || nrow(out) == 0) return(tibble(NEP = character(), value = numeric()))
  # Order NEPs alphabetically (or keep list order if you prefer: levels = names(data_list))
  out %>% mutate(NEP = factor(NEP, levels = sort(unique(NEP))))
}

# Plot function: jitter + boxplot
plot_nep_jitter_box <- function(df, ylab, y_limits = NULL,
                                point_alpha = 0.15, point_size = 0.6,
                                jitter_width = 0.25, box_color = "#333333", box_fill=NA, point_color = "grey20",
                                title = NULL) {
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + ggtitle("No data"))
  }
  
  p <- ggplot(df, aes(x = NEP, y = value)) +
    # Jittered points (behind boxplot)
    geom_jitter(width = jitter_width, height = 0, alpha = point_alpha,
                size = point_size, color = point_color, stroke = 0) +
    # Boxplot (IQR + median), no outliers (points already show full distribution)
    geom_boxplot(width = 0.5, outlier.shape = NA, color = box_color, fill = NA, linewidth = 0.6) +
    labs(x = NULL, y = ylab, title = title) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  p
}

# 1) pH (total-scale)
ph_df <- build_jitter_df(data_list, "ph_T")
p_ph <- plot_nep_jitter_box(
  ph_df,
  ylab = "pH (total-scale)",
  y_limits = c(6, 9),        # standard range for comparability
  title = "pH by NEP (jitter + IQR/median)"
)

# 2) pCO2 (ppm) — auto-detects column per NEP
pco2_df <- build_jitter_df(data_list, "pco2")
p_pco2 <- plot_nep_jitter_box(
  pco2_df,
  ylab = "pCO2 (ppm)",
  y_limits = NULL,           # set e.g., c(0, 2000) if you want a fixed axis
  title = "pCO2 by NEP (jitter + IQR/median)"
)

# 1) Neutral boxes
p_ph   <- plot_nep_jitter_box(ph_df,  "pH (total-scale)", y_limits = c(6, 9),
                              box_color = "#333333", box_fill = NA,
                              title = "pH by NEP (jitter + IQR/median)")
ggsave("fig_nep_jitter_box_ph.png",  p_ph,   width = 14, height = 8, dpi = 300, bg = "white")


# 2) Variable-matched outlines, no fill
p_ph   <- plot_nep_jitter_box(ph_df,  "pH (total-scale)", y_limits = c(6, 9),
                              box_color = "#6A3D9A", box_fill = NA,
                              title = "pH by NEP (jitter + IQR/median)")

p_pco2 <- plot_nep_jitter_box(pco2_df, "pCO2 (ppm)",
                              box_color = "#E31A1C", box_fill = NA,
                              title = "pCO2 by NEP (jitter + IQR/median)")

# 3) Variable-matched outlines + soft fill
p_ph   <- plot_nep_jitter_box(ph_df,  "pH (total-scale)", y_limits = c(6, 9),
                              box_color = "#6A3D9A", box_fill = alpha("#6A3D9A", 0.15),
                              title = "pH by NEP (jitter + IQR/median)")
ggsave("fig_nep_jitter_box_ph_purp.png",  p_ph,   width = 14, height = 8, dpi = 300, bg = "white")

p_pco2 <- plot_nep_jitter_box(pco2_df, "pCO2 (ppm)",
                              box_color = "#E31A1C", box_fill = alpha("#E31A1C", 0.15),
                              title = "pCO2 by NEP (jitter + IQR/median)")
ggsave("fig_nep_jitter_box_pco2_red.png", p_pco2, width = 14, height = 8, dpi = 300, bg = "white")


# Display
# print(p_ph)
# print(p_pco2)

# Save
ggsave("fig_nep_jitter_box_ph.png",  p_ph,   width = 14, height = 8, dpi = 300, bg = "white")
ggsave("fig_nep_jitter_box_pco2.png", p_pco2, width = 14, height = 8, dpi = 300, bg = "white")
# =========== Faceted Tillamook Time Series with pCO2 added: ----
# Detect a pCO2 column and its unit label
detect_pco2_col <- function(df) {
  if ("pco2_uatm" %in% names(df)) return("pco2_uatm")
  if ("pco2_ppm" %in% names(df))  return("pco2_ppm")
  if ("co2_ppm" %in% names(df))   return("co2_ppm")
  NULL
}
pco2_unit_label <- function(colname) {
  if (is.null(colname)) return(NULL)
  if (identical(colname, "pco2_uatm")) "µatm" else "ppm"
}

# Variable colors (consistent with your figures)
var_cols <- c(
  temp_c  = "#D55E00",  # Temperature
  sal_ppt = "#0072B2",  # Salinity
  ph_T    = "#6A3D9A",  # pH
  do_mgl  = "#1B9E77",  # DO
  pco2    = "#E31A1C"   # pCO2
)

# Faceted, colored time series for one NEP (points colored by variable)
make_faceted_ts_one_colored <- function(df, nep_name, datetime_col = "datetime_utc",
                                        point_size = 0.35, point_alpha = 0.35) {
  if (!(datetime_col %in% names(df)) || !inherits(df[[datetime_col]], "POSIXt")) {
    stop("'", datetime_col, "' must exist and be POSIXct/POSIXlt")
  }
  
  # Detect and normalize pCO2 to a 'pco2' column, with unit-aware label
  pco2_col <- detect_pco2_col(df)
  df2 <- df
  if (!is.null(pco2_col)) df2 <- df2 %>% mutate(pco2 = .data[[pco2_col]])
  
  vars_all   <- c("temp_c","sal_ppt","ph_T","do_mgl","pco2")
  vars_avail <- intersect(vars_all, names(df2))
  
  # Labels with pCO2 units if present
  pco2_lab <- if (is.null(pco2_col)) "pCO2" else paste0("pCO2 (", pco2_unit_label(pco2_col), ")")
  var_labels <- c(
    temp_c  = "Temperature (°C)",
    sal_ppt = "Salinity (ppt)",
    ph_T    = "pH (total-scale)",
    do_mgl  = "Dissolved Oxygen (mg/L)",
    pco2    = pco2_lab
  )
  
  # Order facets (top→bottom): Temp, Sal, pH, DO, pCO2
  facet_order <- vars_all[vars_all %in% vars_avail]
  
  long_df <- df2 %>%
    select(all_of(c(datetime_col, vars_avail))) %>%
    pivot_longer(cols = all_of(vars_avail),
                 names_to = "variable", values_to = "value") %>%
    filter(!is.na(.data[[datetime_col]]), is.finite(value)) %>%
    mutate(variable = factor(variable, levels = facet_order))
  
  if (nrow(long_df) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste(nep_name, "(no data)")))
  }
  
  ggplot(long_df, aes(x = .data[[datetime_col]], y = value, color = variable)) +
    geom_scattermore(size = point_size, alpha = point_alpha) +
    scale_color_manual(values = var_cols, guide = "none") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                     expand = expansion(mult = c(0.01, 0.01))) +
    facet_wrap(~ variable, ncol = 1, scales = "free_y",
               labeller = as_labeller(var_labels)) +
    labs(x = NULL, y = NULL, title = paste0(nep_name, " — Faceted time series")) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      strip.background   = element_rect(fill = "grey95"),
      plot.title         = element_text(face = "bold")
    )
}

# If your list is named nep_unfiltered_data (or nep_filtered_data), and the key is "Tillamook":
p_tilla_col <- make_faceted_ts_one_colored(
  nep_filtered_data[["Tillamook"]],
  nep_name = "Tillamook",
  datetime_col = "datetime_utc"
)

# print(p_tilla_col)
ggsave("fig_tillamook_faceted_timeseries_colored.png", p_tilla_col,
       width = 10, height = 10, dpi = 300, bg = "white")
