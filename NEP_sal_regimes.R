# Assigning Salinity Regimes to NEP Sites

library(dplyr)
library(purrr)
library(rlang)
library(viridisLite)

# Define salinity regime levels (ordered fresh -> saline)
regime_levels <- c("Oligo-Mesohaline", "Polyhaline", "Euhaline")

# Helper: classify a numeric salinity median into a regime
classify_sal_regime <- function(sal_median) {
  case_when(
    # sal_median < 0.5                      ~ "Tidal fresh",
    # sal_median >= 0.5  & sal_median < 5   ~ "Oligohaline",
     sal_median < 18  ~ "Oligo-Mesohaline",
    sal_median >= 18   & sal_median <= 30 ~ "Polyhaline",  # includes 30.0
    sal_median > 30                       ~ "Euhaline",
    TRUE ~ NA_character_
  )
}

# 1) Build a site -> sal_class_median map for each NEP using nep_filtered_data
compute_site_class_map <- function(df, sal_col = "sal_ppt", site_col = "site_code") {
  if (!(sal_col %in% names(df)) || !(site_col %in% names(df))) {
    return(tibble(!!site_col := character(), sal_class_median = factor(character(), levels = regime_levels)))
  }
  
  df %>%
    filter(!is.na(.data[[site_col]]), !is.na(.data[[sal_col]])) %>%
    group_by(.data[[site_col]]) %>%
    summarize(site_sal_med = median(.data[[sal_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      sal_class_median = factor(classify_sal_regime(site_sal_med), levels = regime_levels)
    ) %>%
    select(all_of(site_col), sal_class_median)
}

# Compute maps for all NEPs (quality-filtered data)
sal_maps <- imap(nep_filtered_data, ~ compute_site_class_map(.x, sal_col = "sal_ppt", site_col = "site_code"))

# 2) Apply maps to BOTH lists: nep_filtered_data and nep_unfiltered_data
add_sal_class_to_df <- function(df, map_tbl, site_col = "site_code") {
  if (nrow(map_tbl) == 0 || !(site_col %in% names(df))) {
    df$sal_class_median <- factor(NA_character_, levels = regime_levels)
    return(df)
  }
  out <- df %>%
    left_join(map_tbl, by = setNames("site_code", site_col))
  # Ensure factor levels are consistent
  out$sal_class_median <- factor(out$sal_class_median, levels = regime_levels)
  out
}

nep_filtered_data <- imap(nep_filtered_data, ~ add_sal_class_to_df(.x, sal_maps[[.y]], site_col = "site_code"))
nep_unfiltered_data <- imap(nep_unfiltered_data, ~ add_sal_class_to_df(.x, sal_maps[[.y]], site_col = "site_code"))

# # Optional: diagnostics — how many sites per regime per NEP (unfiltered coverage)
# diag_counts <- imap(nep_unfiltered_data, ~ {
#   .x %>%
#     distinct(site_code, sal_class_median) %>%
#     count(sal_class_median, name = "n_sites") %>%
#     mutate(NEP = .y)
# }) %>% bind_rows()


#### << ----- Adding a dynamic salinity regime: calculated based on each row's sal value ----
# install.packages("data.table")  # if needed
library(data.table)

regime_levels_3 <- c("Oligo-mesohaline", "Polyhaline", "Euhaline")

add_sal_class_dynamic_dt <- function(df,
                                     sal_col = "sal_ppt",
                                     out_col = "sal_class_dynamic") {
  if (!sal_col %in% names(df)) {
    df[[out_col]] <- factor(rep(NA_character_, nrow(df)), levels = regime_levels_3)
    return(df)
  }
  DT <- as.data.table(df)  # creates a shallow data.table wrapper
  DT[, (out_col) := fcase(
    is.na(get(sal_col)),                NA_character_,
    get(sal_col) < 18,                  "Oligo-mesohaline",
    get(sal_col) <= 30,                 "Polyhaline",   # 18–30 inclusive
    get(sal_col) > 30,                  "Euhaline"
  )]
  DT[, (out_col) := factor(get(out_col), levels = regime_levels_3)]
  as.data.frame(DT)
}

# Apply to your lists (choose one or both):
nep_unfiltered_data <- lapply(nep_unfiltered_data, add_sal_class_dynamic_dt)
nep_filtered_data  <- lapply(nep_filtered_data,  add_sal_class_dynamic_dt)

