# Export sf_nep_cma and sf_nep_eso after running sfbay_master to create zoomable plots

# Ensure datetime_utc is written as "YYYY-MM-DD HH:MM:SS" (UTC), e.g., 00:00:00 for midnight
export_df <- sf_nep_cma
export_df$datetime_utc <- format(
  as.POSIXct(export_df$datetime_utc, tz = "UTC"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

# Write to CSV (no row names)
write.csv(export_df, file = "sf_nep_cma_260723.csv", row.names = FALSE, na = "")


# Ensure datetime_utc is written as "YYYY-MM-DD HH:MM:SS" (UTC), e.g., 00:00:00 for midnight
export_df <- sf_nep_eos
export_df$datetime_utc <- format(
  as.POSIXct(export_df$datetime_utc, tz = "UTC"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

# Write to CSV (no row names)
write.csv(export_df, file = "sf_nep_eos_260723.csv", row.names = FALSE, na = "")