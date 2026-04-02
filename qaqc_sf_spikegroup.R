# Adds additional flagging for San Francisco data:
# assigns flags based on whether or not data occurred between two spike test suspect or fails over a designated 'window' of observations
# if the points do fall between two spikes over the window, then they will be given a suspect flag under the newly-created 'flag_max' 
# 'flag_max' is incorporated into the 'flags_manual' column in sfbay_manual_qaqc.R

# Flag interior points between two '2' flags that occur within a given window
flag_clusters_between_twos <- function(df,
                                       col = "test_Spike",
                                       window = 10,
                                       suspect_flag = "2",
                                       protect_flags = "3",     # e.g., c("3","4") if you don't want to overwrite harder fails
                                       out_col = paste0(col, "_clustered"),
                                       verbose = TRUE) {
  # Pull column
  v <- df[[col]]
  original <- v
  
  # Coerce to character for editing
  v_chr <- as.character(v)
  
  # Indices currently flagged as suspect_flag
  idx2 <- which(v_chr == suspect_flag)
  
  if (length(idx2) >= 2) {
    # Find consecutive pairs of suspect flags close in row index
    gaps <- diff(idx2)
    close_pairs <- which(gaps <= window)
    
    if (length(close_pairs) > 0) {
      # Build a set of indices to fill (all indices between each close pair, inclusive)
      fill_idx <- unique(unlist(Map(seq, idx2[close_pairs], idx2[close_pairs + 1])))
      
      # Optionally protect certain flags from being overwritten
      if (!is.null(protect_flags) && length(protect_flags) > 0) {
        keep_mask <- is.na(v_chr[fill_idx]) | !(v_chr[fill_idx] %in% protect_flags)
        fill_idx <- fill_idx[keep_mask]
      }
      
      # Apply suspect flag to all interior indices
      v_chr[fill_idx] <- suspect_flag
    }
  }
  
  # Write back with the original type
  if (is.factor(original)) {
    # Ensure suspect_flag is in the levels
    new_levels <- union(levels(original), suspect_flag)
    df[[out_col]] <- factor(v_chr, levels = new_levels)
  } else if (is.numeric(original)) {
    suppressWarnings(df[[out_col]] <- as.numeric(v_chr))
  } else if (is.integer(original)) {
    suppressWarnings(df[[out_col]] <- as.integer(v_chr))
  } else {
    df[[out_col]] <- v_chr
  }
  
  if (verbose) {
    before <- sum(as.character(original) == suspect_flag, na.rm = TRUE)
    after  <- sum(as.character(df[[out_col]]) == suspect_flag, na.rm = TRUE)
    message(sprintf("Suspect ('%s') count: before = %s, after = %s (Δ = %+d)",
                    suspect_flag, before, after, after - before))
  }
  
  df
}

# Apply to CMA data
# - Writes to a new column to preserve the original flags
# 3) Split stations to ease review (CMA and EOS)
# --------------------------------------------
sf_nep_cma <- sf_recombined %>% filter(site_code == "CMA")
sf_nep_cma <- flag_clusters_between_twos(
  df = sf_nep_cma,
  col = "test_Spike",
  window = 10,              # within 10 observations of each other
  suspect_flag = "2",      # suspect flag
  protect_flags = "3",    # avoid overwriting "fail" points with suspect
  out_col = "test_Spike_clustered",
  verbose = TRUE
)

c1 <- sf_nep_cma$flags_2026
c2 <- sf_nep_cma$test_Spike_clustered 

sf_nep_cma$flag_max <- pmax(c1, c2, na.rm = TRUE)
sf_nep_cma$flag_max[is.infinite(sf_nep_cma$flag_max)] <- NA_real_  # both NA -> NA
#sf_nep_cma$flags_2026 <- sf_nep_cma$flag_max

# Apply to EOS data
# - Writes to a new column to preserve the original flags
# --------------------------------------------
sf_nep_eos <- sf_recombined %>% filter(site_code == "EOS")
sf_nep_eos <- flag_clusters_between_twos(
  df = sf_nep_eos,
  col = "test_Spike",
  window = 10,              # within 10 observations of each other
  suspect_flag = "2",      # suspect flag
  protect_flags = "3",    # avoid overwriting "fail" points with suspect
  out_col = "test_Spike_clustered",
  verbose = TRUE
)

c1 <- sf_nep_eos$flags_2026
c2 <- sf_nep_eos$test_Spike_clustered 

sf_nep_eos$flag_max <- pmax(c1, c2, na.rm = TRUE)
sf_nep_eos$flag_max[is.infinite(sf_nep_eos$flag_max)] <- NA_real_  # both NA -> NA
