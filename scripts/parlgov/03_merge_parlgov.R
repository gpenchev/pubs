# =============================================================================
# 03_merge_parlgov.R
# Merge ParlGov political variables into main panel
# Input:  panel_full.rds + parlgov_country_year.rds
# Output: panel_full.rds (updated with political variables)
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load data ----------------------------------------------------------------
panel   <- readRDS(file.path(path_data,    "panel_full.rds"))
parlgov <- readRDS(file.path(path_parlgov, "parlgov_country_year.rds"))

n_before <- nrow(panel)

# --- Select variables to merge ------------------------------------------------
parlgov_merge <- parlgov %>%
  dplyr::select(
    country,
    year,
    gov_left_right,
    gov_eu_position,
    election_year,
    any_lr_imputed,
    any_eu_imputed
  )

# --- Pre-join duplicate check -------------------------------------------------
if (anyDuplicated(parlgov_merge[, c("country", "year")])) {
  stop("Duplicate country-year rows detected in parlgov_merge — ",
       "check 02_process_parlgov.R expand_cabinet_to_years output.")
}

# --- Left join ----------------------------------------------------------------
panel_full <- panel %>%
  dplyr::left_join(parlgov_merge, by = c("country", "year"))

# --- Verify no rows lost ------------------------------------------------------
stopifnot(nrow(panel_full) == n_before)

# --- Coverage summary ---------------------------------------------------------
coverage <- panel_full %>%
  dplyr::summarise(
    n_total          = dplyr::n(),
    n_lr_present     = sum(!is.na(gov_left_right)),
    n_eu_present     = sum(!is.na(gov_eu_position)),
    n_election_years = sum(election_year, na.rm = TRUE),
    pct_lr           = round(100 * n_lr_present / n_total, 1),
    pct_eu           = round(100 * n_eu_present / n_total, 1)
  )

message("Panel political variable coverage:")
message("  gov_left_right:  ", coverage$pct_lr, "% (", coverage$n_lr_present, " rows)")
message("  gov_eu_position: ", coverage$pct_eu, "% (", coverage$n_eu_present, " rows)")
message("  election years:  ", coverage$n_election_years, " country-years flagged")

# --- Save ---------------------------------------------------------------------
saveRDS(panel_full, file.path(path_data, "panel_full.rds"))
readr::write_csv(panel_full, file.path(path_data, "panel_full.csv"))

message("Script 03_merge_parlgov complete.")
message("  panel_full has ", nrow(panel_full), " rows and ", ncol(panel_full), " columns.")
