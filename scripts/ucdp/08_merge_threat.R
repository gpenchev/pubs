# =============================================================================
# 08_merge_threat.R
# Merge threat scores into the main panel
# Input:  panel_eurostat.rds + threat_scores.rds
# Output: panel_full.rds
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load data ----------------------------------------------------------------
panel  <- readRDS(file.path(path_data, "panel_eurostat.rds"))
threat <- readRDS(file.path(path_data, "threat_scores.rds"))

# --- Merge --------------------------------------------------------------------
panel_full <- panel %>%
  dplyr::left_join(
    threat %>%
      dplyr::select(
        country,
        year,
        threat_score,
        threat_score_log,
        threat_score_z,
        threat_score_centroid,
        threat_centroid_log,
        threat_centroid_z,
        threat_land,
        threat_land_log,
        threat_land_z
      ),
    by = c("country", "year")
  )

# --- Verify no rows lost ------------------------------------------------------
stopifnot(nrow(panel_full) == nrow(panel))

# --- Validate threat score ranges ---------------------------------------------
n_neg_land <- sum(panel_full$threat_land_log < 0, na.rm = TRUE)
n_inf_land <- sum(!is.finite(panel_full$threat_land_log) &
                    !is.na(panel_full$threat_land_log))
n_neg_all  <- sum(panel_full$threat_score_log < 0, na.rm = TRUE)

if (n_neg_land > 0) {
  warning(n_neg_land, " rows have threat_land_log < 0 — check distance computation.")
}
if (n_inf_land > 0) {
  warning(n_inf_land, " rows have non-finite threat_land_log — check for Inf/NaN.")
}
if (n_neg_all > 0) {
  warning(n_neg_all, " rows have threat_score_log < 0 — check distance computation.")
}

message("Threat score validation:")
message("  threat_land_log  — min: ", round(min(panel_full$threat_land_log,  na.rm = TRUE), 4),
        ", max: ", round(max(panel_full$threat_land_log,  na.rm = TRUE), 4),
        ", NA: ",  sum(is.na(panel_full$threat_land_log)))
message("  threat_score_log — min: ", round(min(panel_full$threat_score_log, na.rm = TRUE), 4),
        ", max: ", round(max(panel_full$threat_score_log, na.rm = TRUE), 4),
        ", NA: ",  sum(is.na(panel_full$threat_score_log)))

# --- Country-level threat summary for evaluation ------------------------------
# Saved to quality_reports/ so the pipeline evaluation phase can assess
# per-country threat score distribution, zero-rate, and divergence between
# the border-based (primary) and centroid-based (robustness) measures.

threat_country_summary <- panel_full %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    n_years          = dplyr::n(),
    mean_land_log    = round(mean(threat_land_log,    na.rm = TRUE), 4),
    sd_land_log      = round(sd(threat_land_log,      na.rm = TRUE), 4),
    min_land_log     = round(min(threat_land_log,     na.rm = TRUE), 4),
    max_land_log     = round(max(threat_land_log,     na.rm = TRUE), 4),
    n_zero_land      = sum(threat_land    == 0, na.rm = TRUE),
    pct_zero_land    = round(100 * mean(threat_land   == 0, na.rm = TRUE), 1),
    mean_score_log   = round(mean(threat_score_log,   na.rm = TRUE), 4),
    sd_score_log     = round(sd(threat_score_log,     na.rm = TRUE), 4),
    divergence_mean  = round(mean(abs(threat_land_log - threat_score_log),
                                  na.rm = TRUE), 4),
    .groups          = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_land_log))

readr::write_csv(
  threat_country_summary,
  file.path(path_reports, "threat_country_summary.csv")
)

message("Threat country summary saved to quality_reports/threat_country_summary.csv")
message("Countries with >50% zero threat_land: ",
  paste(
    threat_country_summary$country[threat_country_summary$pct_zero_land > 50],
    collapse = ", "
  )
)

# --- Save ---------------------------------------------------------------------
saveRDS(panel_full, file.path(path_data, "panel_full.rds"))
readr::write_csv(panel_full, file.path(path_data, "panel_full.csv"))

message("Script 08 complete: panel_full has ",
        nrow(panel_full), " rows and ",
        ncol(panel_full), " columns.")
