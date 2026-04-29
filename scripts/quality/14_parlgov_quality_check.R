# =============================================================================
# 14_parlgov_quality_check.R
# Quality check for political variables added by 03_merge_parlgov.R
#
# Checks performed:
#   1. Presence of required political variables in panel_full.rds
#   2. NaN detection in gov_left_right and gov_eu_position
#      (NaN arises if a cabinet has zero total seats after filtering,
#       causing 0/0 in the seat-weighted mean)
#   3. Coverage by country (% of years with non-NA values)
#   4. Coverage by year (number of countries with non-NA values)
#   5. Range validation (ParlGov scale: 0-10 for both variables)
#
# Output:
#   scripts/output/quality_reports/parlgov_quality_report.csv
# =============================================================================

source(here::here("scripts", "00_setup.R"))

panel <- readRDS(file.path(path_data, "panel_full.rds"))

# =============================================================================
# Check 1: Variable presence
# =============================================================================
political_vars <- c("gov_left_right", "gov_eu_position", "election_year")

missing_vars <- setdiff(political_vars, names(panel))
if (length(missing_vars) > 0) {
  stop("Political variables not found in panel_full.rds: ",
       paste(missing_vars, collapse = ", "),
       "\nEnsure 03_merge_parlgov.R has been run before this script.")
}

message("Variable presence check passed: all political variables found.")

# =============================================================================
# Check 2: NaN detection
# NaN arises from 0/0 in the seat-weighted mean when a cabinet has zero
# total seats after filtering. This should not occur if 02_process_parlgov.R
# correctly filters to seats > 0 before computing weighted means.
# =============================================================================
nan_counts <- panel %>%
  dplyr::summarise(
    nan_lr = sum(is.nan(gov_left_right)),
    nan_eu = sum(is.nan(gov_eu_position))
  )

if (nan_counts$nan_lr > 0 || nan_counts$nan_eu > 0) {
  warning("NaN values detected in political variables: ",
          "gov_left_right NaN=", nan_counts$nan_lr,
          ", gov_eu_position NaN=", nan_counts$nan_eu,
          ". Check 02_process_parlgov.R seat-weighted mean computation.")
} else {
  message("NaN check passed: no NaN values in political variables.")
}

# =============================================================================
# Check 3: Coverage by country
# Countries with < 80% coverage are flagged as potentially problematic.
# Low coverage may indicate missing ParlGov data for that country or
# gaps in the cabinet spell carry-forward logic.
# =============================================================================
coverage <- panel %>%
  dplyr::group_by(country, country_name) %>%
  dplyr::summarise(
    n_years        = dplyr::n(),
    n_lr_missing   = sum(is.na(gov_left_right)),
    n_eu_missing   = sum(is.na(gov_eu_position)),
    n_elections    = sum(election_year, na.rm = TRUE),
    pct_lr_covered = round(100 * (1 - n_lr_missing / n_years), 1),
    pct_eu_covered = round(100 * (1 - n_eu_missing / n_years), 1),
    .groups        = "drop"
  ) %>%
  dplyr::arrange(pct_lr_covered)

message("Political variable coverage by country:")
print(coverage)

poor_lr <- coverage %>% dplyr::filter(pct_lr_covered < 80)
poor_eu <- coverage %>% dplyr::filter(pct_eu_covered < 80)

if (nrow(poor_lr) > 0) {
  warning("Countries with < 80% gov_left_right coverage: ",
          paste(poor_lr$country, collapse = ", "))
}
if (nrow(poor_eu) > 0) {
  warning("Countries with < 80% gov_eu_position coverage: ",
          paste(poor_eu$country, collapse = ", "))
}

# =============================================================================
# Check 4: Coverage by year
# Useful for identifying years where ParlGov data is sparse across countries,
# which may affect the reliability of year fixed effects in regression.
# =============================================================================
coverage_year <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_lr_present = sum(!is.na(gov_left_right)),
    n_eu_present = sum(!is.na(gov_eu_position)),
    .groups      = "drop"
  )

message("Political variable coverage by year (first and last 5 years):")
print(head(coverage_year, 5))
print(tail(coverage_year, 5))

# =============================================================================
# Check 5: Range validation
# ParlGov scores are on a 0-10 scale for both left-right and EU position.
# Values outside this range indicate a data processing error.
# =============================================================================
lr_range <- range(panel$gov_left_right, na.rm = TRUE)
eu_range <- range(panel$gov_eu_position, na.rm = TRUE)

message("gov_left_right range: [", round(lr_range[1], 2),
        ", ", round(lr_range[2], 2), "] (expected 0-10)")
message("gov_eu_position range: [", round(eu_range[1], 2),
        ", ", round(eu_range[2], 2), "] (expected 0-10)")

if (lr_range[1] < 0 || lr_range[2] > 10) {
  warning("gov_left_right values outside expected [0, 10] range. ",
          "Check 02_process_parlgov.R seat-weighted mean computation.")
}
if (eu_range[1] < 0 || eu_range[2] > 10) {
  warning("gov_eu_position values outside expected [0, 10] range. ",
          "Check 02_process_parlgov.R seat-weighted mean computation.")
}

# =============================================================================
# Save report
# =============================================================================
readr::write_csv(coverage,
                 file.path(path_reports, "parlgov_quality_report.csv"))

message("Script 14_parlgov_quality_check complete.")
message("Report saved to: ",
        file.path(path_reports, "parlgov_quality_report.csv"))
