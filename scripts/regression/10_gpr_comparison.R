# =============================================================================
# 10_gpr_comparison.R
# Compare the UCDP-based threat measure (threat_land_log) with the
# Caldara-Iacoviello Geopolitical Risk (GPR) index and estimate M13.
#
# Purpose:
#   The UCDP threat measure (threat_land_log) captures actual conflict
#   fatalities near each country's border. GPR (Caldara & Iacoviello 2022)
#   captures media salience of geopolitical risk. The two are conceptually
#   complementary: high correlation validates our UCDP measure against a
#   well-established benchmark; divergence periods reveal cases where media
#   perception exceeded or lagged actual violence.
#
# Blocks:
#   1. Download and parse GPR data (country-level GPRC_ columns)
#   2. Annualise and merge GPR with panel (14 available countries)
#   3. Correlation analysis at three levels:
#        a. Country-level Pearson correlations
#        b. Pooled (all country-years) correlation
#        c. Year-level divergence series
#   4. M13: SAR using GPR as alternative threat proxy
#      Compare AIC(M13) vs AIC(M5) — lower M5 AIC confirms threat_land_log
#
# GPR country coverage note:
#   GPR provides country-level indices (GPRC_XXX) for 14 of our 24 panel
#   countries as of dataset version GPR 2024. The 14 covered countries are
#   used for the correlation analysis; M13 is estimated on this subsample.
#   Countries not covered by GPR are simply excluded from M13 (not imputed).
#
# Outputs:
#   gpr_comparison_results.rds         — full results list
#   quality_reports/gpr_correlation_summary.csv
#   quality_reports/gpr_divergence_by_country.csv
#   quality_reports/gpr_aic_comparison.csv
#
# Citation:
#   Caldara, D. & Iacoviello, M. (2022). Measuring Geopolitical Risk.
#   American Economic Review 112(4): 1194-1225.
#   Data: https://www.matteoiacoviello.com/gpr.htm
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

panel   <- readRDS(file.path(path_data, "panel_full.rds"))
spatial <- readRDS(file.path(path_data, "spatial_panel_results.rds"))

# =============================================================================
# BLOCK 1: Download and parse GPR data
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 1: Download Caldara-Iacoviello GPR data")
message(strrep("=", 60))

gpr_raw <- tryCatch(
  haven::read_dta(
    "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.dta"
  ),
  error = function(e) {
    message("ERROR downloading GPR: ", e$message)
    NULL
  }
)

if (is.null(gpr_raw)) {
  stop(
    "GPR download failed. Check internet connection and that ",
    "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.dta ",
    "is accessible."
  )
}

message("GPR raw: ", nrow(gpr_raw), " rows, ", ncol(gpr_raw), " columns.")

# --- Parse date column --------------------------------------------------------
# The `month` column may arrive as (a) Date, (b) Stata monthly numeric
# (months since Jan 1960, values < 10000), or (c) Stata daily numeric
# (days since 1960-01-01, values > 10000). Detect and convert accordingly.

gpr_raw <- tibble::as_tibble(gpr_raw)

if (inherits(gpr_raw$month, "Date")) {
  gpr_raw <- gpr_raw %>% dplyr::mutate(month_parsed = month)
} else if (is.numeric(gpr_raw$month) &&
           max(gpr_raw$month, na.rm = TRUE) < 10000) {
  gpr_raw <- gpr_raw %>%
    dplyr::mutate(
      month_parsed = as.Date(
        paste(
          1960 + floor(as.numeric(month) / 12),
          (as.numeric(month) %% 12) + 1,
          "01", sep = "-"
        )
      )
    )
} else {
  gpr_raw <- gpr_raw %>%
    dplyr::mutate(
      month_parsed = as.Date(as.numeric(month), origin = "1960-01-01")
    )
}

message("GPR date range: ",
        format(min(gpr_raw$month_parsed, na.rm = TRUE), "%Y-%m"),
        " to ",
        format(max(gpr_raw$month_parsed, na.rm = TRUE), "%Y-%m"))

# =============================================================================
# BLOCK 2: Map GPR country columns to panel ISO2 codes and annualise
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 2: Map GPR country columns and annualise")
message(strrep("=", 60))

# Mapping from ISO2 (panel) to GPR column suffix (GPRC_XXX).
# Only countries for which a country-level GPR column exists.
# Source: Caldara & Iacoviello (2022) replication data documentation.
gpr_col_map <- c(
  "BE" = "GPRC_BEL",
  "BG" = "GPRC_BGR",
  "HR" = "GPRC_HRV",
  "CZ" = "GPRC_CZE",
  "DK" = "GPRC_DNK",
  "FI" = "GPRC_FIN",
  "FR" = "GPRC_FRA",
  "DE" = "GPRC_DEU",
  "GR" = "GPRC_GRC",
  "HU" = "GPRC_HUN",
  "IT" = "GPRC_ITA",
  "NL" = "GPRC_NLD",
  "PL" = "GPRC_POL",
  "PT" = "GPRC_PRT",
  "RO" = "GPRC_ROU",
  "ES" = "GPRC_ESP",
  "GB" = "GPRC_GBR",
  "NO" = "GPRC_NOR"
)

# Keep only columns that actually exist in this version of the dataset
gpr_col_map <- gpr_col_map[gpr_col_map %in% names(gpr_raw)]

n_covered <- length(gpr_col_map)
message("GPR country columns found: ", n_covered, " / ", length(gpr_col_map) + 0)
message("Covered: ", paste(names(gpr_col_map), collapse = ", "))
message("Not covered: ",
        paste(setdiff(nato_eu_core, names(gpr_col_map)), collapse = ", "))

# Reshape to long, extract year, annualise
gpr_long <- gpr_raw %>%
  dplyr::select(month = month_parsed, dplyr::any_of(unname(gpr_col_map))) %>%
  dplyr::filter(!is.na(month),
                lubridate::year(month) >= year_start,
                lubridate::year(month) <= year_end) %>%
  tidyr::pivot_longer(
    -month,
    names_to  = "gpr_col",
    values_to = "gpr_monthly"
  ) %>%
  dplyr::mutate(
    country = names(gpr_col_map)[match(gpr_col, gpr_col_map)],
    year    = lubridate::year(month)
  ) %>%
  dplyr::filter(!is.na(country), !is.na(gpr_monthly))

# Annual mean GPR per country-year
gpr_annual <- gpr_long %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(
    gpr_mean   = mean(gpr_monthly, na.rm = TRUE),
    gpr_max    = max(gpr_monthly,  na.rm = TRUE),
    gpr_n_months = sum(!is.na(gpr_monthly)),
    .groups    = "drop"
  )

message("Annual GPR rows: ", nrow(gpr_annual), " (", n_covered,
        " countries × up to ", year_end - year_start + 1, " years)")

# Merge with panel
panel_gpr <- panel %>%
  dplyr::inner_join(gpr_annual, by = c("country", "year")) %>%
  dplyr::filter(!is.na(threat_land_log), !is.na(gpr_mean))

gpr_countries <- sort(unique(panel_gpr$country))
message("Countries in merged GPR panel: ", paste(gpr_countries, collapse = ", "))
message("Observations in merged GPR panel: ", nrow(panel_gpr))

# =============================================================================
# BLOCK 3: Correlation analysis
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 3: Correlation analysis")
message(strrep("=", 60))

# --- 3a. Country-level Pearson correlations -----------------------------------
gpr_correlation_by_country <- purrr::map_dfr(gpr_countries, function(iso) {
  df <- panel_gpr %>%
    dplyr::filter(country == iso,
                  !is.na(threat_land_log),
                  !is.na(gpr_mean))

  if (nrow(df) < 5) {
    return(data.frame(
      country = iso, n = nrow(df),
      pearson_r = NA_real_, p_value = NA_real_,
      spearman_r = NA_real_, interpretation = "Insufficient data"
    ))
  }

  pt  <- tryCatch(cor.test(df$threat_land_log, df$gpr_mean,
                            method = "pearson"),  error = function(e) NULL)
  spt <- tryCatch(cor.test(df$threat_land_log, df$gpr_mean,
                            method = "spearman", exact = FALSE),
                  error = function(e) NULL)

  data.frame(
    country    = iso,
    n          = nrow(df),
    pearson_r  = round(if (!is.null(pt))  pt$estimate  else NA, 3),
    p_value    = round(if (!is.null(pt))  pt$p.value   else NA, 4),
    spearman_r = round(if (!is.null(spt)) spt$estimate else NA, 3),
    interpretation = dplyr::case_when(
      is.null(pt) | is.null(spt) ~ "Test failed",
      abs(pt$estimate) >= 0.7   ~ "Strong",
      abs(pt$estimate) >= 0.5   ~ "Moderate",
      abs(pt$estimate) >= 0.3   ~ "Weak",
      TRUE                      ~ "Negligible"
    )
  )
})

message("Country-level correlations (threat_land_log vs GPR):")
print(gpr_correlation_by_country)

# --- 3b. Pooled correlation ---------------------------------------------------
pooled_cor <- tryCatch(
  cor.test(panel_gpr$threat_land_log, panel_gpr$gpr_mean, method = "pearson"),
  error = function(e) NULL
)
pooled_r <- if (!is.null(pooled_cor)) round(pooled_cor$estimate, 3) else NA
pooled_p <- if (!is.null(pooled_cor)) round(pooled_cor$p.value,  4) else NA

message("Pooled Pearson correlation (threat_land_log vs GPR): r = ", pooled_r,
        ", p = ", pooled_p)

# --- 3c. Year-level divergence series ----------------------------------------
# Divergence = |threat_land_log_z - gpr_z|, averaged across countries per year.
# Highlights years where actual violence and media perception diverge most.

panel_gpr_scaled <- panel_gpr %>%
  dplyr::mutate(
    threat_z = as.numeric(scale(threat_land_log)),
    gpr_z    = as.numeric(scale(gpr_mean))
  )

gpr_divergence_by_year <- panel_gpr_scaled %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean_divergence  = round(mean(abs(threat_z - gpr_z), na.rm = TRUE), 4),
    max_divergence   = round(max(abs(threat_z - gpr_z),  na.rm = TRUE), 4),
    n_countries      = sum(!is.na(threat_z) & !is.na(gpr_z)),
    .groups          = "drop"
  ) %>%
  dplyr::arrange(year)

message("Years with highest UCDP–GPR divergence (top 5):")
print(dplyr::slice_max(gpr_divergence_by_year, mean_divergence, n = 5))

# Per-country divergence summary
gpr_divergence_by_country <- panel_gpr_scaled %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    n_years          = dplyr::n(),
    mean_divergence  = round(mean(abs(threat_z - gpr_z), na.rm = TRUE), 4),
    max_divergence   = round(max(abs(threat_z - gpr_z),  na.rm = TRUE), 4),
    mean_threat_z    = round(mean(threat_z, na.rm = TRUE), 4),
    mean_gpr_z       = round(mean(gpr_z,    na.rm = TRUE), 4),
    .groups          = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_divergence))

# =============================================================================
# BLOCK 4: M13 — SAR with GPR as alternative threat proxy
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 4: M13 — SAR with GPR as alternative threat proxy")
message(strrep("=", 60))

# M13 formula mirrors M5 (SAR primary) but replaces threat_land_log with
# gpr_mean (log-scaled). Estimated on the GPR subsample (countries with
# GPR coverage only). AIC comparison vs M5 confirms whether land-contiguous
# UCDP threat outperforms the perception-based GPR measure.

panel_m13 <- panel_gpr %>%
  dplyr::mutate(
    country   = as.character(country),
    year      = as.integer(year),
    gpr_log   = log(gpr_mean + 1),
    country_f = as.factor(country),
    year_f    = as.factor(year)
  ) %>%
  dplyr::filter(complete.cases(
    defence_gdp, gpr_log,
    debt_gdp, deficit_gdp, gdp_growth, immigration_rate,
    gov_left_right, gov_eu_position, election_year
  ))

formula_m13 <- paste(
  "defence_gdp ~ gpr_log +",
  "debt_gdp + deficit_gdp + gdp_growth +",
  "immigration_rate + gov_left_right +",
  "gov_eu_position + election_year"
)

m13_sar <- tryCatch(
  run_sar_pooled(
    data         = panel_m13,
    formula_vars = formula_m13,
    sp_weights   = sp_weights,
    label        = "M13: SAR with GPR"
  ),
  error = function(e) {
    message("M13 SAR failed: ", e$message)
    NULL
  }
)

# Re-estimate M5 on the same GPR subsample for a fair AIC comparison
formula_m5_sub <- paste(
  "defence_gdp ~ threat_land_log +",
  "debt_gdp + deficit_gdp + gdp_growth +",
  "immigration_rate + gov_left_right +",
  "gov_eu_position + election_year"
)

m5_gpr_subsample <- tryCatch(
  run_sar_pooled(
    data         = panel_m13 %>%
      dplyr::filter(!is.na(threat_land_log)),
    formula_vars = formula_m5_sub,
    sp_weights   = sp_weights,
    label        = "M5 (GPR subsample)"
  ),
  error = function(e) {
    message("M5 GPR-subsample SAR failed: ", e$message)
    NULL
  }
)

# AIC comparison
aic_m13       <- tryCatch(AIC(m13_sar),         error = function(e) NA_real_)
aic_m5_sub    <- tryCatch(AIC(m5_gpr_subsample), error = function(e) NA_real_)
aic_m5_full   <- tryCatch(AIC(spatial$m5_sar),   error = function(e) NA_real_)

gpr_aic_comparison <- data.frame(
  model         = c("M5 (full sample)", "M5 (GPR subsample)", "M13: GPR proxy"),
  threat_var    = c("threat_land_log", "threat_land_log", "gpr_log"),
  sample        = c("All panel countries", "GPR-covered countries only",
                    "GPR-covered countries only"),
  aic           = round(c(aic_m5_full, aic_m5_sub, aic_m13), 3),
  preferred     = c(
    "",
    ifelse(!is.na(aic_m5_sub) & !is.na(aic_m13) & aic_m5_sub <= aic_m13,
           "YES", ""),
    ifelse(!is.na(aic_m5_sub) & !is.na(aic_m13) & aic_m13 < aic_m5_sub,
           "YES", "")
  ),
  interpretation = c(
    "Primary model (full 23-country sample)",
    "UCDP measure on GPR-covered subsample",
    "GPR perception measure (Caldara-Iacoviello 2022)"
  )
)

message("M13 AIC comparison:")
print(gpr_aic_comparison)

if (!is.null(m13_sar)) {
  message("M13 SAR summary:")
  print(summary(m13_sar))
}

# =============================================================================
# Save outputs
# =============================================================================
gpr_comparison_results <- list(
  gpr_countries            = gpr_countries,
  n_gpr_countries          = n_covered,
  gpr_annual               = gpr_annual,
  panel_gpr                = panel_gpr,
  gpr_correlation_by_country = gpr_correlation_by_country,
  pooled_r                 = pooled_r,
  pooled_p                 = pooled_p,
  gpr_divergence_by_year   = gpr_divergence_by_year,
  gpr_divergence_by_country = gpr_divergence_by_country,
  m13_sar                  = m13_sar,
  m5_gpr_subsample         = m5_gpr_subsample,
  gpr_aic_comparison       = gpr_aic_comparison
)

saveRDS(gpr_comparison_results,
        file.path(path_data, "gpr_comparison_results.rds"))

readr::write_csv(gpr_correlation_by_country,
                 file.path(path_reports, "gpr_correlation_summary.csv"))

readr::write_csv(gpr_divergence_by_country,
                 file.path(path_reports, "gpr_divergence_by_country.csv"))

readr::write_csv(gpr_aic_comparison,
                 file.path(path_reports, "gpr_aic_comparison.csv"))

readr::write_csv(gpr_divergence_by_year,
                 file.path(path_reports, "gpr_divergence_by_year.csv"))

message("\nScript 10_gpr_comparison complete.")
message("  Pooled correlation (threat_land_log vs GPR): r = ", pooled_r)
message("  Interpretation: ",
        dplyr::case_when(
          is.na(pooled_r)        ~ "Could not compute",
          abs(pooled_r) >= 0.7  ~ "Strong — UCDP and GPR track each other well",
          abs(pooled_r) >= 0.5  ~ "Moderate — complementary measures",
          abs(pooled_r) >= 0.3  ~ "Weak — conceptually distinct signals",
          TRUE                   ~ "Negligible — divergent measures"
        )
)
message("  M13 AIC: ", round(aic_m13, 2),
        " | M5 (GPR subsample) AIC: ", round(aic_m5_sub, 2))
