# =============================================================================
# 05_results_table.R
# Extract and compile regression results into long and wide tables
#
# Output files:
#   regression_tables.rds         — list of all result tables
#   regression_results_long.csv   — one row per model x term
#   regression_results_core.csv   — as above, FE dummies excluded
#   regression_results_wide.csv   — wide format (terms as rows, models as cols)
#   lr_test_sar_sem.csv           — LR test SAR vs SEM
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

baseline <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial  <- readRDS(file.path(path_data, "spatial_panel_results.rds"))
sp_tests <- readRDS(file.path(path_data, "spatial_test_results.rds"))

# --- Compile results ----------------------------------------------------------
# Country and year FE dummies are included in the long table but filtered
# out in results_core to keep the publication table manageable.
results_long <- dplyr::bind_rows(
  extract_plm(baseline$m3_fe_twoway,               "M3: Two-way FE"),
  extract_plm(baseline$m4_fe_regime,               "M4: FE + Regime"),
  extract_spatialreg(spatial$m5_sar,               "M5: SAR"),
  extract_spatial_param(spatial$m5_sar,            "M5: SAR"),
  extract_spatialreg(spatial$m6_sem,               "M6: SEM"),
  extract_spatial_param(spatial$m6_sem,            "M6: SEM"),
  extract_spatialreg(spatial$m7_sar_regime,        "M7: SAR + Regime"),
  extract_spatial_param(spatial$m7_sar_regime,     "M7: SAR + Regime"),
  extract_spatialreg(spatial$m8_sar_robust,        "M8: SAR Robust threat"),
  extract_spatial_param(spatial$m8_sar_robust,     "M8: SAR Robust threat"),
  extract_spatialreg(spatial$m10a_sar_nofi,        "M10a: SAR no Finland"),
  extract_spatial_param(spatial$m10a_sar_nofi,     "M10a: SAR no Finland"),
  extract_spatialreg(spatial$m10b_sar_post2014,    "M10b: SAR post-2014"),
  extract_spatial_param(spatial$m10b_sar_post2014, "M10b: SAR post-2014"),
  extract_spatialreg(spatial$m10c_sar_pre2014,     "M10c: SAR pre-2014"),
  extract_spatial_param(spatial$m10c_sar_pre2014,  "M10c: SAR pre-2014"),
  extract_spatialreg(spatial$m12_sar_lagged,       "M12: SAR lagged DV"),
  extract_spatial_param(spatial$m12_sar_lagged,    "M12: SAR lagged DV")
)

# results_core excludes country_f and year_f dummy rows
results_core <- results_long %>%
  dplyr::filter(
    !grepl("^country_f|^year_f", term)
  )

results_wide <- results_core %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::distinct(model, term, .keep_all = TRUE) %>%
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    coef_str = paste0(
      formatC(estimate, digits = 3, format = "f"),
      stars,
      "\n(",
      ifelse(is.na(std_error), "",
             formatC(std_error, digits = 3, format = "f")),
      ")"
    )
  ) %>%
  dplyr::select(model, term, coef_str) %>%
  tidyr::pivot_wider(names_from = model, values_from = coef_str)

lr_summary <- if (!is.null(spatial$lr_sar_sem)) {
  data.frame(
    test      = "LR test SAR vs SEM",
    ll_sar    = round(spatial$lr_sar_sem$ll_sar,  3),
    ll_sem    = round(spatial$lr_sar_sem$ll_sem,  3),
    lr_stat   = round(spatial$lr_sar_sem$lr_stat, 3),
    lr_pval   = round(spatial$lr_sar_sem$lr_pval, 4),
    preferred = spatial$lr_sar_sem$preferred
  )
} else {
  NULL
}

if (!is.null(lr_summary)) {
  message("LR test SAR vs SEM:")
  print(lr_summary)
}

safe_finite_mean <- function(x) mean(x[is.finite(x)], na.rm = TRUE)

moran_summary_queen <- if (!is.null(sp_tests$moran_by_year)) {
  sp_tests$moran_by_year %>%
    dplyr::summarise(
      weights = "queen",
      mean_i  = round(safe_finite_mean(moran_i), 4),
      pct_sig = round(100 * mean(p_value < 0.05, na.rm = TRUE), 1)
    )
} else {
  data.frame(weights = "queen", mean_i = NA_real_, pct_sig = NA_real_)
}

moran_summary_distband <- if (!is.null(sp_tests$moran_by_year_distband)) {
  sp_tests$moran_by_year_distband %>%
    dplyr::summarise(
      weights = "dist_band_1000km",
      mean_i  = round(safe_finite_mean(moran_i), 4),
      pct_sig = round(100 * mean(p_value < 0.05, na.rm = TRUE), 1)
    )
} else {
  data.frame(weights = "dist_band_1000km", mean_i = NA_real_, pct_sig = NA_real_)
}

moran_summary <- dplyr::bind_rows(moran_summary_queen, moran_summary_distband)

message("Moran's I summary:")
print(moran_summary)

fit_summary <- data.frame(
  model = c(
    "M5: SAR", "M6: SEM", "M7: SAR + Regime",
    "M8: SAR Robust threat",
    "M10a: SAR no Finland", "M10b: SAR post-2014",
    "M10c: SAR pre-2014",   "M12: SAR lagged DV"
  ),
  log_lik = c(
    tryCatch(as.numeric(logLik(spatial$m5_sar)),            error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m6_sem)),            error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m7_sar_regime)),     error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m8_sar_robust)),     error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m10a_sar_nofi)),     error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m10b_sar_post2014)), error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m10c_sar_pre2014)),  error = function(e) NA),
    tryCatch(as.numeric(logLik(spatial$m12_sar_lagged)),    error = function(e) NA)
  ),
  aic = c(
    tryCatch(AIC(spatial$m5_sar),            error = function(e) NA),
    tryCatch(AIC(spatial$m6_sem),            error = function(e) NA),
    tryCatch(AIC(spatial$m7_sar_regime),     error = function(e) NA),
    tryCatch(AIC(spatial$m8_sar_robust),     error = function(e) NA),
    tryCatch(AIC(spatial$m10a_sar_nofi),     error = function(e) NA),
    tryCatch(AIC(spatial$m10b_sar_post2014), error = function(e) NA),
    tryCatch(AIC(spatial$m10c_sar_pre2014),  error = function(e) NA),
    tryCatch(AIC(spatial$m12_sar_lagged),    error = function(e) NA)
  )
) %>%
  dplyr::mutate(
    log_lik = round(log_lik, 2),
    aic     = round(aic, 2)
  )

message("\nModel fit summary:")
print(fit_summary)

message("\nCore results table (FE dummies excluded):")
print(results_wide)

regression_tables <- list(
  results_long  = results_long,
  results_core  = results_core,
  results_wide  = results_wide,
  moran_summary = moran_summary,
  fit_summary   = fit_summary,
  lr_summary    = lr_summary
)

saveRDS(regression_tables,
        file.path(path_data, "regression_tables.rds"))

readr::write_csv(results_long,
                 file.path(path_data, "regression_results_long.csv"))
readr::write_csv(results_core,
                 file.path(path_data, "regression_results_core.csv"))
readr::write_csv(results_wide,
                 file.path(path_data, "regression_results_wide.csv"))

if (!is.null(lr_summary)) {
  readr::write_csv(lr_summary,
                   file.path(path_data, "lr_test_sar_sem.csv"))
}

message("Script 05_results_table complete.")
