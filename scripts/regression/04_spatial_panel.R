# =============================================================================
# 04_spatial_panel.R
# Spatial panel models (SAR and SEM) with block-diagonal weight matrices
#
# Models estimated:
#   M5:   SAR — primary specification (queen W, all years)
#   M6:   SEM — compared to M5 via LR test; SAR preferred if LL_SAR > LL_SEM
#   M7:   SAR with regime x threat interactions
#   M8:   SAR robustness — threat_score_log instead of threat_land_log
#   M9:   SAR inverse distance W — robustness to weight matrix choice
#   M10a: SAR excluding Finland (no land border with sample countries)
#   M10b: SAR post-2014 subsample
#   M10c: SAR pre-2014 subsample
#   M12:  SAR with lagged DV — tests whether spatial lag reflects persistence
#
# All models use two-way fixed effects (country + year dummies) implemented
# by appending country_f and year_f to the formula. Factor levels are
# re-set after row removal by build_block_w to avoid empty factor levels.
#
# zero.policy = TRUE: allows observations with no neighbours (spatial lag = 0).
# This is appropriate here because isolated observations arise from the
# block-diagonal structure, not from genuine geographic isolation.
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

baseline   <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
sp_weights <- readRDS(file.path(path_data, "spatial_weights.rds"))

reg_data <- baseline$reg_data

reg_data_complete <- reg_data[complete.cases(reg_data), ] %>%
  dplyr::mutate(
    country = as.character(country),
    year    = as.integer(year),
    regime  = factor(as.character(regime), levels = c("1", "2", "3", "4"))
  )

message("Complete cases for spatial estimation: ", nrow(reg_data_complete))
message("Years covered: ", min(reg_data_complete$year),
        "-", max(reg_data_complete$year))
message("Countries in estimation sample: ",
        paste(sort(unique(reg_data_complete$country)), collapse = ", "))

# --- Formula definitions ------------------------------------------------------

# Primary specification: land-based threat measure
formula_primary <- paste(
  "defence_gdp ~ threat_land_log + debt_gdp +",
  "deficit_gdp + gdp_growth + immigration_rate +",
  "gov_left_right + gov_eu_position + election_year"
)

# Regime interactions: tests whether threat response differs across periods
formula_regime <- paste(
  "defence_gdp ~ threat_land_log * regime +",
  "debt_gdp + deficit_gdp + gdp_growth +",
  "immigration_rate + gov_left_right +",
  "gov_eu_position + election_year"
)

# Robustness: all-conflict threat measure (not land-contiguous only)
formula_robust <- paste(
  "defence_gdp ~ threat_score_log + debt_gdp +",
  "deficit_gdp + gdp_growth + immigration_rate +",
  "gov_left_right + gov_eu_position + election_year"
)

# Lagged DV: tests whether spatial lag reflects persistence rather than diffusion
formula_lagged_dv <- paste(
  "defence_gdp ~ lag_defence_gdp + threat_land_log + debt_gdp +",
  "deficit_gdp + gdp_growth + immigration_rate +",
  "gov_left_right + gov_eu_position + election_year"
)

# --- M5: SAR primary ----------------------------------------------------------
m5_sar <- run_sar_pooled(
  data         = reg_data_complete,
  formula_vars = formula_primary,
  sp_weights   = sp_weights,
  label        = "Model 5: SAR primary"
)

# --- M6: SEM primary ----------------------------------------------------------
m6_sem <- run_sem_pooled(
  data         = reg_data_complete,
  formula_vars = formula_primary,
  sp_weights   = sp_weights,
  label        = "Model 6: SEM primary"
)

# --- LR test: SAR vs SEM ------------------------------------------------------
# If LL_SAR > LL_SEM, the SAR model is preferred. The LR statistic is
# 2*(LL_SAR - LL_SEM) ~ chi-squared(1) under the null that rho = lambda.
lr_sar_sem <- tryCatch({
  if (is.null(m5_sar) || is.null(m6_sem)) stop("One or both models are NULL")

  conv_sar <- tryCatch(m5_sar$convergence, error = function(e) 0L)
  conv_sem <- tryCatch(m6_sem$convergence, error = function(e) 0L)
  if (!is.null(conv_sar) && !is.na(conv_sar) && conv_sar != 0)
    message("Warning: M5 SAR convergence flag = ", conv_sar,
            " — LR test may be unreliable")
  if (!is.null(conv_sem) && !is.na(conv_sem) && conv_sem != 0)
    message("Warning: M6 SEM convergence flag = ", conv_sem,
            " — LR test may be unreliable")

  ll_sar  <- as.numeric(logLik(m5_sar))
  ll_sem  <- as.numeric(logLik(m6_sem))
  lr_stat <- 2 * (ll_sar - ll_sem)
  lr_pval <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
  result  <- list(
    ll_sar    = ll_sar,
    ll_sem    = ll_sem,
    lr_stat   = lr_stat,
    lr_pval   = lr_pval,
    preferred = if (ll_sar > ll_sem) "SAR" else "SEM"
  )
  message("\nLR test SAR vs SEM:")
  message("  LL SAR  = ", round(ll_sar,  3))
  message("  LL SEM  = ", round(ll_sem,  3))
  message("  LR stat = ", round(lr_stat, 3))
  message("  p-value = ", round(lr_pval, 4))
  message("  Preferred: ", result$preferred)
  result
}, error = function(e) {
  message("LR test failed: ", e$message)
  NULL
})

# --- M7: SAR with regime interactions -----------------------------------------
m7_sar_regime <- run_sar_pooled(
  data         = reg_data_complete,
  formula_vars = formula_regime,
  sp_weights   = sp_weights,
  label        = "Model 7: SAR regime interactions"
)

# --- M8: SAR robustness (threat_score_log) ------------------------------------
m8_sar_robust <- run_sar_pooled(
  data         = reg_data_complete,
  formula_vars = formula_robust,
  sp_weights   = sp_weights,
  label        = "Model 8: SAR robustness threat_score_log"
)

# --- M9: SAR inverse distance W -----------------------------------------------
w_inv_obj <- build_block_w_invdist(
  data = reg_data_complete %>%
    dplyr::arrange(year, country) %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year)
    ),
  sp_weights = sp_weights
)

m9_sar_invdist <- NULL

if (!is.null(w_inv_obj)) {
  data_inv <- w_inv_obj$data_valid %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year),
      regime    = factor(as.character(regime), levels = c("1", "2", "3", "4"))
    )

  fe_formula_primary <- as.formula(
    paste(formula_primary, "+ country_f + year_f")
  )

  m9_sar_invdist <- tryCatch(
    spatialreg::lagsarlm(
      formula     = fe_formula_primary,
      data        = data_inv,
      listw       = w_inv_obj$W_listw,
      zero.policy = TRUE,
      quiet       = TRUE
    ),
    error = function(e) {
      message("Model 9: SAR inverse distance failed: ", e$message)
      NULL
    }
  )

  if (!is.null(m9_sar_invdist)) {
    message("\nModel 9: SAR inverse distance:")
    print(summary(m9_sar_invdist))
  }
}

# --- M10a: SAR excluding Finland ----------------------------------------------
# Finland has no land border with other regression sample countries.
# Its queen contiguity neighbours are assigned via distance fallback in
# 01_spatial_weights.R. This model checks sensitivity to that decision
# by excluding Finland entirely and rebuilding the weight matrix.
message("M10a: Excluding Finland (distance fallback in queen W)")
reg_data_nofi <- reg_data_complete %>%
  dplyr::filter(country != "FI")

message("Rows after removing Finland: ", nrow(reg_data_nofi))
message("Countries in M10a sample: ",
        paste(sort(unique(reg_data_nofi$country)), collapse = ", "))

countries_nofi <- sort(unique(reg_data_nofi$country))

all_ne_nofi <- rnaturalearth::ne_countries(
  scale       = "medium",
  returnclass = "sf"
) %>%
  dplyr::mutate(
    iso_a2 = dplyr::case_when(
      name == "France" ~ "FR",
      name == "Norway" ~ "NO",
      TRUE             ~ iso_a2
    )
  ) %>%
  sf::st_transform(4326) %>%
  dplyr::filter(iso_a2 %in% countries_nofi) %>%
  dplyr::select(country = iso_a2, geometry) %>%
  dplyr::arrange(country)

nb_nofi      <- spdep::poly2nb(all_ne_nofi, queen = TRUE)
W_queen_nofi <- spdep::nb2listw(nb_nofi,
                                 style       = "W",
                                 zero.policy = TRUE)

sp_weights_nofi <- list(
  countries          = all_ne_nofi$country,
  W_queen            = W_queen_nofi,
  W_inv_dist         = sp_weights$W_inv_dist,
  isolated_countries = character(0)
)

m10a_sar_nofi <- run_sar_pooled(
  data         = reg_data_nofi,
  formula_vars = formula_primary,
  sp_weights   = sp_weights_nofi,
  label        = "Model 10a: SAR no Finland"
)

# --- M10b: SAR post-2014 subsample --------------------------------------------
# Tests whether the spatial lag and threat coefficients differ in the
# post-Crimea period (2014 onwards).
reg_data_post2014 <- reg_data_complete %>%
  dplyr::filter(year >= 2014)

message("Rows in post-2014 subsample: ", nrow(reg_data_post2014))
message("Years covered: ", min(reg_data_post2014$year),
        "-", max(reg_data_post2014$year))

m10b_sar_post2014 <- run_sar_pooled(
  data         = reg_data_post2014,
  formula_vars = formula_primary,
  sp_weights   = sp_weights,
  label        = "Model 10b: SAR post-2014"
)

# --- M10c: SAR pre-2014 subsample ---------------------------------------------
reg_data_pre2014 <- reg_data_complete %>%
  dplyr::filter(year < 2014)

message("Rows in pre-2014 subsample: ", nrow(reg_data_pre2014))
message("Years covered: ", min(reg_data_pre2014$year),
        "-", max(reg_data_pre2014$year))

m10c_sar_pre2014 <- run_sar_pooled(
  data         = reg_data_pre2014,
  formula_vars = formula_primary,
  sp_weights   = sp_weights,
  label        = "Model 10c: SAR pre-2014"
)

# --- M12: SAR with lagged DV --------------------------------------------------
# If the spatial lag (rho) becomes insignificant after controlling for the
# lagged DV, this suggests the spatial pattern reflects persistence in
# defence spending rather than genuine cross-country diffusion.
reg_data_lagged <- reg_data_complete %>%
  dplyr::arrange(country, year) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(lag_defence_gdp = dplyr::lag(defence_gdp, 1)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(lag_defence_gdp))

message("Rows in lagged DV sample: ", nrow(reg_data_lagged))
message("Years covered: ", min(reg_data_lagged$year),
        "-", max(reg_data_lagged$year))

m12_sar_lagged <- run_sar_pooled(
  data         = reg_data_lagged,
  formula_vars = formula_lagged_dv,
  sp_weights   = sp_weights,
  label        = "Model 12: SAR lagged DV"
)

# --- Save ---------------------------------------------------------------------
spatial_panel_results <- list(
  m5_sar            = m5_sar,
  m6_sem            = m6_sem,
  m7_sar_regime     = m7_sar_regime,
  m8_sar_robust     = m8_sar_robust,
  m9_sar_invdist    = m9_sar_invdist,
  m10a_sar_nofi     = m10a_sar_nofi,
  m10b_sar_post2014 = m10b_sar_post2014,
  m10c_sar_pre2014  = m10c_sar_pre2014,
  m12_sar_lagged    = m12_sar_lagged,
  lr_sar_sem        = lr_sar_sem
)

saveRDS(spatial_panel_results,
        file.path(path_data, "spatial_panel_results.rds"))

message("Script 04_spatial_panel complete.")
