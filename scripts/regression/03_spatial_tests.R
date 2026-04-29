# =============================================================================
# 03_spatial_tests.R
# Spatial autocorrelation tests and model selection
#
# Tests performed:
#   1. Moran's I on two-way FE residuals (queen W and distance-band W)
#   2. Robust Score (RS) tests for SAR vs SEM model selection
#
# Why two-way FE residuals?
#   Moran's I is computed on residuals from M3 (two-way FE), not pooled OLS.
#   Pooled OLS residuals contain unmodelled country and year effects which
#   would conflate panel heterogeneity with spatial autocorrelation. Using
#   FE residuals isolates the genuine spatial signal after removing the
#   panel structure.
#
# RS test decision rule:
#   If adjRSlag > adjRSerr (robust score test for SAR > robust score test
#   for SEM), prefer the SAR (spatial lag) model. The robust versions of
#   the RS tests are used because they remain valid when both spatial lag
#   and spatial error are present (Anselin et al. 1996).
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

# --- Load data ----------------------------------------------------------------
baseline    <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
sp_weights  <- readRDS(file.path(path_data, "spatial_weights.rds"))

reg_data <- baseline$reg_data
W_queen  <- sp_weights$W_queen

# Extract raw distance-band matrix for manual subsetting
dist_band_mat_full <- spdep::listw2mat(sp_weights$W_dist_band)
rownames(dist_band_mat_full) <- sp_weights$countries
colnames(dist_band_mat_full) <- sp_weights$countries

# --- Use two-way FE residuals for Moran's I -----------------------------------
reg_data_complete <- reg_data[complete.cases(reg_data), ]
residuals_twoway  <- residuals(baseline$m3_fe_twoway)

stopifnot(nrow(reg_data_complete) == length(residuals_twoway))

reg_data_res <- reg_data_complete %>%
  dplyr::mutate(resid = as.numeric(residuals_twoway))

# --- Helper: safely extract statistic and p-value from htest -----------------
safe_stat <- function(x) {
  if (is.null(x)) return(NA_real_)
  val <- tryCatch(as.numeric(x$statistic), error = function(e) NA_real_)
  if (length(val) == 0) return(NA_real_)
  val[[1]]
}

safe_pval <- function(x) {
  if (is.null(x)) return(NA_real_)
  val <- tryCatch(as.numeric(x$p.value), error = function(e) NA_real_)
  if (length(val) == 0) return(NA_real_)
  val[[1]]
}

# --- Moran's I by year — queen W ----------------------------------------------
moran_by_year <- reg_data_res %>%
  dplyr::group_by(year) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(df) {
    yr <- unique(df$year)

    common <- intersect(
      as.character(df$country),
      sp_weights$countries
    )

    if (length(common) < 3) {
      message("Year ", yr, ": fewer than 3 countries in queen W — skipping.")
      return(data.frame(year = yr, moran_i = NA, p_value = NA,
                        n_countries = length(common),
                        weights     = "queen"))
    }

    df_sub <- df %>%
      dplyr::filter(country %in% common) %>%
      dplyr::arrange(factor(country, levels = sp_weights$countries))

    W_sub   <- subset_listw(W_queen, sp_weights$countries, common)
    res_vec <- df_sub$resid

    mt <- tryCatch(
      spdep::moran.test(res_vec, W_sub, zero.policy = TRUE),
      error = function(e) NULL
    )

    if (is.null(mt)) {
      return(data.frame(year = yr, moran_i = NA, p_value = NA,
                        n_countries = length(common),
                        weights     = "queen"))
    }

    data.frame(
      year        = yr,
      moran_i     = round(safe_stat(mt), 4),
      p_value     = round(safe_pval(mt), 4),
      n_countries = length(common),
      weights     = "queen"
    )
  })

message("Moran's I by year (queen W, two-way FE residuals):")
print(moran_by_year)

n_sig_queen <- sum(moran_by_year$p_value < 0.05, na.rm = TRUE)
message("Years with significant spatial autocorrelation (queen W): ",
        n_sig_queen, " of ", nrow(moran_by_year))

# --- Moran's I by year — distance-band W (1000 km) ---------------------------
moran_by_year_distband <- reg_data_res %>%
  dplyr::group_by(year) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(df) {
    yr <- unique(df$year)

    common <- intersect(
      as.character(df$country),
      sp_weights$countries
    )

    if (length(common) < 3) {
      message("Year ", yr, ": fewer than 3 countries in dist-band W — skipping.")
      return(data.frame(year = yr, moran_i = NA, p_value = NA,
                        n_countries = length(common),
                        weights     = "dist_band_1000km"))
    }

    df_sub <- df %>%
      dplyr::filter(country %in% common) %>%
      dplyr::arrange(factor(country, levels = sp_weights$countries))

    W_sub <- subset_distband_listw(dist_band_mat_full, common)

    if (is.null(W_sub)) {
      return(data.frame(year = yr, moran_i = NA, p_value = NA,
                        n_countries = length(common),
                        weights     = "dist_band_1000km"))
    }

    res_vec <- df_sub$resid

    mt <- tryCatch(
      spdep::moran.test(res_vec, W_sub, zero.policy = TRUE),
      error = function(e) NULL
    )

    if (is.null(mt)) {
      return(data.frame(year = yr, moran_i = NA, p_value = NA,
                        n_countries = length(common),
                        weights     = "dist_band_1000km"))
    }

    data.frame(
      year        = yr,
      moran_i     = round(safe_stat(mt), 4),
      p_value     = round(safe_pval(mt), 4),
      n_countries = length(common),
      weights     = "dist_band_1000km"
    )
  })

message("Moran's I by year (distance-band W, 1000 km, two-way FE residuals):")
print(moran_by_year_distband)

n_sig_distband <- sum(moran_by_year_distband$p_value < 0.05, na.rm = TRUE)
message("Years with significant spatial autocorrelation (distance-band W): ",
        n_sig_distband, " of ", nrow(moran_by_year_distband))

# --- RS tests for spatial model selection -------------------------------------
# Cross-sectional OLS is fitted per year without fixed effects.
# Fixed effects are not included here because the RS tests are designed
# for cross-sectional models. The panel FE structure is accounted for
# by using FE residuals for Moran's I above.
rs_tests_by_year <- reg_data_res %>%
  dplyr::group_by(year) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(df) {
    yr <- unique(df$year)

    common <- intersect(
      as.character(df$country),
      sp_weights$countries
    )

    if (length(common) < 3) {
      return(data.frame(year = yr,
                        rs_lag  = NA, rs_lag_p  = NA,
                        rs_err  = NA, rs_err_p  = NA,
                        rrs_lag = NA, rrs_lag_p = NA,
                        rrs_err = NA, rrs_err_p = NA,
                        n_countries = length(common)))
    }

    df_sub <- df %>%
      dplyr::filter(country %in% common) %>%
      dplyr::arrange(factor(country, levels = sp_weights$countries))

    W_sub <- subset_listw(W_queen, sp_weights$countries, common)

    formula_cs <- defence_gdp ~ threat_land_log + debt_gdp +
      deficit_gdp + gdp_growth + immigration_rate +
      gov_left_right + gov_eu_position + election_year

    ols_cs <- tryCatch(
      lm(formula_cs, data = df_sub),
      error = function(e) NULL
    )

    if (is.null(ols_cs)) {
      return(data.frame(year = yr,
                        rs_lag  = NA, rs_lag_p  = NA,
                        rs_err  = NA, rs_err_p  = NA,
                        rrs_lag = NA, rrs_lag_p = NA,
                        rrs_err = NA, rrs_err_p = NA,
                        n_countries = length(common)))
    }

    rst <- tryCatch(
      spdep::lm.RStests(ols_cs, W_sub,
                         test        = c("RSlag", "RSerr",
                                         "adjRSlag", "adjRSerr"),
                         zero.policy = TRUE),
      error = function(e) NULL
    )

    if (is.null(rst)) {
      return(data.frame(year = yr,
                        rs_lag  = NA, rs_lag_p  = NA,
                        rs_err  = NA, rs_err_p  = NA,
                        rrs_lag = NA, rrs_lag_p = NA,
                        rrs_err = NA, rrs_err_p = NA,
                        n_countries = length(common)))
    }

    data.frame(
      year        = yr,
      rs_lag      = round(safe_stat(rst$RSlag),    4),
      rs_lag_p    = round(safe_pval(rst$RSlag),    4),
      rs_err      = round(safe_stat(rst$RSerr),    4),
      rs_err_p    = round(safe_pval(rst$RSerr),    4),
      rrs_lag     = round(safe_stat(rst$adjRSlag), 4),
      rrs_lag_p   = round(safe_pval(rst$adjRSlag), 4),
      rrs_err     = round(safe_stat(rst$adjRSerr), 4),
      rrs_err_p   = round(safe_pval(rst$adjRSerr), 4),
      n_countries = length(common)
    )
  })

message("RS tests by year:")
print(rs_tests_by_year)

# --- Model selection summary --------------------------------------------------
model_selection <- rs_tests_by_year %>%
  dplyr::summarise(
    pct_lag_sig  = round(100 * mean(rs_lag_p  < 0.05, na.rm = TRUE), 1),
    pct_err_sig  = round(100 * mean(rs_err_p  < 0.05, na.rm = TRUE), 1),
    pct_rlag_sig = round(100 * mean(rrs_lag_p < 0.05, na.rm = TRUE), 1),
    pct_rerr_sig = round(100 * mean(rrs_err_p < 0.05, na.rm = TRUE), 1)
  )

message("Model selection summary (% years significant):")
print(model_selection)

# Decision rule: prefer SAR if adjRSlag > adjRSerr (Anselin et al. 1996)
rlag <- model_selection$pct_rlag_sig
rerr <- model_selection$pct_rerr_sig

if (!is.na(rlag) && !is.na(rerr)) {
  if (rlag > rerr) {
    message("Recommendation: SAR (spatial lag) model — adjRSlag significant in ",
            rlag, "% of years vs adjRSerr in ", rerr, "% of years.")
  } else {
    message("Recommendation: SEM (spatial error) model — adjRSerr significant in ",
            rerr, "% of years vs adjRSlag in ", rlag, "% of years.")
  }
} else {
  message("Recommendation: insufficient data for automatic selection.")
}

# --- Moran's I comparison summary ---------------------------------------------
moran_comparison <- dplyr::bind_rows(
  moran_by_year,
  moran_by_year_distband
) %>%
  dplyr::group_by(weights) %>%
  dplyr::summarise(
    mean_i  = round(mean(moran_i[is.finite(moran_i)], na.rm = TRUE), 4),
    pct_sig = round(100 * mean(p_value < 0.05, na.rm = TRUE), 1),
    .groups = "drop"
  )

message("Moran's I comparison across weight matrices:")
print(moran_comparison)

# --- Save ---------------------------------------------------------------------
spatial_test_results <- list(
  moran_by_year          = moran_by_year,
  moran_by_year_distband = moran_by_year_distband,
  moran_comparison       = moran_comparison,
  rs_tests_by_year       = rs_tests_by_year,
  model_selection        = model_selection
)

saveRDS(spatial_test_results,
        file.path(path_data, "spatial_test_results.rds"))

message("Script 03_spatial_tests complete.")
