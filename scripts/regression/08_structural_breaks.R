# =============================================================================
# 08_structural_breaks.R
# Structural break analysis and regime validation
#
# Blocks:
#   1. Map BIC-optimal breakpoint to calendar date
#      CAUTION: strucchange is applied to a stacked panel, not a true time
#      series. The panel is sorted by year then country, so observation 97
#      does not correspond to a single point in time. Results from Block 1
#      are supplementary only. Blocks 2 and 3 (Chow tests, AIC/BIC) are
#      the primary evidence for break year selection.
#   2. Formal Bai-Perron test (supF, BIC comparison)
#   3. Compare regime specifications (AIC/BIC/LR)
#   4. SAR with data-driven regime break
#   5. Pre/post-2014 spatial lag asymmetry test
#   6. gov_eu_position pre-2014 finding validation
#   7. Persistence vs diffusion decomposition (first-difference SAR)
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

baseline   <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial    <- readRDS(file.path(path_data, "spatial_panel_results.rds"))
sp_weights <- readRDS(file.path(path_data, "spatial_weights.rds"))

reg_data <- baseline$reg_data

reg_data_lm <- reg_data %>%
  dplyr::mutate(
    country_f = as.factor(country),
    year_f    = as.factor(year)
  )

path_tables <- file.path(path_root, "scripts", "output", "tables")
dir.create(path_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(path_reports, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Block 1: Map observation 97 to exact date
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 1: Map BIC-optimal breakpoint to calendar date")
message("CAUTION: strucchange is applied to a stacked panel sorted by year")
message("then country. Observation 97 does not correspond to a single point")
message("in time. This block is supplementary — see Blocks 2 and 3 for")
message("primary evidence on break year selection.")
message(strrep("=", 60))

reg_data_ts <- reg_data_lm %>%
  dplyr::arrange(year, country) %>%
  dplyr::filter(complete.cases(.))

n_complete <- nrow(reg_data_ts)
message("Complete observations in sorted panel: ", n_complete)

row_97 <- reg_data_ts[97, c("country", "year")]
message("Observation 97 corresponds to: country = ", row_97$country,
        ", year = ", row_97$year)

sc_formula <- defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
  gdp_growth + immigration_rate + gov_left_right +
  gov_eu_position + election_year

bp_obj <- tryCatch(
  strucchange::breakpoints(sc_formula, data = reg_data_ts, h = 0.15),
  error = function(e) { message("breakpoints failed: ", e$message); NULL }
)

bp_dates <- NULL
bp_ci    <- NULL

if (!is.null(bp_obj)) {
  bp_dates <- tryCatch(
    strucchange::breakdates(bp_obj),
    error = function(e) NULL
  )
  bp_ci <- tryCatch(
    strucchange::breakdates(bp_obj, breaks = 1),
    error = function(e) NULL
  )

  message("Breakdates (fractional):")
  print(bp_dates)

  if (!is.null(bp_dates)) {
    frac_break  <- as.numeric(bp_dates)[1]
    approx_row  <- round(frac_break * n_complete)
    approx_year <- reg_data_ts$year[approx_row]
    message("Approximate calendar year of break: ", approx_year)
  }

  bp_row_m1 <- bp_obj$breakpoints[1]
  if (!is.na(bp_row_m1)) {
    exact_break <- reg_data_ts[bp_row_m1, c("country", "year")]
    message("Exact break observation: country = ", exact_break$country,
            ", year = ", exact_break$year)
  }
}

# =============================================================================
# Block 2: Formal Bai-Perron test
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 2: Formal Bai-Perron structural break tests")
message(strrep("=", 60))

fstats_obj <- tryCatch(
  strucchange::Fstats(sc_formula, data = reg_data_ts, from = 0.15),
  error = function(e) { message("Fstats failed: ", e$message); NULL }
)

supf_test <- NULL
if (!is.null(fstats_obj)) {
  supf_test <- tryCatch(
    strucchange::sctest(fstats_obj, type = "supF"),
    error = function(e) NULL
  )
  if (!is.null(supf_test)) {
    message("supF test (any structural break):")
    message("  Statistic = ", round(supf_test$statistic, 4),
            ", p-value = ", round(supf_test$p.value, 4))
  }

  png(file.path(path_reports, "fstats_plot.png"), width = 900, height = 500)
  plot(fstats_obj, main = "F-Statistics for Structural Break Detection")
  dev.off()
  message("F-statistics plot saved.")
}

bp_comparison <- tryCatch({
  bp_full <- strucchange::breakpoints(sc_formula, data = reg_data_ts,
                                       h = 0.15, breaks = 5)
  bp_sum  <- summary(bp_full)
  rss_bic <- data.frame(
    n_breaks = 0:5,
    rss      = bp_sum$RSS["RSS", ],
    bic      = bp_sum$RSS["BIC", ]
  )
  message("RSS and BIC by number of breaks:")
  print(rss_bic)
  rss_bic
}, error = function(e) {
  message("Break comparison failed: ", e$message)
  NULL
})

# Chow tests at candidate break years using shared chow_test() helper
chow_results <- dplyr::bind_rows(
  chow_test(sc_formula, reg_data_ts, 2003),
  chow_test(sc_formula, reg_data_ts, 2014),
  chow_test(sc_formula, reg_data_ts, 2022)
)

message("Chow tests at candidate break years:")
print(chow_results)

readr::write_csv(chow_results, file.path(path_data, "bai_perron_tests.csv"))

# =============================================================================
# Block 3: Compare regime specifications formally
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 3: Compare regime specifications (AIC/BIC/LR)")
message(strrep("=", 60))

# auto_break_year is derived from strucchange but used only as a secondary
# check. The primary evidence for break year selection is the Chow tests
# and AIC/BIC comparison in this block.
auto_break_year <- tryCatch({
  if (!is.null(bp_obj) && !is.na(bp_obj$breakpoints[1])) {
    candidate <- reg_data_ts$year[bp_obj$breakpoints[1]]
    if (candidate < 1998 || candidate > 2020) {
      warning("strucchange detected break year ", candidate,
              " is outside theoretically sensible range [1998, 2020]. ",
              "Falling back to 2003. Use Chow tests and AIC/BIC as primary evidence.")
      2003L
    } else {
      candidate
    }
  } else {
    warning("strucchange breakpoints() returned no break — falling back to 2003. ",
            "Use Chow tests and AIC/BIC as primary evidence.")
    2003L
  }
}, error = function(e) {
  warning("auto_break_year detection failed — falling back to 2003.")
  2003L
})

message("Auto-detected break year (supplementary): ", auto_break_year)

reg_data_regimes <- reg_data_ts %>%
  dplyr::mutate(
    regime_auto = as.factor(ifelse(year < auto_break_year, 1L, 2L)),
    regime_2014 = as.factor(dplyr::case_when(
      year < 2014 ~ 1L,
      year < 2022 ~ 2L,
      TRUE        ~ 3L
    )),
    regime_4    = as.factor(dplyr::case_when(
      year <= 2004 ~ 1L,
      year <= 2013 ~ 2L,
      year <= 2021 ~ 3L,
      TRUE         ~ 4L
    ))
  )

lm_A <- lm(sc_formula, data = reg_data_regimes)

lm_B <- lm(
  defence_gdp ~ threat_land_log * regime_auto + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data = reg_data_regimes
)

lm_C <- lm(
  defence_gdp ~ threat_land_log * regime_2014 + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data = reg_data_regimes
)

lm_D <- lm(
  defence_gdp ~ threat_land_log * regime_4 + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data = reg_data_regimes
)

regime_comparison <- data.frame(
  model       = c("A: No regime", "B: Auto break", "C: 2014/2022", "D: Four-regime"),
  n_params    = c(length(coef(lm_A)), length(coef(lm_B)),
                  length(coef(lm_C)), length(coef(lm_D))),
  log_lik     = round(c(as.numeric(logLik(lm_A)), as.numeric(logLik(lm_B)),
                         as.numeric(logLik(lm_C)), as.numeric(logLik(lm_D))), 3),
  aic         = round(c(AIC(lm_A), AIC(lm_B), AIC(lm_C), AIC(lm_D)), 3),
  bic         = round(c(BIC(lm_A), BIC(lm_B), BIC(lm_C), BIC(lm_D)), 3)
)

message("Regime specification comparison:")
print(regime_comparison)

readr::write_csv(regime_comparison,
                 file.path(path_data, "regime_comparison_aic.csv"))

lr_tests <- purrr::map_dfr(
  list(
    list(null = lm_A, alt = lm_B, label = "A vs B"),
    list(null = lm_A, alt = lm_C, label = "A vs C"),
    list(null = lm_A, alt = lm_D, label = "A vs D"),
    list(null = lm_B, alt = lm_D, label = "B vs D")
  ),
  function(x) {
    tryCatch({
      ll_null <- as.numeric(logLik(x$null))
      ll_alt  <- as.numeric(logLik(x$alt))
      df_diff <- length(coef(x$alt)) - length(coef(x$null))
      lr_stat <- 2 * (ll_alt - ll_null)
      p_val   <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
      data.frame(
        comparison = x$label,
        lr_stat    = round(lr_stat, 3),
        df         = df_diff,
        p_value    = round(p_val, 4),
        preferred  = ifelse(p_val < 0.05, "Alternative", "Null")
      )
    }, error = function(e) {
      data.frame(comparison = x$label, lr_stat = NA, df = NA,
                 p_value = NA, preferred = "Failed")
    })
  }
)

message("LR tests between regime specifications:")
print(lr_tests)

# =============================================================================
# Block 4: SAR with data-driven regime
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 4: SAR with data-driven regime (break at ", auto_break_year, ")")
message("Note: auto_break_year is supplementary. Primary break evidence is")
message("from Chow tests and AIC/BIC in Block 3.")
message(strrep("=", 60))

reg_data_auto <- reg_data_ts %>%
  dplyr::mutate(
    country     = as.character(country),
    year        = as.integer(year),
    regime_auto = as.factor(ifelse(year < auto_break_year, 1L, 2L))
  )

formula_auto_regime <- paste(
  "defence_gdp ~ threat_land_log * regime_auto +",
  "debt_gdp + deficit_gdp + gdp_growth +",
  "immigration_rate + gov_left_right +",
  "gov_eu_position + election_year"
)

m_sar_auto <- run_sar_pooled(
  data         = reg_data_auto,
  formula_vars = formula_auto_regime,
  sp_weights   = sp_weights,
  label        = paste0("SAR auto-regime (break at ", auto_break_year, ")")
)

if (!is.null(m_sar_auto)) {
  message("AIC (auto-regime SAR): ", round(AIC(m_sar_auto), 2))
  message("AIC (four-regime SAR, M7): ",
          round(AIC(spatial$m7_sar_regime), 2))
}

# =============================================================================
# Block 5: Pre/post-2014 spatial lag asymmetry test
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 5: Pre/post-2014 spatial lag asymmetry test")
message(strrep("=", 60))

rho_pre  <- tryCatch(as.numeric(spatial$m10c_sar_pre2014$rho),  error = function(e) NA)
rho_post <- tryCatch(as.numeric(spatial$m10b_sar_post2014$rho), error = function(e) NA)

if (length(rho_pre)  == 0) rho_pre  <- NA
if (length(rho_post) == 0) rho_post <- NA

se_pre  <- tryCatch(as.numeric(summary(spatial$m10c_sar_pre2014)$rho.se),
                    error = function(e) NA)
se_post <- tryCatch(as.numeric(summary(spatial$m10b_sar_post2014)$rho.se),
                    error = function(e) NA)

if (length(se_pre)  == 0) se_pre  <- NA
if (length(se_post) == 0) se_post <- NA

z_diff <- tryCatch(
  (rho_pre - rho_post) / sqrt(se_pre^2 + se_post^2),
  error = function(e) NA
)

p_diff <- tryCatch(
  2 * pnorm(abs(z_diff), lower.tail = FALSE),
  error = function(e) NA
)

spatial_asymmetry <- data.frame(
  period = c("Pre-2014 (M10c)", "Post-2014 (M10b)"),
  rho    = round(c(rho_pre, rho_post), 4),
  se     = round(c(se_pre,  se_post),  4),
  ci_lo  = round(c(rho_pre  - 1.96 * se_pre,
                    rho_post - 1.96 * se_post), 4),
  ci_hi  = round(c(rho_pre  + 1.96 * se_pre,
                    rho_post + 1.96 * se_post), 4)
)

asymmetry_test <- data.frame(
  test       = "z-test: rho_pre vs rho_post",
  rho_pre    = round(rho_pre,  4),
  rho_post   = round(rho_post, 4),
  difference = round(rho_pre - rho_post, 4),
  z_stat     = round(z_diff, 4),
  p_value    = round(p_diff, 4),
  conclusion = ifelse(!is.na(p_diff) & p_diff < 0.05,
                      "Significant asymmetry",
                      "No significant asymmetry")
)

message("Spatial lag by period:")
print(spatial_asymmetry)
message("Asymmetry test:")
print(asymmetry_test)

readr::write_csv(asymmetry_test,
                 file.path(path_data, "spatial_lag_asymmetry_test.csv"))

# =============================================================================
# Block 6: gov_eu_position pre-2014 finding validation
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 6: gov_eu_position pre-2014 finding validation")
message(strrep("=", 60))

coef_eu_pre  <- tryCatch({
  s  <- summary(spatial$m10c_sar_pre2014)
  cm <- s$Coef
  cm["gov_eu_position", ]
}, error = function(e) NULL)

coef_eu_full <- tryCatch({
  s  <- summary(spatial$m5_sar)
  cm <- s$Coef
  cm["gov_eu_position", ]
}, error = function(e) NULL)

if (!is.null(coef_eu_pre) && !is.null(coef_eu_full)) {
  message("gov_eu_position — full sample (M5):")
  print(coef_eu_full)
  message("gov_eu_position — pre-2014 (M10c):")
  print(coef_eu_pre)
}

lm_eu_pre  <- lm(sc_formula, data = reg_data_ts %>% dplyr::filter(year < 2014))
lm_eu_post <- lm(sc_formula, data = reg_data_ts %>% dplyr::filter(year >= 2014))

eu_pre_coef  <- coef(lm_eu_pre)["gov_eu_position"]
eu_post_coef <- coef(lm_eu_post)["gov_eu_position"]
eu_pre_se    <- sqrt(diag(vcov(lm_eu_pre)))["gov_eu_position"]
eu_post_se   <- sqrt(diag(vcov(lm_eu_post)))["gov_eu_position"]

z_eu <- (eu_pre_coef - eu_post_coef) / sqrt(eu_pre_se^2 + eu_post_se^2)
p_eu <- 2 * pnorm(abs(z_eu), lower.tail = FALSE)

eu_position_test <- data.frame(
  variable      = "gov_eu_position",
  coef_pre2014  = round(eu_pre_coef,  4),
  se_pre2014    = round(eu_pre_se,    4),
  coef_post2014 = round(eu_post_coef, 4),
  se_post2014   = round(eu_post_se,   4),
  z_stat        = round(z_eu, 4),
  p_value       = round(p_eu, 4),
  conclusion    = ifelse(p_eu < 0.05,
                         "Significant difference across periods",
                         "No significant difference")
)

message("gov_eu_position subperiod test:")
print(eu_position_test)

readr::write_csv(eu_position_test,
                 file.path(path_data, "gov_eu_position_subperiod.csv"))

# =============================================================================
# Block 7: Persistence vs diffusion decomposition
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 7: Persistence vs diffusion decomposition")
message(strrep("=", 60))

reg_data_lagged <- reg_data_ts %>%
  dplyr::arrange(country, year) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(lag_defence_gdp = dplyr::lag(defence_gdp, 1)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(lag_defence_gdp)) %>%
  dplyr::mutate(
    country_f = as.factor(country),
    year_f    = as.factor(year)
  )

lm_resid_y <- lm(
  defence_gdp ~ lag_defence_gdp + country_f + year_f,
  data = reg_data_lagged
)

resid_rows   <- as.integer(rownames(model.frame(lm_resid_y)))
resid_vals   <- residuals(lm_resid_y)
lag_vals     <- reg_data_lagged$lag_defence_gdp[resid_rows]

partial_corr <- cor(resid_vals, lag_vals, use = "complete.obs")

message("Partial correlation between defence_gdp residuals and lag_defence_gdp: ",
        round(partial_corr, 4))

reg_data_fd <- reg_data_ts %>%
  dplyr::arrange(country, year) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    dplyr::across(
      c(defence_gdp, threat_land_log, debt_gdp, deficit_gdp,
        gdp_growth, immigration_rate, gov_left_right,
        gov_eu_position, election_year),
      ~ . - dplyr::lag(.),
      .names = "d_{.col}"
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(d_defence_gdp)) %>%
  dplyr::mutate(
    country   = as.character(country),
    year      = as.integer(year),
    country_f = as.factor(country),
    year_f    = as.factor(year)
  )

formula_fd <- paste(
  "d_defence_gdp ~ d_threat_land_log + d_debt_gdp +",
  "d_deficit_gdp + d_gdp_growth + d_immigration_rate +",
  "d_gov_left_right + d_gov_eu_position + d_election_year"
)

m_sar_fd <- run_sar_pooled(
  data         = reg_data_fd,
  formula_vars = formula_fd,
  sp_weights   = sp_weights,
  label        = "First-difference SAR"
)

if (!is.null(m_sar_fd)) {
  rho_fd <- tryCatch(as.numeric(m_sar_fd$rho), error = function(e) NA)
  se_fd  <- tryCatch({
    se_val <- as.numeric(summary(m_sar_fd)$rho.se)
    if (length(se_val) == 0) NA else se_val
  }, error = function(e) NA)
  if (length(rho_fd) == 0) rho_fd <- NA
  p_fd <- 2 * pnorm(abs(rho_fd / se_fd), lower.tail = FALSE)

  message("First-difference SAR:")
  message("  rho = ", round(rho_fd, 4),
          ", SE = ", round(se_fd, 4),
          ", p = ", round(p_fd, 4))
  message("  Interpretation: ",
          ifelse(!is.na(p_fd) & p_fd < 0.05,
                 "Spatial diffusion survives first-differencing — genuine diffusion",
                 "Spatial lag disappears in first differences — likely persistence artefact"))
}

message("\nVIF check after orthogonalising threat_land_log on debt_gdp:")

lm_orth <- lm(threat_land_log ~ debt_gdp, data = reg_data_ts)

reg_data_orth <- reg_data_ts %>%
  dplyr::filter(!is.na(threat_land_log) & !is.na(debt_gdp)) %>%
  dplyr::mutate(
    country_f   = as.factor(country),
    year_f      = as.factor(year)
  )

reg_data_orth$threat_orth <- NA_real_
reg_data_orth$threat_orth[
  as.integer(rownames(model.frame(lm_orth)))
] <- residuals(lm_orth)

lm_orth_full <- lm(
  defence_gdp ~ threat_orth + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year +
    country_f + year_f,
  data = reg_data_orth
)

vif_orth <- tryCatch(
  car::vif(lm_orth_full),
  error = function(e) NULL
)

vif_orth_df <- NULL
if (!is.null(vif_orth)) {
  if (is.matrix(vif_orth)) {
    vif_orth_df <- data.frame(
      term    = rownames(vif_orth),
      vif_adj = round(vif_orth[, "GVIF^(1/(2*Df))"]^2, 3)
    )
  } else {
    vif_orth_df <- data.frame(
      term    = names(vif_orth),
      vif_adj = round(as.numeric(vif_orth), 3)
    )
  }
  vif_orth_df <- vif_orth_df %>%
    dplyr::filter(!grepl("^country_f|^year_f", term))

  message("VIF after orthogonalisation:")
  print(vif_orth_df)

  coef_threat_orig <- coef(lm(
    defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
      gdp_growth + immigration_rate + gov_left_right +
      gov_eu_position + election_year + country_f + year_f,
    data = reg_data_orth
  ))["threat_land_log"]

  coef_threat_orth <- coef(lm_orth_full)["threat_orth"]

  message("threat_land_log coefficient (original):       ",
          round(coef_threat_orig, 5))
  message("threat_land_log coefficient (orthogonalised): ",
          round(coef_threat_orth, 5))
}

# =============================================================================
# Save all results
# =============================================================================
structural_break_results <- list(
  row_97            = row_97,
  auto_break_year   = auto_break_year,
  bp_comparison     = bp_comparison,
  chow_results      = chow_results,
  lr_tests          = lr_tests,
  regime_comparison = regime_comparison,
  m_sar_auto        = m_sar_auto,
  spatial_asymmetry = spatial_asymmetry,
  asymmetry_test    = asymmetry_test,
  eu_position_test  = eu_position_test,
  m_sar_fd          = m_sar_fd,
  vif_orth          = vif_orth_df,
  supf_test         = supf_test
)

saveRDS(structural_break_results,
        file.path(path_data, "structural_break_results.rds"))

message("\nScript 08_structural_breaks complete.")
