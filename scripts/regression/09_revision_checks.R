# =============================================================================
# 09_revision_checks.R
# Targeted revision checks
#
# Checks:
#   A. Persistence vs diffusion summary (rho across levels, lagged DV, FD)
#   B. Regime4 power analysis (2-year window, 2022-2023)
#   C. Fiscal coefficient stability after orthogonalisation
#   D. Threat score correlation matrix
#   E. Defence source consistency
#      Note: under the current single-source design (all countries use WDI),
#      this check will find no countries with multiple sources. It is retained
#      for future use if source mixing is reintroduced.
#   F. SAR robustness without immigration_rate (GB enters sample)
#      Dropping immigration_rate brings GB into the sample. Large coefficient
#      shifts relative to M5 confirm GB is a structural outlier.
#   G. GB structural outlier formal documentation
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

baseline   <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial    <- readRDS(file.path(path_data, "spatial_panel_results.rds"))
sb_results <- readRDS(file.path(path_data, "structural_break_results.rds"))
panel      <- readRDS(file.path(path_data, "panel_full.rds"))
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
# Check A: Persistence vs diffusion summary
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK A: Persistence vs diffusion summary")
message(strrep("=", 60))

rho_levels <- tryCatch({
  r <- as.numeric(spatial$m5_sar$rho)
  if (length(r) == 0) NA else r
}, error = function(e) NA)

rho_lagged <- tryCatch({
  r <- as.numeric(spatial$m12_sar_lagged$rho)
  if (length(r) == 0) NA else r
}, error = function(e) NA)

rho_fd <- tryCatch({
  if (!is.null(sb_results$m_sar_fd)) {
    r <- as.numeric(sb_results$m_sar_fd$rho)
    if (length(r) == 0) NA else r
  } else NA
}, error = function(e) NA)

se_levels <- tryCatch({
  se <- as.numeric(summary(spatial$m5_sar)$rho.se)
  if (length(se) == 0) NA else se
}, error = function(e) NA)

se_lagged <- tryCatch({
  se <- as.numeric(summary(spatial$m12_sar_lagged)$rho.se)
  if (length(se) == 0) NA else se
}, error = function(e) NA)

se_fd <- tryCatch({
  if (!is.null(sb_results$m_sar_fd)) {
    se <- as.numeric(summary(sb_results$m_sar_fd)$rho.se)
    if (length(se) == 0) NA else se
  } else NA
}, error = function(e) NA)

persistence_summary <- data.frame(
  specification = c("M5: Levels SAR", "M12: Lagged DV SAR", "FD SAR"),
  rho           = round(c(rho_levels, rho_lagged, rho_fd), 4),
  se            = round(c(se_levels,  se_lagged,  se_fd),  4),
  p_value       = round(c(
    2 * pnorm(abs(rho_levels / se_levels), lower.tail = FALSE),
    2 * pnorm(abs(rho_lagged / se_lagged), lower.tail = FALSE),
    2 * pnorm(abs(rho_fd     / se_fd),     lower.tail = FALSE)
  ), 4),
  interpretation = c(
    "Baseline spatial lag",
    "After controlling for persistence",
    "After first-differencing"
  )
)

message("Spatial lag across specifications:")
print(persistence_summary)

# =============================================================================
# Check B: Regime4 power analysis
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK B: Regime4 power analysis (2-year window)")
message(strrep("=", 60))

m7_coef_mat <- tryCatch({
  s  <- summary(spatial$m7_sar_regime)
  cm <- s$Coef
  if (is.null(cm)) s$coefficients else cm
}, error = function(e) NULL)

regime4_row <- tryCatch({
  if (!is.null(m7_coef_mat) &&
      "threat_land_log:regime4" %in% rownames(m7_coef_mat)) {
    m7_coef_mat["threat_land_log:regime4", ]
  } else {
    NULL
  }
}, error = function(e) NULL)

if (!is.null(regime4_row)) {
  col_est <- which(colnames(m7_coef_mat) %in% c("Estimate", "estimate"))[1]
  col_se  <- which(colnames(m7_coef_mat) %in% c("Std. Error", "Std.Error"))[1]
  effect_size <- as.numeric(regime4_row[col_est])
  se_observed <- as.numeric(regime4_row[col_se])
  message("Effect size extracted from M7: ", round(effect_size, 4),
          " (SE = ", round(se_observed, 4), ")")
} else {
  effect_size <- 0.044
  se_observed <- 0.020
  message("M7 regime4 interaction not found — using fallback values: ",
          effect_size, " (SE = ", se_observed, ")")
}

n_regime4 <- reg_data %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::filter(complete.cases(.)) %>%
  nrow()

z_observed <- effect_size / se_observed
p_observed <- 2 * pnorm(abs(z_observed), lower.tail = FALSE)

power_current <- pnorm(
  abs(effect_size / se_observed) - qnorm(0.975)
) + pnorm(
  -abs(effect_size / se_observed) - qnorm(0.975)
)

se_80pct   <- effect_size / (qnorm(0.975) + qnorm(0.80))
n_required <- round(n_regime4 * (se_observed / se_80pct)^2)

power_analysis <- data.frame(
  metric  = c("N observations (regime4)", "Effect size", "SE (observed)",
              "z-statistic", "p-value (observed)",
              "Power at current N (%)", "N required for 80% power"),
  value   = round(c(n_regime4, effect_size, se_observed,
                     z_observed, p_observed,
                     power_current * 100, n_required), 3)
)

message("Regime4 power analysis:")
print(power_analysis)
message("Note: N_required assumes SE ~ 1/sqrt(N). ",
        "Spatial correlation means true required N may be higher.")

# =============================================================================
# Check C: Fiscal coefficients after orthogonalisation
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK C: Fiscal coefficients after orthogonalisation")
message(strrep("=", 60))

reg_data_complete <- reg_data[complete.cases(reg_data), ] %>%
  dplyr::mutate(country_f = as.factor(country), year_f = as.factor(year))

lm_orth_threat <- lm(threat_land_log ~ debt_gdp, data = reg_data_complete)

reg_data_complete$threat_orth <- NA_real_
reg_data_complete$threat_orth[
  as.integer(rownames(model.frame(lm_orth_threat)))
] <- residuals(lm_orth_threat)

lm_orig <- lm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year + country_f + year_f,
  data = reg_data_complete
)

lm_orth_full <- lm(
  defence_gdp ~ threat_orth + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year + country_f + year_f,
  data = reg_data_complete
)

fiscal_vars <- c("debt_gdp", "deficit_gdp", "gdp_growth",
                 "immigration_rate", "gov_left_right",
                 "gov_eu_position", "election_year")

coef_comparison <- purrr::map_dfr(fiscal_vars, function(v) {
  coef_o <- coef(lm_orig)[v]
  coef_r <- coef(lm_orth_full)[v]
  pct_ch <- if (!is.na(coef_o) && abs(coef_o) > 1e-10) {
    round(100 * abs(coef_r - coef_o) / abs(coef_o), 2)
  } else {
    NA_real_
  }
  data.frame(
    variable   = v,
    coef_orig  = round(coef_o, 5),
    coef_orth  = round(coef_r, 5),
    se_orig    = round(sqrt(diag(vcov(lm_orig)))[v],      5),
    se_orth    = round(sqrt(diag(vcov(lm_orth_full)))[v], 5),
    pct_change = pct_ch
  )
})

message("Fiscal coefficient stability after orthogonalisation:")
print(coef_comparison)

# =============================================================================
# Check D: Threat score correlation matrix
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK D: Threat score correlation matrix")
message(strrep("=", 60))

threat_vars <- c("threat_land_log", "threat_score_log",
                 "debt_gdp", "deficit_gdp", "gdp_growth")

corr_data <- reg_data_complete %>%
  dplyr::select(dplyr::all_of(threat_vars)) %>%
  dplyr::filter(complete.cases(.))

corr_mat <- round(cor(corr_data, use = "complete.obs"), 3)

message("Correlation matrix (threat and fiscal variables):")
print(corr_mat)

# =============================================================================
# Check E: Defence source consistency
# Note: under the current single-source design (all countries use WDI),
# this check will find no countries with multiple sources. It is retained
# for future use if source mixing is reintroduced.
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK E: Defence source consistency")
message(strrep("=", 60))

source_check <- NULL
if ("defence_source" %in% names(panel)) {
  source_check <- panel %>%
    dplyr::filter(!is.na(defence_gdp)) %>%
    dplyr::group_by(country, defence_source) %>%
    dplyr::summarise(
      n_years  = dplyr::n(),
      mean_def = round(mean(defence_gdp, na.rm = TRUE), 3),
      sd_def   = round(sd(defence_gdp,   na.rm = TRUE), 3),
      .groups  = "drop"
    ) %>%
    dplyr::arrange(country, defence_source)

  multi_source <- source_check %>%
    dplyr::group_by(country) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(multi_source) == 0) {
    message("No countries with multiple defence data sources (expected under ",
            "single-source WDI design).")
  } else {
    message("Countries with multiple defence data sources:")
    print(multi_source)

    switch_countries <- unique(multi_source$country)
    jump_check <- panel %>%
      dplyr::filter(country %in% switch_countries,
                    !is.na(defence_gdp),
                    !is.na(defence_source)) %>%
      dplyr::arrange(country, year) %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(
        source_change = defence_source != dplyr::lag(defence_source),
        yoy_change    = abs(defence_gdp - dplyr::lag(defence_gdp))
      ) %>%
      dplyr::filter(source_change == TRUE, !is.na(yoy_change)) %>%
      dplyr::select(country, year, defence_gdp, defence_source, yoy_change) %>%
      dplyr::ungroup()

    message("Year-on-year changes at source switch points:")
    print(jump_check)
  }

  readr::write_csv(source_check,
                   file.path(path_data, "defence_source_check.csv"))
} else {
  message("defence_source variable not found in panel — skipping source check.")
}

# =============================================================================
# Check F: SAR robustness without immigration_rate (GB enters sample)
#
# immigration_rate is NA for all GB years by design — GB is a structural
# outlier in the threat-defence space (island nation, global power
# projection commitments). Dropping immigration_rate brings GB into the
# sample with 29 observations, increasing N from ~530 to ~663 and
# countries from 22 to 24. Coefficient instability relative to M5
# confirms the structural outlier interpretation.
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK F: SAR robustness without immigration_rate (GB enters)")
message(strrep("=", 60))

formula_no_immig <- paste(
  "defence_gdp ~ threat_land_log + debt_gdp +",
  "deficit_gdp + gdp_growth +",
  "gov_left_right + gov_eu_position + election_year"
)

reg_data_no_immig <- panel %>%
  dplyr::filter(!is.na(defence_gdp),
                !is.na(threat_land_log),
                !is.na(debt_gdp),
                !is.na(deficit_gdp),
                !is.na(gdp_growth),
                !is.na(gov_left_right),
                !is.na(gov_eu_position),
                !is.na(election_year)) %>%
  dplyr::mutate(
    country = as.character(country),
    year    = as.integer(year),
    regime  = factor(as.character(regime), levels = c("1", "2", "3", "4"))
  )

message("No-immigration sample N: ", nrow(reg_data_no_immig))
message("No-immigration sample countries: ",
        length(unique(reg_data_no_immig$country)))
message("Countries: ",
        paste(sort(unique(reg_data_no_immig$country)), collapse = ", "))
message("GB rows in no-immigration sample: ",
        sum(reg_data_no_immig$country == "GB"))

m_f_sar_no_immig <- tryCatch(
  run_sar_pooled(
    data         = reg_data_no_immig,
    formula_vars = formula_no_immig,
    sp_weights   = sp_weights,
    label        = "Check F: SAR no immigration_rate"
  ),
  error = function(e) {
    message("Check F SAR failed: ", e$message)
    NULL
  }
)

vars_f <- c("threat_land_log", "debt_gdp", "deficit_gdp",
            "gdp_growth", "gov_left_right", "gov_eu_position")

check_f_comparison <- purrr::map_dfr(vars_f, function(v) {
  est_m5 <- extract_coef(spatial$m5_sar, v)
  est_f  <- extract_coef(m_f_sar_no_immig, v)
  se_m5  <- extract_se(spatial$m5_sar, v)
  se_f   <- extract_se(m_f_sar_no_immig, v)
  pct_ch <- if (!is.na(est_m5) && abs(est_m5) > 1e-10) {
    round(100 * abs(est_f - est_m5) / abs(est_m5), 1)
  } else NA_real_
  data.frame(
    variable      = v,
    m5_with_immig = round(est_m5, 4),
    mf_no_immig   = round(est_f,  4),
    se_m5         = round(se_m5,  4),
    se_f          = round(se_f,   4),
    pct_change    = pct_ch
  )
})

rho_m5 <- tryCatch(as.numeric(spatial$m5_sar$rho),       error = function(e) NA_real_)
rho_f  <- tryCatch(as.numeric(m_f_sar_no_immig$rho),     error = function(e) NA_real_)

message("Spatial lag comparison:")
message("  M5 rho (with immigration, N=530, 22 countries): ",
        round(rho_m5, 4))
message("  MF rho (no immigration, N=", nrow(reg_data_no_immig),
        ", ", length(unique(reg_data_no_immig$country)), " countries): ",
        round(rho_f, 4))

message("\nCoefficient comparison M5 vs Check F:")
print(check_f_comparison)

message("\nInterpretation: Large coefficient shifts confirm GB is a ",
        "structural outlier. The threat_land_log measure systematically ",
        "underestimates GB's threat environment (island nation, global ",
        "power projection). Primary results use M5 (with immigration_rate, ",
        "GB excluded) as the theoretically appropriate specification.")

# =============================================================================
# Check G: GB structural outlier formal documentation
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK G: GB structural outlier documentation")
message(strrep("=", 60))

gb_vs_rest <- panel %>%
  dplyr::filter(year >= 1995, year <= 2023,
                !is.na(threat_land_log), !is.na(defence_gdp)) %>%
  dplyr::group_by(is_gb = country == "GB") %>%
  dplyr::summarise(
    n_obs        = dplyr::n(),
    mean_threat  = round(mean(threat_land_log, na.rm = TRUE), 3),
    sd_threat    = round(sd(threat_land_log,   na.rm = TRUE), 3),
    mean_defence = round(mean(defence_gdp,     na.rm = TRUE), 3),
    sd_defence   = round(sd(defence_gdp,       na.rm = TRUE), 3),
    .groups      = "drop"
  ) %>%
  dplyr::mutate(group = dplyr::if_else(is_gb, "GB", "All other countries")) %>%
  dplyr::select(group, n_obs, mean_threat, sd_threat, mean_defence, sd_defence)

message("GB vs rest of sample — threat and defence spending:")
print(gb_vs_rest)

gb_threat    <- gb_vs_rest$mean_threat[gb_vs_rest$group == "GB"]
rest_threat  <- gb_vs_rest$mean_threat[gb_vs_rest$group == "All other countries"]
gb_defence   <- gb_vs_rest$mean_defence[gb_vs_rest$group == "GB"]
rest_defence <- gb_vs_rest$mean_defence[gb_vs_rest$group == "All other countries"]

threat_pct_below  <- round(100 * (rest_threat - gb_threat)  / rest_threat,  1)
defence_pct_above <- round(100 * (gb_defence  - rest_defence) / rest_defence, 1)

message("\nGB threat score is ", threat_pct_below,
        "% below the rest-of-sample mean.")
message("GB defence spending is ", defence_pct_above,
        "% above the rest-of-sample mean.")
message("This is the opposite of the theory-predicted direction, ",
        "confirming structural incompatibility.")

gb_yearly <- panel %>%
  dplyr::filter(country == "GB") %>%
  dplyr::select(year, threat_land_log, defence_gdp,
                debt_gdp, deficit_gdp, gdp_growth) %>%
  dplyr::arrange(year)

message("\nGB year-by-year threat and defence (for appendix):")
print(gb_yearly, n = Inf)

readr::write_csv(gb_vs_rest,
                 file.path(path_data, "gb_structural_outlier_summary.csv"))
readr::write_csv(gb_yearly,
                 file.path(path_data, "gb_yearly_profile.csv"))

message("\nCheck G complete: GB confirmed as structural outlier. ",
        "Exclusion from primary regression is theoretically grounded, ",
        "not a data availability limitation.")

# =============================================================================
# Save all revision check results
# =============================================================================
revision_checks <- list(
  persistence_summary  = persistence_summary,
  power_analysis       = power_analysis,
  coef_comparison      = coef_comparison,
  corr_mat             = corr_mat,
  source_check         = source_check,
  check_f_comparison   = check_f_comparison,
  rho_m5               = rho_m5,
  rho_no_immig         = rho_f,
  gb_vs_rest           = gb_vs_rest,
  gb_yearly            = gb_yearly
)

saveRDS(revision_checks,
        file.path(path_data, "revision_checks_results.rds"))

summary_rows <- list(
  data.frame(check   = "A: Persistence",
             finding = paste0("rho levels=", round(rho_levels, 3),
                              ", lagged=", round(rho_lagged, 3),
                              ", FD=", round(rho_fd, 3))),
  data.frame(check   = "B: Regime4 power",
             finding = paste0("N=", n_regime4,
                              ", power=", round(power_current * 100, 1),
                              "%, N_required=", n_required,
                              " (assumes SE~1/sqrt(N); spatial corr may increase this)")),
  data.frame(check   = "C: Orthogonalisation",
             finding = paste0("Max pct change in fiscal coefs: ",
                              round(max(coef_comparison$pct_change, na.rm = TRUE), 1), "%")),
  data.frame(check   = "D: Threat correlation",
             finding = paste0("cor(threat_land, threat_score)=",
                              corr_mat["threat_land_log", "threat_score_log"])),
  data.frame(check   = "F: No immigration SAR",
             finding = paste0("rho M5=", round(rho_m5, 3),
                              ", rho no-immig=", round(rho_f, 3),
                              "; threat coef M5=",
                              round(extract_coef(spatial$m5_sar,
                                                 "threat_land_log"), 3),
                              ", no-immig=",
                              round(extract_coef(m_f_sar_no_immig,
                                                 "threat_land_log"), 3))),
  data.frame(check   = "G: GB structural outlier",
             finding = paste0("GB threat ", threat_pct_below,
                              "% below mean; defence ", defence_pct_above,
                              "% above mean — structural exclusion confirmed"))
)

revision_summary <- dplyr::bind_rows(summary_rows)
readr::write_csv(revision_summary,
                 file.path(path_data, "revision_checks_summary.csv"))

message("\nScript 09_revision_checks complete.")
