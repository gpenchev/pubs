# =============================================================================
# 11_app_data.R
# Prepare all app-ready data for the Shiny visualisation app.
#
# Reads from scripts/output/data/ and scripts/output/quality_reports/
# Writes 23 flat CSV files to scripts/output/app/
#
# The app folder is fully autonomous: the Shiny app reads ONLY from app/
# (CSVs) and the models/results/*.md and methodology/models/*.md files.
# No R model objects, no pipeline packages needed by the app itself.
#
# Blocks:
#   1. Core panel + conflict events
#   2. Threat & GPR summaries
#   3. Unified coefficient table M1-M13
#   4. Model fit, rho comparison, country FE, regime effects
#   5. Robustness check tables (copies + reshapes)
#   6. Specific issues tables (kinetic bias, Greece, coverage, GPR map)
#   7. Markdown path lookup table
# =============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "helpers", "spatial_helpers.R"))

# --- Output directory ---------------------------------------------------------
path_app <- file.path(path_root, "scripts", "output", "app")
dir.create(path_app, showWarnings = FALSE, recursive = TRUE)
message("App data directory: ", path_app)

# --- Load all source files ----------------------------------------------------
message("\nLoading source data...")

panel        <- readRDS(file.path(path_data, "panel_full.rds"))
baseline     <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial      <- readRDS(file.path(path_data, "spatial_panel_results.rds"))
sb_results   <- readRDS(file.path(path_data, "structural_break_results.rds"))
diag_results <- readRDS(file.path(path_data, "diagnostics_results.rds"))
reg_tables   <- readRDS(file.path(path_data, "regression_tables.rds"))
rev_checks   <- readRDS(file.path(path_data, "revision_checks_results.rds"))
gpr_results  <- readRDS(file.path(path_data, "gpr_comparison_results.rds"))
map_events   <- readRDS(file.path(path_data, "ucdp_map_events.rds"))

reg_long     <- readr::read_csv(file.path(path_data, "regression_results_long.csv"),
                                 show_col_types = FALSE)

message("All source files loaded.")

# Helper: safe p-value from coef + se
safe_p <- function(cf, se) {
  2 * pnorm(abs(cf / se), lower.tail = FALSE)
}

# Helper: write CSV and report
write_app <- function(df, name) {
  path <- file.path(path_app, name)
  readr::write_csv(df, path)
  message("  Written: ", name, " (", nrow(df), " rows x ", ncol(df), " cols)")
  invisible(df)
}

# =============================================================================
# BLOCK 1 — Core panel extract + conflict events
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 1: Core panel + conflict events")
message(strrep("=", 60))

# app_threat_panel.csv — country x year panel, all key variables
app_threat_panel <- panel %>%
  dplyr::select(
    country, year, regime,
    defence_gdp, threat_land_log, threat_score_log,
    debt_gdp, deficit_gdp, gdp_growth, immigration_rate,
    gov_left_right, gov_eu_position, election_year
  ) %>%
  dplyr::arrange(country, year)

write_app(app_threat_panel, "app_threat_panel.csv")

# app_conflict_events.csv — land-contiguous events only, aggregated to
# 0.1-degree grid x year for fast map rendering (~787 rows)
app_conflict_events <- map_events %>%
  dplyr::filter(land_contiguous == TRUE) %>%
  dplyr::mutate(
    lon_grid = round(lon, 1),
    lat_grid = round(lat, 1)
  ) %>%
  dplyr::group_by(lon_grid, lat_grid, year) %>%
  dplyr::summarise(
    fatalities  = sum(best, na.rm = TRUE),
    n_events    = dplyr::n(),
    .groups     = "drop"
  ) %>%
  dplyr::arrange(year, dplyr::desc(fatalities))

write_app(app_conflict_events, "app_conflict_events.csv")

# =============================================================================
# BLOCK 2 — Threat & GPR summaries
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 2: Threat & GPR summaries")
message(strrep("=", 60))

# app_threat_country.csv — per-country threat statistics
app_threat_country <- readr::read_csv(
  file.path(path_reports, "threat_country_summary.csv"),
  show_col_types = FALSE
) %>%
  dplyr::rename(
    mean_threat   = mean_land_log,
    sd_threat     = sd_land_log,
    min_threat    = min_land_log,
    max_threat    = max_land_log,
    n_zero        = n_zero_land,
    pct_zero      = pct_zero_land,
    mean_score    = mean_score_log,
    sd_score      = sd_score_log
  )

write_app(app_threat_country, "app_threat_country.csv")

# app_gpr_correlation.csv — GPR vs UCDP correlation by country
app_gpr_correlation <- readr::read_csv(
  file.path(path_reports, "gpr_correlation_summary.csv"),
  show_col_types = FALSE
)
write_app(app_gpr_correlation, "app_gpr_correlation.csv")

# app_gpr_divergence_year.csv — mean divergence by year
app_gpr_divergence_year <- readr::read_csv(
  file.path(path_reports, "gpr_divergence_by_year.csv"),
  show_col_types = FALSE
)
write_app(app_gpr_divergence_year, "app_gpr_divergence_year.csv")

# app_gpr_panel.csv — country x year with both threat_land_log and gpr_mean
# Used for Tab 1 scatter and Tab 4 Issue 1 (Crimea 2014)
app_gpr_panel <- gpr_results$panel_gpr %>%
  dplyr::select(country, year, regime,
                threat_land_log, threat_score_log, gpr_mean) %>%
  dplyr::arrange(country, year)

write_app(app_gpr_panel, "app_gpr_panel.csv")

# =============================================================================
# BLOCK 3 — Unified coefficient table M1-M13
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 3: Unified coefficient table M1-M13")
message(strrep("=", 60))

# Substantive variables (no FE dummies)
subst_vars <- c(
  "threat_land_log", "gpr_log",
  "debt_gdp", "deficit_gdp", "gdp_growth",
  "immigration_rate", "gov_left_right", "gov_eu_position", "election_year",
  "lag_defence_gdp",
  "threat_land_log:regime2", "threat_land_log:regime3",
  "threat_land_log:regime4"
)

# --- M1: Pooled OLS ---
extract_plm_coefs <- function(m, model_label, se_vec = NULL) {
  cf <- coef(m)
  if (is.null(se_vec)) {
    se_vec <- tryCatch(
      sqrt(diag(sandwich::vcovHC(m, type = "HC3"))),
      error = function(e) sqrt(diag(vcov(m)))
    )
  }
  keep <- names(cf)[names(cf) %in% subst_vars]
  if (length(keep) == 0) return(NULL)
  pv <- safe_p(cf[keep], se_vec[keep])
  data.frame(
    model      = model_label,
    term       = keep,
    estimate   = round(cf[keep], 6),
    std_error  = round(se_vec[keep], 6),
    p_value    = round(pv, 4),
    significant = pv < 0.05,
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
}

m1_coefs <- extract_plm_coefs(baseline$m1_pooled, "M1: Pooled OLS")
m2_coefs <- extract_plm_coefs(baseline$m2_fe_country, "M2: Country FE")
m4_coefs <- extract_plm_coefs(
  baseline$m4_fe_regime, "M4: FE + Regime",
  se_vec = baseline$se_m4
)

# --- M3-M12 from existing long table (already clean) ---
# Standardise model labels to match
m3_m12_coefs <- reg_long %>%
  dplyr::filter(term %in% subst_vars) %>%
  dplyr::rename(
    std_error = std_error,
    p_value   = p_value
  ) %>%
  dplyr::mutate(
    estimate    = round(estimate,   6),
    std_error   = round(std_error,  6),
    p_value     = round(p_value,    4),
    significant = p_value < 0.05
  ) %>%
  dplyr::select(model, term, estimate, std_error, p_value, significant)

# --- M13: GPR SAR ---
m13     <- gpr_results$m13_sar
cf_m13  <- coef(m13)[-1]          # drop rho
se_m13  <- setNames(m13$rest.se, names(coef(m13))[-1])
pv_m13  <- safe_p(cf_m13, se_m13)
m13_keep <- names(cf_m13)[names(cf_m13) %in% c(subst_vars, "gpr_log")]

m13_coefs <- data.frame(
  model       = "M13: GPR SAR",
  term        = m13_keep,
  estimate    = round(cf_m13[m13_keep], 6),
  std_error   = round(se_m13[m13_keep], 6),
  p_value     = round(pv_m13[m13_keep], 4),
  significant = pv_m13[m13_keep] < 0.05,
  row.names   = NULL,
  stringsAsFactors = FALSE
)

# --- Add rho/lambda rows for spatial models ---
# M6 SEM uses $lambda / $lambda.se; all SAR models use $rho / $rho.se
extract_spatial_param_row <- function(label, m) {
  is_sem <- grepl("SEM", label, fixed = FALSE)
  val <- tryCatch(
    as.numeric(if (is_sem) m$lambda    else m$rho),
    error = function(e) NA_real_
  )
  se <- tryCatch(
    as.numeric(if (is_sem) m$lambda.se else m$rho.se),
    error = function(e) NA_real_
  )
  pv  <- tryCatch(safe_p(val, se), error = function(e) NA_real_)
  term_name <- if (is_sem) "lambda" else "rho"
  data.frame(
    model       = label,
    term        = term_name,
    estimate    = round(val, 6),
    std_error   = round(se,  6),
    p_value     = round(pv,  4),
    significant = !is.na(pv) & pv < 0.05,
    stringsAsFactors = FALSE
  )
}

rho_rows <- dplyr::bind_rows(
  extract_spatial_param_row("M5: SAR",              spatial$m5_sar),
  extract_spatial_param_row("M6: SEM",              spatial$m6_sem),
  extract_spatial_param_row("M7: SAR + Regime",     spatial$m7_sar_regime),
  extract_spatial_param_row("M8: SAR Robust threat",spatial$m8_sar_robust),
  extract_spatial_param_row("M9: SAR inv.dist",     spatial$m9_sar_invdist),
  extract_spatial_param_row("M10a: SAR no Finland", spatial$m10a_sar_nofi),
  extract_spatial_param_row("M10b: SAR post-2014",  spatial$m10b_sar_post2014),
  extract_spatial_param_row("M10c: SAR pre-2014",   spatial$m10c_sar_pre2014),
  extract_spatial_param_row("M12: SAR lagged DV",   spatial$m12_sar_lagged),
  extract_spatial_param_row("M13: GPR SAR",         gpr_results$m13_sar)
)

# --- Bind all ---
app_coef_long <- dplyr::bind_rows(
  m1_coefs, m2_coefs, m3_m12_coefs, m4_coefs, m13_coefs, rho_rows
) %>%
  dplyr::mutate(
    model_order = dplyr::case_when(
      grepl("^M1:",  model) ~ 1,  grepl("^M2:",  model) ~ 2,
      grepl("^M3:",  model) ~ 3,  grepl("^M4:",  model) ~ 4,
      grepl("^M5:",  model) ~ 5,  grepl("^M6:",  model) ~ 6,
      grepl("^M7:",  model) ~ 7,  grepl("^M8:",  model) ~ 8,
      grepl("^M9:",  model) ~ 9,  grepl("^M10a:", model) ~ 10,
      grepl("^M10b:", model) ~ 11, grepl("^M10c:", model) ~ 12,
      grepl("^M12:", model) ~ 13, grepl("^M13:", model) ~ 14,
      TRUE ~ 99
    )
  ) %>%
  dplyr::arrange(model_order, term) %>%
  dplyr::select(-model_order)

write_app(app_coef_long, "app_coef_long.csv")

# =============================================================================
# BLOCK 4 — Model fit, rho comparison, country FE, regime effects
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 4: Model fit, rho, country FE, regime effects")
message(strrep("=", 60))

# app_model_fit.csv — N, AIC, log-lik, rho per model
# N obs confirmed from pipeline runs (hard-coded from run outputs)
model_n <- c(
  "M1: Pooled OLS"           = 529,
  "M2: Country FE"           = 529,
  "M3: Two-way FE"           = 529,
  "M4: FE + Regime"          = 529,
  "M5: SAR"                  = 517,
  "M6: SEM"                  = 517,
  "M7: SAR + Regime"         = 517,
  "M8: SAR Robust threat"    = 517,
  "M9: SAR inv.dist"         = 529,
  "M10a: SAR no Finland"     = 465,
  "M10b: SAR post-2014"      = 220,
  "M10c: SAR pre-2014"       = 297,
  "M12: SAR lagged DV"       = 495,
  "M13: GPR SAR"             = 377
)

# Base: M5-M12 from fit_summary
base_fit <- reg_tables$fit_summary %>%
  dplyr::rename(log_lik_val = log_lik, aic_val = aic)

# M1-M4 AIC via logLik on lm/plm objects
get_plm_fit <- function(m, label) {
  ll  <- tryCatch(as.numeric(logLik(m)),  error = function(e) NA_real_)
  aic <- tryCatch(AIC(m),                 error = function(e) NA_real_)
  data.frame(model = label, log_lik_val = round(ll, 2), aic_val = round(aic, 2))
}

m1m4_fit <- dplyr::bind_rows(
  get_plm_fit(baseline$m1_pooled,     "M1: Pooled OLS"),
  get_plm_fit(baseline$m2_fe_country, "M2: Country FE"),
  get_plm_fit(baseline$m3_fe_twoway,  "M3: Two-way FE"),
  get_plm_fit(baseline$m4_fe_regime,  "M4: FE + Regime")
)

# M13 from gpr_aic_comparison
m13_fit <- data.frame(
  model       = "M13: GPR SAR",
  log_lik_val = round(gpr_results$m13_sar$LL, 2),
  aic_val     = round(
    readr::read_csv(file.path(path_reports, "gpr_aic_comparison.csv"),
                    show_col_types = FALSE) %>%
      dplyr::filter(grepl("M13", model)) %>%
      dplyr::pull(aic),
    2)
)

# Rho/lambda values per spatial model (M6 SEM uses lambda)
extract_rho_val <- function(label, m) {
  is_sem <- grepl("SEM", label, fixed = FALSE)
  val <- tryCatch(round(as.numeric(if (is_sem) m$lambda    else m$rho),    4), error = function(e) NA_real_)
  se  <- tryCatch(round(as.numeric(if (is_sem) m$lambda.se else m$rho.se), 4), error = function(e) NA_real_)
  pv  <- tryCatch(round(safe_p(val, se), 4), error = function(e) NA_real_)
  data.frame(model = label, rho = val, rho_se = se, rho_p = pv,
             stringsAsFactors = FALSE)
}

rho_vals <- dplyr::bind_rows(
  extract_rho_val("M5: SAR",              spatial$m5_sar),
  extract_rho_val("M6: SEM",              spatial$m6_sem),
  extract_rho_val("M7: SAR + Regime",     spatial$m7_sar_regime),
  extract_rho_val("M8: SAR Robust threat",spatial$m8_sar_robust),
  extract_rho_val("M9: SAR inv.dist",     spatial$m9_sar_invdist),
  extract_rho_val("M10a: SAR no Finland", spatial$m10a_sar_nofi),
  extract_rho_val("M10b: SAR post-2014",  spatial$m10b_sar_post2014),
  extract_rho_val("M10c: SAR pre-2014",   spatial$m10c_sar_pre2014),
  extract_rho_val("M12: SAR lagged DV",   spatial$m12_sar_lagged),
  extract_rho_val("M13: GPR SAR",         gpr_results$m13_sar)
)

app_model_fit <- dplyr::bind_rows(m1m4_fit, base_fit) %>%
  dplyr::bind_rows(m13_fit) %>%
  dplyr::left_join(rho_vals, by = "model") %>%
  dplyr::mutate(
    n_obs = model_n[model],
    type  = dplyr::case_when(
      grepl("Pooled OLS", model) ~ "Pooled OLS",
      grepl("Country FE", model) ~ "Country FE",
      grepl("Two-way FE|FE \\+", model) ~ "Two-way FE",
      grepl("SEM", model) ~ "SEM",
      grepl("GPR", model) ~ "SAR (GPR)",
      TRUE ~ "SAR"
    )
  ) %>%
  dplyr::arrange(dplyr::case_when(
    grepl("^M1",  model) ~ 1,  grepl("^M2",  model) ~ 2,
    grepl("^M3",  model) ~ 3,  grepl("^M4",  model) ~ 4,
    grepl("^M5",  model) ~ 5,  grepl("^M6",  model) ~ 6,
    grepl("^M7",  model) ~ 7,  grepl("^M8",  model) ~ 8,
    grepl("^M9",  model) ~ 9,  grepl("^M10a",model) ~ 10,
    grepl("^M10b",model) ~ 11, grepl("^M10c",model) ~ 12,
    grepl("^M12", model) ~ 13, grepl("^M13", model) ~ 14,
    TRUE ~ 99
  )) %>%
  dplyr::select(model, type, n_obs, log_lik_val, aic_val, rho, rho_se, rho_p)

write_app(app_model_fit, "app_model_fit.csv")

# app_rho_comparison.csv — rho across all spatial specs + FD SAR
fd_rho <- tryCatch(as.numeric(sb_results$m_sar_fd$rho),    error = function(e) NA_real_)
fd_se  <- tryCatch(as.numeric(sb_results$m_sar_fd$rho.se), error = function(e) NA_real_)

app_rho_comparison <- dplyr::bind_rows(
  rho_vals,
  data.frame(
    model  = "FD SAR (first-difference)",
    rho    = round(fd_rho, 4),
    rho_se = round(fd_se,  4),
    rho_p  = round(safe_p(fd_rho, fd_se), 4),
    stringsAsFactors = FALSE
  )
) %>%
  dplyr::mutate(
    n_obs = dplyr::coalesce(model_n[model], NA_integer_),
    note  = dplyr::case_when(
      grepl("M5: SAR$",       model) ~ "Primary specification",
      grepl("lagged DV",      model) ~ "After controlling for spending persistence",
      grepl("first-difference", model) ~ "After first-differencing (removes persistence)",
      grepl("post-2014",      model) ~ "Post-2014 subsample",
      grepl("pre-2014",       model) ~ "Pre-2014 subsample",
      grepl("no Finland",     model) ~ "Excluding Finland",
      grepl("inv.dist",       model) ~ "Inverse-distance W matrix",
      grepl("Robust threat",  model) ~ "All-events threat measure",
      grepl("Regime",         model) ~ "With regime x threat interactions",
      grepl("SEM",            model) ~ "Spatial error model",
      grepl("GPR",            model) ~ "GPR proxy (13-country subsample)",
      TRUE ~ ""
    )
  )

write_app(app_rho_comparison, "app_rho_comparison.csv")

# app_country_fe.csv — country fixed effects from M5
m5      <- spatial$m5_sar
cf_m5   <- coef(m5)[-1]
se_m5   <- setNames(m5$rest.se, names(coef(m5))[-1])
pv_m5   <- safe_p(cf_m5, se_m5)

fe_idx  <- grep("^country_f", names(cf_m5))
fe_ctrs <- sub("^country_f", "", names(cf_m5)[fe_idx])

app_country_fe <- dplyr::bind_rows(
  # BE is baseline (absorbed), fe = 0
  data.frame(country = "BE", fe_value = 0, se = NA_real_,
             z = NA_real_, p_value = NA_real_, stringsAsFactors = FALSE),
  data.frame(
    country  = fe_ctrs,
    fe_value = round(cf_m5[fe_idx], 5),
    se       = round(se_m5[fe_idx], 5),
    z        = round(cf_m5[fe_idx] / se_m5[fe_idx], 3),
    p_value  = round(pv_m5[fe_idx], 4),
    stringsAsFactors = FALSE
  )
) %>%
  dplyr::arrange(dplyr::desc(fe_value))

write_app(app_country_fe, "app_country_fe.csv")

# app_regime_effects.csv — regime net threat effects from M4
cf_m4 <- coef(baseline$m4_fe_regime)
se_m4 <- baseline$se_m4

base_coef <- cf_m4["threat_land_log"]
base_se   <- se_m4["threat_land_log"]

regime_labels <- c("1995-2004", "2005-2013", "2014-2021", "2022-2023")

app_regime_effects <- purrr::map_dfr(1:4, function(i) {
  if (i == 1) {
    int_cf <- 0; int_se <- NA_real_; int_p <- NA_real_
  } else {
    vname  <- paste0("threat_land_log:regime", i)
    int_cf <- tryCatch(cf_m4[vname], error = function(e) NA_real_)
    int_se <- tryCatch(se_m4[vname], error = function(e) NA_real_)
    int_p  <- tryCatch(safe_p(int_cf, int_se), error = function(e) NA_real_)
  }
  net    <- base_coef + int_cf
  net_se <- tryCatch(sqrt(base_se^2 + int_se^2), error = function(e) NA_real_)
  data.frame(
    regime            = i,
    label             = regime_labels[i],
    base_coef         = round(base_coef, 5),
    interaction_coef  = round(int_cf,    5),
    net_coef          = round(net,       5),
    se_net            = round(net_se,    5),
    p_interaction     = round(int_p,     4),
    stringsAsFactors  = FALSE
  )
})

write_app(app_regime_effects, "app_regime_effects.csv")

# =============================================================================
# BLOCK 5 — Robustness check tables
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 5: Robustness check tables")
message(strrep("=", 60))

# Simple copies / renames
copy_to_app <- function(src_path, dest_name) {
  df <- readr::read_csv(src_path, show_col_types = FALSE)
  write_app(df, dest_name)
}

copy_to_app(file.path(path_data,    "revision_checks_summary.csv"), "app_checks_summary.csv")
copy_to_app(file.path(path_reports, "cross_section_2022_2023.csv"), "app_check_i.csv")
copy_to_app(file.path(path_data,    "bg2019_sensitivity.csv"),       "app_check_h.csv")
copy_to_app(file.path(path_reports, "immigration_interaction_check.csv"), "app_check_j.csv")
copy_to_app(file.path(path_data,    "vif_results.csv"),              "app_vif.csv")
copy_to_app(file.path(path_data,    "influence_by_country.csv"),     "app_influence_country.csv")

# app_regime_lr.csv — bind SAR LR test + regime AIC comparison
regime_lr  <- readr::read_csv(file.path(path_data, "regime_sar_lr_test.csv"),
                               show_col_types = FALSE)
regime_aic <- readr::read_csv(file.path(path_data, "regime_comparison_aic.csv"),
                               show_col_types = FALSE)

app_regime_lr <- dplyr::bind_rows(
  regime_lr  %>% dplyr::mutate(source = "SAR LR test"),
  regime_aic %>% dplyr::mutate(source = "AIC comparison")
)
write_app(app_regime_lr, "app_regime_lr.csv")

# =============================================================================
# BLOCK 6 — Specific issues tables
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 6: Specific issues")
message(strrep("=", 60))

# app_issue1_crimea.csv — threat vs GPR 2010-2023: Crimea kinetic bias + Ukraine spike
# Shows: GPR spikes at Crimea 2014 (UCDP flat) AND both spike at Ukraine 2022
app_issue1_crimea <- gpr_results$panel_gpr %>%
  dplyr::filter(year >= 2010) %>%
  dplyr::select(country, year, threat_land_log, gpr_mean) %>%
  dplyr::arrange(country, year)

write_app(app_issue1_crimea, "app_issue1_crimea.csv")

# app_issue2_greece.csv — Greece vs sample mean (50km threshold / Mediterranean)
gr_data <- panel %>%
  dplyr::filter(country == "GR") %>%
  dplyr::select(year, gr_threat = threat_land_log, gr_defence = defence_gdp)

sample_means <- panel %>%
  dplyr::filter(!country %in% c("GB", "LU")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean_threat  = round(mean(threat_land_log, na.rm = TRUE), 4),
    mean_defence = round(mean(defence_gdp,     na.rm = TRUE), 4),
    .groups      = "drop"
  )

app_issue2_greece <- gr_data %>%
  dplyr::left_join(sample_means, by = "year") %>%
  dplyr::arrange(year)

write_app(app_issue2_greece, "app_issue2_greece.csv")

# app_issue3_coverage.csv — immigration data coverage heatmap
# Shows Regime 1 truncation: 1995-1999 fully missing for all countries
app_issue3_coverage <- panel %>%
  dplyr::filter(!country %in% c("GB", "LU")) %>%
  dplyr::select(country, year, immigration_rate) %>%
  dplyr::mutate(has_immigration = !is.na(immigration_rate)) %>%
  dplyr::select(country, year, has_immigration) %>%
  dplyr::arrange(country, year)

write_app(app_issue3_coverage, "app_issue3_coverage.csv")

# app_issue4_gpr_coverage.csv — which countries have GPR data (map data)
# Highlights missing Eastern European frontline states
eastern_eu <- c("BG", "CZ", "EE", "HR", "HU", "LT", "LV", "PL", "RO", "SI", "SK")
gpr_covered <- gpr_results$gpr_countries

mean_threats <- panel %>%
  dplyr::filter(!country %in% c("GB", "LU")) %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    mean_threat    = round(mean(threat_land_log, na.rm = TRUE), 4),
    mean_defence   = round(mean(defence_gdp,     na.rm = TRUE), 4),
    .groups        = "drop"
  )

app_issue4_gpr_coverage <- panel %>%
  dplyr::distinct(country) %>%
  dplyr::filter(!country %in% c("LU")) %>%
  dplyr::mutate(
    in_gpr  = country %in% gpr_covered,
    region  = dplyr::case_when(
      country == "GB"               ~ "Island (GB)",
      country %in% eastern_eu       ~ "Eastern EU",
      TRUE                          ~ "Western/Northern EU"
    )
  ) %>%
  dplyr::left_join(mean_threats, by = "country") %>%
  dplyr::arrange(region, country)

write_app(app_issue4_gpr_coverage, "app_issue4_gpr_coverage.csv")

# =============================================================================
# BLOCK 7 — Markdown path lookup table
# =============================================================================
message("\n", strrep("=", 60))
message("BLOCK 7: Markdown path lookup table")
message(strrep("=", 60))

app_md_paths <- tibble::tribble(
  ~tab,         ~section,      ~md_file,                                     ~heading,
  "about",      "intro",       "methodology/models/brief.md",                "full",
  "threat",     "results",     "models/results/threat_index.md",             "full",
  "threat",     "methods",     "methodology/models/threat.md",               "full",
  "threat",     "variables",   "methodology/models/variables.md",            "full",
  "estimation", "results",     "models/results/m1_m12.md",                   "full",
  "estimation", "methods",     "methodology/models/models.md",               "full",
  "estimation", "naive",       "methodology/models/naive.md",                "full",
  "robustness", "results",     "models/results/diagnostics_breaks.md",       "full",
  "robustness", "extra",       "models/results/revision_checks_ij.md",       "full",
  "robustness", "weaknesses",  "methodology/models/weak.md",                 "full",
  "issues",     "kinetic",     "methodology/models/weak.md",                 "## 9.",
  "issues",     "threshold",   "methodology/models/weak.md",                 "## 10.",
  "issues",     "truncation",  "models/results/m1_m12.md",                   "## 1.",
  "issues",     "gpr_bias",    "models/results/gpr_results.md",              "## 7."
)

write_app(app_md_paths, "app_md_paths.csv")

# =============================================================================
# FINAL REPORT
# =============================================================================
message("\n", strrep("=", 60))
message("11_app_data.R COMPLETE")
message(strrep("=", 60))

app_files <- list.files(path_app, pattern = "\\.csv$")
message("Files written to scripts/output/app/ (", length(app_files), " total):")
for (f in sort(app_files)) {
  n <- nrow(readr::read_csv(file.path(path_app, f), show_col_types = FALSE))
  message("  ", formatC(f, width = 40, flag = "-"), n, " rows")
}
