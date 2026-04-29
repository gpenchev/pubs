# =============================================================================
# 12_unit_root_check.R
# Panel unit root and stationarity tests
# Runs on v1 panel (before ParlGov merge).
# Political variables (gov_left_right, gov_eu_position) are not yet present —
# they are checked in 14_parlgov_quality_check.R.
# Methods: ADF (per country), KPSS (per country), Im-Pesaran-Shin panel test
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")

# --- Load panel ---------------------------------------------------------------
panel <- readRDS(file.path(path_data, "panel_full.rds"))

# --- Variables to test — intersect with panel columns to avoid errors ---------
vars_candidate <- c(
  "defence_gdp",
  "debt_gdp",
  "deficit_gdp",
  "gdp_pc",
  "gdp_growth",
  "immigration_rate",
  "threat_score_log",
  "threat_land_log",
  "gov_left_right",
  "gov_eu_position"
)

vars_to_test <- intersect(vars_candidate, names(panel))

missing_vars <- setdiff(vars_candidate, vars_to_test)
if (length(missing_vars) > 0) {
  message("Variables not yet in panel (will be tested after ParlGov merge): ",
          paste(missing_vars, collapse = ", "))
}

# --- ADF test per country per variable ----------------------------------------
run_adf <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 10) return(data.frame(adf_stat = NA, adf_pval = NA))
  result <- tryCatch(
    tseries::adf.test(x_clean, alternative = "stationary"),
    error = function(e) NULL
  )
  if (is.null(result)) return(data.frame(adf_stat = NA, adf_pval = NA))
  data.frame(adf_stat = result$statistic, adf_pval = result$p.value)
}

adf_results <- purrr::map_dfr(vars_to_test, function(v) {
  panel %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      result  = list(run_adf(.data[[v]])),
      .groups = "drop"
    ) %>%
    tidyr::unnest(result) %>%
    dplyr::mutate(variable = v)
})

# --- KPSS test per country per variable ---------------------------------------
run_kpss <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 10) return(data.frame(kpss_stat = NA, kpss_pval = NA))
  result <- tryCatch(
    tseries::kpss.test(x_clean, null = "Level"),
    error = function(e) NULL
  )
  if (is.null(result)) return(data.frame(kpss_stat = NA, kpss_pval = NA))
  data.frame(kpss_stat = result$statistic, kpss_pval = result$p.value)
}

kpss_results <- purrr::map_dfr(vars_to_test, function(v) {
  panel %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      result  = list(run_kpss(.data[[v]])),
      .groups = "drop"
    ) %>%
    tidyr::unnest(result) %>%
    dplyr::mutate(variable = v)
})

# --- Im-Pesaran-Shin (IPS) panel unit root test -------------------------------
run_ips <- function(panel_data, variable) {
  tryCatch({
    df_wide <- panel_data %>%
      dplyr::filter(!is.na(.data[[variable]])) %>%
      dplyr::select(country, year, dplyr::all_of(variable)) %>%
      tidyr::pivot_wider(names_from = country, values_from = dplyr::all_of(variable)) %>%
      dplyr::arrange(year) %>%
      dplyr::select(-year)

    if (ncol(df_wide) < 3 || nrow(df_wide) < 10) return(NULL)

    df_wide <- df_wide[, colSums(!is.na(df_wide)) >= 10, drop = FALSE]
    if (ncol(df_wide) < 3) return(NULL)

    # Check all columns have equal length after NA removal
    col_lengths <- colSums(!is.na(df_wide))
    if (length(unique(col_lengths)) > 1) {
      df_wide <- df_wide[, col_lengths == max(col_lengths), drop = FALSE]
    }
    if (ncol(df_wide) < 3) return(NULL)

    ips_result <- plm::purtest(
      df_wide,
      test   = "ips",
      exo    = "intercept",
      lags   = "AIC",
      pmax   = 4
    )

    stat  <- tryCatch(as.numeric(ips_result$statistic$statistic), error = function(e) NA)
    pval  <- tryCatch(as.numeric(ips_result$statistic$p.value),   error = function(e) NA)
    if (length(stat) == 0) stat <- NA
    if (length(pval) == 0) pval <- NA

    data.frame(
      variable   = variable,
      ips_stat   = round(stat, 4),
      ips_pval   = round(pval, 4),
      n_panels   = ncol(df_wide),
      conclusion = ifelse(!is.na(pval) & pval < 0.05,
                          "Reject H0: some panels stationary",
                          "Fail to reject H0: unit root not rejected")
    )
  }, error = function(e) {
    data.frame(variable = variable, ips_stat = NA, ips_pval = NA,
               n_panels = NA, conclusion = paste("Failed:", e$message))
  })
}

ips_results <- purrr::map_dfr(vars_to_test, run_ips, panel_data = panel)

message("Im-Pesaran-Shin panel unit root test results:")
print(ips_results)

# --- Summary ------------------------------------------------------------------
adf_summary <- adf_results %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_tested          = sum(!is.na(adf_pval)),
    n_reject_unitroot = sum(adf_pval < 0.05, na.rm = TRUE),
    pct_stationary    = round(n_reject_unitroot / n_tested * 100, 1),
    .groups           = "drop"
  )

kpss_summary <- kpss_results %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_tested              = sum(!is.na(kpss_pval)),
    n_reject_stationarity = sum(kpss_pval < 0.05, na.rm = TRUE),
    pct_nonstationary     = round(n_reject_stationarity / n_tested * 100, 1),
    .groups               = "drop"
  )

unitroot_results <- adf_results %>%
  dplyr::left_join(kpss_results, by = c("country", "variable"))

# --- ADF p-value distribution plot -------------------------------------------
adf_plot <- ggplot2::ggplot(
  adf_results %>% dplyr::filter(!is.na(adf_pval)),
  ggplot2::aes(x = adf_pval)
) +
  ggplot2::geom_histogram(bins = 20, fill = "#2171b5", colour = "white") +
  ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed", colour = "red") +
  ggplot2::facet_wrap(~ variable, scales = "free_y") +
  ggplot2::labs(
    title    = "Distribution of ADF p-values by Variable",
    subtitle = "Dashed line = 0.05 threshold",
    x        = "ADF p-value",
    y        = "Count"
  ) +
  ggplot2::theme_minimal()

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_reports, "unit_root_adf_pvalues.png"),
    plot     = adf_plot,
    width    = 12,
    height   = 8,
    dpi      = 150,
    device   = "cairo_png"
  ),
  error = function(e) message("unit_root_adf_pvalues.png save failed: ", e$message)
)

readr::write_csv(adf_results,    file.path(path_reports, "unit_root_adf_results.csv"))
readr::write_csv(kpss_results,   file.path(path_reports, "unit_root_kpss_results.csv"))
readr::write_csv(adf_summary,    file.path(path_reports, "unit_root_adf_summary.csv"))
readr::write_csv(kpss_summary,   file.path(path_reports, "unit_root_kpss_summary.csv"))
readr::write_csv(ips_results,    file.path(path_reports, "unit_root_ips_results.csv"))
saveRDS(unitroot_results,        file.path(path_data,    "unitroot_results.rds"))

message("Script 12 complete.")
message("ADF summary:")
print(adf_summary)
message("KPSS summary:")
print(kpss_summary)
