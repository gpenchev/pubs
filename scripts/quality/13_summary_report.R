# =============================================================================
# 13_summary_report.R
# Compile all quality check outputs into a single summary report
# Runs on v1 panel (before ParlGov merge).
# Political variables are excluded from descriptive statistics here;
# they are covered in 14_parlgov_quality_check.R.
# Output: scripts/output/quality_reports/data_quality_report.html
#         (falls back to CSV summary if pandoc is not available)
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")

# --- Load panel ---------------------------------------------------------------
panel <- readRDS(file.path(path_data, "panel_full.rds"))

# --- Load quality outputs -----------------------------------------------------
safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    message("File not found, skipping: ", path)
    NULL
  }
}

miss_country    <- safe_read_csv(file.path(path_reports, "missingness_by_country.csv"))
miss_year       <- safe_read_csv(file.path(path_reports, "missingness_by_year.csv"))
drop_candidates <- safe_read_csv(file.path(path_reports, "drop_candidates.csv"))
balance_country <- safe_read_csv(file.path(path_reports, "balance_by_country.csv"))
outlier_summary <- safe_read_csv(file.path(path_reports, "outlier_summary.csv"))
adf_summary     <- safe_read_csv(file.path(path_reports, "unit_root_adf_summary.csv"))
kpss_summary    <- safe_read_csv(file.path(path_reports, "unit_root_kpss_summary.csv"))

# --- Descriptive statistics ---------------------------------------------------
vars_numeric_candidate <- c(
  "defence_gdp",
  "debt_gdp",
  "deficit_gdp",
  "gdp_pc",
  "gdp_growth",
  "immigration_rate",
  "threat_score_log",
  "threat_score_z",
  "threat_land_log",
  "threat_land_z"
)

vars_numeric <- intersect(vars_numeric_candidate, names(panel))

desc_stats <- panel %>%
  dplyr::select(dplyr::all_of(vars_numeric)) %>%
  tidyr::pivot_longer(
    cols      = dplyr::everything(),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n       = sum(!is.na(value)),
    mean    = round(mean(value, na.rm = TRUE), 3),
    sd      = round(sd(value, na.rm = TRUE), 3),
    min     = round(min(value, na.rm = TRUE), 3),
    p25     = round(quantile(value, 0.25, na.rm = TRUE), 3),
    median  = round(median(value, na.rm = TRUE), 3),
    p75     = round(quantile(value, 0.75, na.rm = TRUE), 3),
    max     = round(max(value, na.rm = TRUE), 3),
    missing = sum(is.na(value)),
    .groups = "drop"
  )

# Always save descriptive stats as CSV regardless of pandoc availability
readr::write_csv(desc_stats,
                 file.path(path_reports, "descriptive_stats.csv"))
message("Descriptive statistics saved to descriptive_stats.csv")

# --- Check pandoc availability ------------------------------------------------
pandoc_available <- tryCatch({
  rmarkdown::pandoc_available()
}, error = function(e) FALSE)

if (!pandoc_available) {
  message("pandoc not available — skipping HTML report generation.")
  message("All quality outputs have been saved as CSV files in: ", path_reports)
  message("Script 13 complete (CSV only — no HTML report).")
} else {

  # --- Render report ----------------------------------------------------------
  report_path <- file.path(path_reports, "data_quality_report.Rmd")

  writeLines(
    con = report_path,
    text = c(
      "---",
      "title: 'Data Quality Report'",
      "date: '`r Sys.Date()`'",
      "output: html_document",
      "---",
      "",
      "## 1. Sample",
      paste0("- Countries (core): ", length(nato_eu_core)),
      paste0("- Countries (with robustness): ", length(nato_eu_robustness)),
      paste0("- Years: ", year_start, " to ", year_end),
      paste0("- Total rows in panel: ", nrow(panel)),
      paste0("- Note: political variables (gov_left_right, gov_eu_position, election_year)"),
      paste0("  are added after ParlGov merge and reported in parlgov_quality_report.csv"),
      "",
      "## 2. Descriptive Statistics",
      "```{r echo=FALSE}",
      "knitr::kable(desc_stats)",
      "```",
      "",
      "## 3. Missingness by Country",
      "```{r echo=FALSE}",
      "if (!is.null(miss_country)) knitr::kable(miss_country)",
      "```",
      "",
      "## 4. Countries Flagged for Exclusion (>30% missing on defence_gdp)",
      "```{r echo=FALSE}",
      "if (!is.null(drop_candidates)) knitr::kable(drop_candidates)",
      "```",
      "",
      "## 5. Panel Balance",
      "```{r echo=FALSE}",
      "if (!is.null(balance_country)) knitr::kable(balance_country)",
      "```",
      "",
      "## 6. Outlier Summary",
      "```{r echo=FALSE}",
      "if (!is.null(outlier_summary)) knitr::kable(outlier_summary)",
      "```",
      "",
      "## 7. Unit Root Tests",
      "### ADF Summary (% stationary)",
      "```{r echo=FALSE}",
      "if (!is.null(adf_summary)) knitr::kable(adf_summary)",
      "```",
      "",
      "### KPSS Summary (% non-stationary)",
      "```{r echo=FALSE}",
      "if (!is.null(kpss_summary)) knitr::kable(kpss_summary)",
      "```",
      "",
      "## 8. Threat Score Distribution",
      "```{r echo=FALSE}",
      "threat_plot <- file.path(path_reports, 'timeseries_threat_score_log.png')",
      "if (file.exists(threat_plot)) knitr::include_graphics(threat_plot)",
      "```",
      "",
      "## 9. Threat Land Score Distribution",
      "```{r echo=FALSE}",
      "land_plot <- file.path(path_reports, 'timeseries_threat_land_log.png')",
      "if (file.exists(land_plot)) knitr::include_graphics(land_plot)",
      "```",
      "",
      "## 10. Missingness Heatmap",
      "```{r echo=FALSE}",
      "heat_plot <- file.path(path_reports, 'missingness_heatmap.png')",
      "if (file.exists(heat_plot)) knitr::include_graphics(heat_plot)",
      "```",
      "",
      "## 11. Balance Over Time",
      "```{r echo=FALSE}",
      "bal_plot <- file.path(path_reports, 'balance_by_year.png')",
      "if (file.exists(bal_plot)) knitr::include_graphics(bal_plot)",
      "```"
    )
  )

  tryCatch(
    rmarkdown::render(
      input       = report_path,
      output_file = file.path(path_reports, "data_quality_report.html"),
      quiet       = TRUE
    ),
    error = function(e) {
      message("HTML report render failed: ", e$message)
      message("All quality outputs are available as CSV files in: ", path_reports)
    }
  )

  message("Script 13 complete: report saved to ",
          file.path(path_reports, "data_quality_report.html"))
}
