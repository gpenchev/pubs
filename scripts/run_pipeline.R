# =============================================================================
# run_pipeline.R
# Master pipeline runner — executes all scripts in dependency order
#
# Each step is sourced in the current R session. If a step fails, the
# pipeline stops and the error is reported with the step label.
#
# Dependency order:
#   1. Downloads (defence, fiscal, GDP, migration)
#   2. Panel merge
#   3. UCDP threat scores
#   4. Quality checks (pre-ParlGov)
#   5. ParlGov political variables
#   5b. ParlGov quality check
#   6. Regression (weights, baseline, spatial tests, spatial panel,
#      results table, diagnostics, structural breaks, revision checks)
#   7. Publication outputs
# =============================================================================

library(here)

run_step <- function(path, label) {
  message("\n", strrep("=", 60))
  message("RUNNING: ", label)
  message(strrep("=", 60))
  source(here::here(path))
}

# --- Step 1: Downloads --------------------------------------------------------
run_step("scripts/eurostat/01_download_defence.R",   "01 Download WDI defence (all 24 countries)")
run_step("scripts/eurostat/02_download_fiscal.R",    "02 Download IMF WEO fiscal (all 24 countries)")
run_step("scripts/eurostat/03_download_gdp.R",       "03 Download Eurostat GDP + WDI supplement GB")
run_step("scripts/eurostat/04_download_migration.R", "04 Download Eurostat migration")

# --- Step 2: Merge all sources into panel -------------------------------------
run_step("scripts/eurostat/05_merge_eurostat.R",     "05 Merge panel")

# --- Step 3: UCDP threat scores -----------------------------------------------
run_step("scripts/ucdp/06_download_ucdp.R",          "06 Download UCDP GED")
run_step("scripts/ucdp/07_process_ucdp.R",           "07 Process UCDP threat scores")
run_step("scripts/ucdp/08_merge_threat.R",           "08 Merge threat into panel")

# --- Step 4: Quality checks (pre-ParlGov panel) -------------------------------
run_step("scripts/quality/09_coverage_check.R",      "09 Coverage check")
run_step("scripts/quality/10_balance_check.R",       "10 Balance check")
run_step("scripts/quality/11_outlier_check.R",       "11 Outlier check")
run_step("scripts/quality/12_unit_root_check.R",     "12 Unit root check")
run_step("scripts/quality/13_summary_report.R",      "13 Summary report")

# --- Step 5: ParlGov political variables --------------------------------------
run_step("scripts/parlgov/01_download_parlgov.R",    "14 Download ParlGov")
run_step("scripts/parlgov/02_process_parlgov.R",     "15 Process ParlGov")
run_step("scripts/parlgov/03_merge_parlgov.R",       "16 Merge ParlGov into panel")

# --- Step 5b: Political variable quality check --------------------------------
run_step("scripts/quality/14_parlgov_quality_check.R", "17 ParlGov quality check")

# --- Step 6: Regression -------------------------------------------------------
run_step("scripts/regression/01_spatial_weights.R",  "18 Spatial weights")
run_step("scripts/regression/02_baseline_ols.R",     "19 Baseline OLS")
run_step("scripts/regression/03_spatial_tests.R",    "20 Spatial tests")
run_step("scripts/regression/04_spatial_panel.R",    "21 Spatial panel")
run_step("scripts/regression/05_results_table.R",    "22 Results table")
run_step("scripts/regression/07_diagnostics.R",      "23 Diagnostics")
run_step("scripts/regression/08_structural_breaks.R","24 Structural breaks")
run_step("scripts/regression/09_revision_checks.R",  "25 Revision checks")

# --- Step 7: Publication outputs ----------------------------------------------
run_step("scripts/regression/06_publication_table.R","26 Publication table")

message("\n", strrep("=", 60))
message("PIPELINE COMPLETE")
message(strrep("=", 60))
