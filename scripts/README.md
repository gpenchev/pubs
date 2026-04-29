---

editor_options: 
  markdown: 
    wrap: 72
---

# Scripts

Full pipeline for downloading, processing, and analysing NATO-EU defence spending data, 1995-2023.

Run the full pipeline with:

``` sh
Rscript -e "source(here::here('scripts', 'run_pipeline.R'))"
```

------------------------------------------------------------------------

## Setup

### 00_setup.R

Loads all required R packages, defines file paths, and sets global constants including the country list (nato_eu_core, regression_countries), temporal coverage (1995-2023), and regime boundaries. Sourced at the top of every pipeline script.

### run_pipeline.R

Master pipeline runner. Sources all scripts in dependency order from downloads through to publication outputs. If any step fails the pipeline stops and reports the step label.

### helpers/spatial_helpers.R

Shared spatial and regression helper functions used across all regression scripts. Contains block-diagonal W matrix construction (build_block_w, build_block_w_invdist), spatial model fitting (run_sar_pooled, run_sem_pooled), coefficient extraction (extract_spatialreg, extract_plm, extract_coef, extract_se), and the Chow structural break test.

------------------------------------------------------------------------

## Eurostat and WDI Downloads

### eurostat/01_download_defence.R

Downloads military expenditure as a percentage of GDP from World Bank WDI (indicator MS.MIL.XPND.GD.ZS, SIPRI-sourced) for all 24 sample countries, 1995-2023. Includes exponential backoff retry logic and cache fallback. Output: scripts/output/data/defence_raw.rds

### eurostat/02_download_fiscal.R

Downloads general government gross debt (GGXWDG_NGDP) and net lending/borrowing (GGXCNL_NGDP) from the IMF World Economic Outlook API for all 24 countries, 1995-2023. Negative deficit_gdp values indicate a deficit. Output: scripts/output/data/debt_raw.rds, deficit_raw.rds

### eurostat/03_download_gdp.R

Downloads GDP per capita (CP_EUR_HAB) and real GDP growth (CLV_PCH_PRE) from Eurostat for EU countries. Supplements with WDI data for Great Britain, converting from USD to EUR using the ECB annual average exchange rate from Eurostat ert_bil_eur_a. Output: scripts/output/data/gdp_pc_raw.rds, gdp_growth_raw.rds

### eurostat/04_download_migration.R

Downloads annual immigration by citizenship (migr_imm1ctz) from Eurostat, filtered to COMPLET age definition, total sex, total citizenship. Normalises by 1 January population (demo_pjan) to produce immigration_rate per 1000. Great Britain has no Eurostat immigration data; immigration_rate is NA for all GB years by design. Output: scripts/output/data/migration_raw.rds

### eurostat/05_merge_eurostat.R

Merges all downloaded sources into a single country-year panel skeleton covering all 24 countries and all years 1995-2023. Adds source tracking columns (defence_source, fiscal_source, gdp_source) and regime indicator. Output: scripts/output/data/panel_eurostat.rds

------------------------------------------------------------------------

## UCDP Conflict Data

### ucdp/06_download_ucdp.R

Validates and loads the UCDP GED 25.1 CSV file (manual download required from ucdp.uu.se/downloads). Filters to the European theatre bounding box (30N-72N, 25W-45E) and the study period 1995-2023. Output: scripts/output/data/ucdp_ged_europe.rds

### ucdp/07_process_ucdp.R

Computes country-year threat scores from UCDP GED events. Applies a land-contiguity filter (events within 50 km sea crossing of the EU land border), then computes the spatially-decayed fatality-weighted threat score using a 500 km bandwidth. Uses parallel::mclapply for performance on Linux. Output: scripts/output/data/threat_scores.rds, ucdp_map_events.rds

### ucdp/08_merge_threat.R

Merges threat scores into the main panel. Validates that no rows are lost and that threat_land_log and threat_score_log are within expected ranges. Output: scripts/output/data/panel_full.rds (updated)

------------------------------------------------------------------------

## ParlGov Political Variables

### parlgov/01_download_parlgov.R

Downloads the ParlGov development dataset (cabinet, election, and party tables) from parlgov.org. Saves raw tables as RDS files. Output: scripts/output/data/parlgov/view_cabinet_raw.rds, view_election_raw.rds, view_party_raw.rds

### parlgov/02_process_parlgov.R

Processes ParlGov raw tables into country-year political variables. Computes seat-weighted mean left-right and EU position scores per cabinet spell, carries forward cabinet spells to annual observations, and flags parliamentary election years. Missing party ideology scores are imputed from party family means. Output: scripts/output/data/parlgov/parlgov_country_year.rds

### parlgov/03_merge_parlgov.R

Merges ParlGov political variables (gov_left_right, gov_eu_position, election_year, any_lr_imputed, any_eu_imputed) into panel_full.rds. Output: scripts/output/data/panel_full.rds (updated)

------------------------------------------------------------------------

## Quality Checks

### quality/09_coverage_check.R

Missingness analysis by country and variable. Produces a heatmap of missing data and flags countries with more than 30% missing defence_gdp. Output: scripts/output/quality_reports/missingness_by_country.csv, missingness_heatmap.png

### quality/10_balance_check.R

Panel balance diagnostic. Checks whether every country is observed in every year and produces observation count plots by country and year. Output: scripts/output/quality_reports/balance_by_country.csv, balance_by_year.png

### quality/11_outlier_check.R

IQR-based and year-on-year jump outlier detection for all numeric variables. Produces time series plots for visual inspection of each variable. Output: scripts/output/quality_reports/outlier_flags_iqr.csv, outlier_flags_yoy.csv

### quality/12_unit_root_check.R

Panel unit root and stationarity tests. Runs ADF and KPSS tests per country per variable, and the Im-Pesaran-Shin panel unit root test per variable. Output: scripts/output/data/unitroot_results.rds, scripts/output/quality_reports/unit_root_adf_summary.csv

### quality/13_summary_report.R

Compiles all quality check outputs into a descriptive statistics table and an HTML report (if pandoc is available). Falls back to CSV output if pandoc is not installed. Output: scripts/output/quality_reports/descriptive_stats.csv, data_quality_report.html

### quality/14_parlgov_quality_check.R

Quality check for political variables added by 03_merge_parlgov.R. Checks variable presence, NaN detection, coverage by country and year, and range validation (ParlGov scale 0-10). Output: scripts/output/quality_reports/parlgov_quality_report.csv

------------------------------------------------------------------------

## Regression

### regression/01_spatial_weights.R

Builds three spatial weight matrices for the regression sample (23 countries, Luxembourg excluded): queen contiguity with 2000 km distance fallback for Finland, inverse distance capped at 2000 km, and binary distance band at 1000 km. All distances computed in ETRS89-LAEA projection (EPSG:3035). Output: scripts/output/data/spatial_weights.rds

### regression/02_baseline_ols.R

Estimates four baseline panel models without spatial components: pooled OLS (M1), country FE (M2), two-way FE (M3), and two-way FE with regime interactions (M4). Applies Hausman test and Driscoll-Kraay HC3 standard errors. Output: scripts/output/data/baseline_ols_results.rds

### regression/03_spatial_tests.R

Tests for spatial autocorrelation in two-way FE residuals using Moran's I (queen W and distance-band W) and Robust Score tests for SAR vs SEM model selection. Uses FE residuals rather than pooled OLS residuals to isolate the genuine spatial signal. Output: scripts/output/data/spatial_test_results.rds

### regression/04_spatial_panel.R

Estimates nine spatial panel models using block-diagonal weight matrices: SAR primary (M5), SEM (M6), SAR with regime interactions (M7), SAR robustness specifications (M8, M9, M10a, M10b, M10c), and SAR with lagged DV (M12). Includes LR test for SAR vs SEM preference. Output: scripts/output/data/spatial_panel_results.rds

### regression/05_results_table.R

Extracts and compiles regression results from all models into long, core, and wide format tables. Computes model fit statistics (log-likelihood, AIC) and Moran's I summary. Output: scripts/output/data/regression_tables.rds, regression_results_core.csv, regression_results_wide.csv

### regression/06_publication_table.R

Produces publication-ready outputs: HTML and LaTeX regression tables, coefficient forest plot across model specifications, and marginal effects plot for the regime interaction model (M7). Output: scripts/output/tables/regression_table.html, regression_table.tex, coef_plot.png, marginal_effects_regime4.png

### regression/07_diagnostics.R

Regression diagnostics: VIF on two-way FE OLS, Breusch-Pagan heteroskedasticity test, Chow structural break tests at 2014 and 2022, and Cook's distance influence diagnostics. Output: scripts/output/data/diagnostics_results.rds, vif_results.csv, influence_diagnostics.csv

### regression/08_structural_breaks.R

Structural break analysis and regime validation. Applies Bai-Perron supF test, compares regime specifications by AIC/BIC/LR, tests pre/post-2014 spatial lag asymmetry, validates the gov_eu_position sign reversal, and decomposes persistence vs diffusion using first-difference SAR. Output: scripts/output/data/structural_break_results.rds

### regression/09_revision_checks.R

Seven targeted robustness and revision checks: persistence vs diffusion summary (A), regime 4 power analysis (B), fiscal coefficient stability after orthogonalisation (C), threat score correlation matrix (D), defence source consistency (E), SAR without immigration_rate bringing GB into sample (F), and formal GB structural outlier documentation (G). Output: scripts/output/data/revision_checks_results.rds, revision_checks_summary.csv
