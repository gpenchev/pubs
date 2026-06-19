# Scripts

Analysis pipeline for the paper **"Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023"**. Downloads, processes, and analyses panel data from WDI, IMF WEO, Eurostat, UCDP GED, and ParlGov to produce all regression results, diagnostics, and Shiny app data.

**31 scripts** across 6 subdirectories. Run the full pipeline with:

```sh
Rscript -e "source(here::here('scripts', 'run_pipeline.R'))"
```

---

## Prerequisites — read before running

**UCDP GED 26.1 requires a manual download.** The file is too large for automated download. Before running the pipeline:

1. Go to <https://ucdp.uu.se/downloads/>
2. Download `ged261-csv.zip` (UCDP GED Global version 26.1)
3. Unzip and place the CSV at `scripts/output/data/ucdp_ged_raw.csv`

Script `06_download_ucdp.R` will stop with a clear error if the file is missing.

All other data sources (WDI, IMF WEO, Eurostat, ParlGov) download automatically. An internet connection is required for the download steps.

---

## Pipeline execution order

The table below shows every script in the order `run_pipeline.R` sources it, the step label printed to the console, and the primary output.

| Step | Script | Console label | Primary output |
|------|--------|---------------|----------------|
| 01 | `eurostat/01_download_defence.R` | Download WDI defence | `defence_raw.rds` |
| 02 | `eurostat/02_download_fiscal.R` | Download IMF WEO fiscal | `debt_raw.rds`, `deficit_raw.rds` |
| 03 | `eurostat/03_download_gdp.R` | Download Eurostat GDP | `gdp_pc_raw.rds`, `gdp_growth_raw.rds` |
| 04 | `eurostat/04_download_migration.R` | Download Eurostat migration | `migration_raw.rds` |
| 05 | `eurostat/05_merge_eurostat.R` | Merge panel | `panel_eurostat.rds` |
| 06 | `ucdp/06_download_ucdp.R` | Download UCDP GED | `ucdp_ged_europe.rds` |
| 07 | `ucdp/07_process_ucdp.R` | Process UCDP threat scores | `threat_scores.rds`, `ucdp_map_events.rds` |
| 08 | `ucdp/08_merge_threat.R` | Merge threat into panel | `panel_full.rds` (v1) |
| 09 | `quality/09_coverage_check.R` | Coverage check | `missingness_by_country.csv` |
| 10 | `quality/10_balance_check.R` | Balance check | `balance_by_country.csv` |
| 11 | `quality/11_outlier_check.R` | Outlier check | `outlier_flags_iqr.csv` |
| 12 | `quality/13_summary_report.R` | Summary report | `data_quality_report.html` |
| 13 | `parlgov/01_download_parlgov.R` | Download ParlGov | `view_cabinet_raw.rds` etc. |
| 14 | `parlgov/02_process_parlgov.R` | Process ParlGov | `parlgov_country_year.rds` |
| 15 | `parlgov/03_merge_parlgov.R` | Merge ParlGov into panel | `panel_full.rds` (v2, adds political vars) |
| 16 | `quality/14_parlgov_quality_check.R` | ParlGov quality check | `parlgov_quality_report.csv` |
| 17 | `quality/12_unit_root_check.R` | Unit root check (full panel) | `unitroot_results.rds` |
| 18 | `regression/01_spatial_weights.R` | Spatial weights | `spatial_weights.rds` |
| 19 | `regression/02_baseline_ols.R` | Baseline OLS | `baseline_ols_results.rds` |
| 20 | `regression/03_spatial_tests.R` | Spatial tests | `spatial_test_results.rds` |
| 21 | `regression/04_spatial_panel.R` | Spatial panel | `spatial_panel_results.rds` |
| 22 | `regression/05_results_table.R` | Results table | `regression_tables.rds` |
| 23 | `regression/07_diagnostics.R` | Diagnostics | `diagnostics_results.rds` |
| 24 | `regression/08_structural_breaks.R` | Structural breaks | `structural_break_results.rds` |
| 25 | `regression/09_revision_checks.R` | Revision checks | `revision_checks_results.rds` |
| 26 | `regression/10_gpr_comparison.R` | GPR comparison + M13 | `gpr_comparison_results.rds` |
| 27 | `regression/06_publication_table.R` | Publication table | `regression_table.html/.tex` |
| 28 | `regression/11_app_data.R` | App data (Shiny) | 23 CSVs in `output/app/` |

> **Note on numbering:** Script filenames use their original sequence numbers (01–11 within each subdirectory). Execution order differs from filename order in two cases: `06_publication_table.R` runs as step 27 (after all diagnostics and robustness checks are complete); `12_unit_root_check.R` runs as step 17 (after ParlGov merge, so all variables including political ones are present).

All output files are written to `scripts/output/data/` (RDS and CSV) or `scripts/output/quality_reports/` (quality reports) unless noted otherwise.

---

## Setup

### 00_setup.R

Loads all required R packages (auto-installs if missing), defines file paths (`path_root`, `path_data`, `path_reports`, `path_parlgov`), and sets global constants:

- `nato_eu_core` — 24 countries: all NATO-EU members plus Norway (NATO non-EU) and Great Britain. Excludes AT, CY, IE, MT (EU non-NATO neutrals) and SE (NATO from March 2024, outside study period).
- `nato_eu_robustness` — identical to `nato_eu_core` as of this revision.
- `regression_countries` — `nato_eu_core` minus LU (Luxembourg, structural outlier).
- `year_start = 1995`, `year_end = 2023`.
- `regimes` — list of four regime boundary pairs (1995–2004, 2005–2013, 2014–2021, 2022–2023). Single source of truth for regime definitions — update here if boundaries change, then manually update `vis/helpers/helper_regime.R` which cannot source this file directly.

Sourced at the top of every pipeline script. Must remain free of side effects beyond package loading, directory creation, and constant definition.

### run_pipeline.R

Master pipeline runner. Sources all 28 scripts in dependency order (see table above). Each step is wrapped in `run_step()` which prints a separator and step label to the console. If any step fails, the pipeline stops immediately and reports which step failed.

### helpers/spatial_helpers.R

Shared spatial and regression helper functions sourced by all regression scripts. Functions:

**Spatial weight construction:**
- `build_block_w()` — queen contiguity block-diagonal W for panel data
- `build_block_w_invdist()` — inverse distance block-diagonal W
- `subset_listw()` — subset a listw object to a country subset
- `subset_distband_listw()` — subset a distance-band matrix to a listw

**Model fitting:**
- `run_sar_pooled()` — SAR with two-way FE via block-diagonal W
- `run_sem_pooled()` — SEM with two-way FE via block-diagonal W

**Coefficient extraction:**
- `extract_spatialreg()` — tidy coefficients from a `spatialreg` model
- `extract_spatial_param()` — extract ρ or λ from a `spatialreg` model
- `extract_plm()` — tidy coefficients from a `plm` model
- `extract_coef()` — scalar coefficient from a `spatialreg` model
- `extract_se()` — scalar SE from a `spatialreg` model

**Hypothesis tests:**
- `chow_test()` — Chow structural break test at a specified break year

---

## Eurostat and WDI Downloads

### eurostat/01_download_defence.R

Downloads military expenditure as a percentage of GDP from World Bank WDI (indicator `MS.MIL.XPND.GD.ZS`, SIPRI-sourced) for all 24 countries, 1995–2023. Includes exponential backoff retry logic (up to 5 attempts) and a local cache fallback so re-runs do not repeat successful downloads. Eurostat COFOG GF02 was evaluated and rejected: it is missing FR, DE, and GB entirely, and NO values are systematically underreported.

Output: `scripts/output/data/defence_raw.rds`

### eurostat/02_download_fiscal.R

Downloads general government gross debt (`GGXWDG_NGDP`) and net lending/borrowing (`GGXCNL_NGDP`) from the IMF World Economic Outlook API for all 24 countries, 1995–2023. Sign convention: negative `deficit_gdp` = deficit (consistent with Eurostat convention). Eurostat `gov_10dd_edpt1` was evaluated and rejected: zero rows for NO and GB. IMF WEO provides single-source coverage for all 24 countries.

Output: `scripts/output/data/debt_raw.rds`, `deficit_raw.rds`

### eurostat/03_download_gdp.R

Downloads GDP per capita (`CP_EUR_HAB`, current EUR) and real GDP growth (`CLV_PCH_PRE`, % change on previous year) from Eurostat for all EU countries. Supplements with WDI data for Great Britain only (Eurostat has no GB data post-Brexit), converting from USD to EUR using the ECB annual average EUR/USD exchange rate (`ert_bil_eur_a`) to maintain currency consistency.

Output: `scripts/output/data/gdp_pc_raw.rds`, `gdp_growth_raw.rds`

### eurostat/04_download_migration.R

Downloads annual immigration by citizenship (`migr_imm1ctz`) from Eurostat, filtered to `COMPLET` age definition, total sex, total citizenship. Normalises by 1 January population (`demo_pjan`) to produce `immigration_rate` per 1,000 population. **Great Britain has no Eurostat immigration data; `immigration_rate` is NA for all GB years by design, not a data gap.** Data available from 2000 onwards for most countries — the regression sample therefore starts in 1998–2000 even though the panel skeleton starts in 1995 (rows with NA regressors are dropped by `plm`/`lm`).

Output: `scripts/output/data/migration_raw.rds`

### eurostat/05_merge_eurostat.R

Merges all downloaded sources into a single country-year panel skeleton covering all 24 countries and all years 1995–2023. Joins on ISO2 country code + year. Adds source tracking columns (`defence_source`, `fiscal_source`, `gdp_source`) and a `regime` factor (1–4) derived from the `regimes` constant in `00_setup.R`. Performs row-count validation to confirm no observations are lost in the join.

Output: `scripts/output/data/panel_eurostat.rds`

---

## UCDP Conflict Data

### ucdp/06_download_ucdp.R

Validates and loads the UCDP GED **26.1** CSV (manual download required — see Prerequisites). Checks required columns (`year`, `type_of_violence`, `latitude`, `longitude`, `best`) are present. Filters to the European theatre bounding box (30°N–72°N, 25°W–45°E) and the study period 1995–2023. Of 417,968 global events, 118,234 state-based events (type_of_violence = 1) pass the bounding box filter.

Output: `scripts/output/data/ucdp_ged_europe.rds`

### ucdp/07_process_ucdp.R

Computes country-year threat scores from UCDP GED events. Two scores per country-year:

- `threat_score` / `threat_score_log` — all state-based events within 500 km (robustness)
- `threat_land` / `threat_land_log` — land-contiguous events only (primary)

**Land-contiguity filter:** an event passes if the straight-line path from the event location to the nearest point on the EU external land border crosses no more than 50 km of open sea. Accommodates narrow straits (~8 km) while excluding Mediterranean conflicts (~150 km minimum sea crossing).

**Threat formula:** `threat(c,t) = Σ_e [ log(fatalities_e + 1) × exp(−d(c,e) / 500) ]` where distances are in km to the nearest point on the country border polygon, computed in ETRS89-LAEA (EPSG:3035).

Uses `parallel::mclapply` (fork-based, Linux/macOS only) for performance. Cannot use `furrr`/`future` because `sf` external pointers cannot be serialised across workers.

Output: `scripts/output/data/threat_scores.rds`, `ucdp_map_events.rds`

### ucdp/08_merge_threat.R

Left-joins threat scores into the main panel on `country` + `year`. Asserts `nrow(panel_full) == nrow(panel)` — merge must not gain or lose rows. Validates that `threat_land_log` and `threat_score_log` are within expected ranges.

Output: `scripts/output/data/panel_full.rds` (updated, v1 — threat added, political variables not yet present)

---

## ParlGov Political Variables

### parlgov/01_download_parlgov.R

Downloads the ParlGov development dataset from `parlgov.org` as a ZIP archive via `httr::GET`. Extracts and saves three raw tables: cabinet, election, and party views.

Output: `scripts/output/data/parlgov/view_cabinet_raw.rds`, `view_election_raw.rds`, `view_party_raw.rds`

### parlgov/02_process_parlgov.R

Processes ParlGov raw tables into country-year political variables. Key steps:

- **Seat-weighting:** `gov_left_right` and `gov_eu_position` are the seat-weighted mean scores of all parties in the governing cabinet. Weights = share of parliamentary seats per party within the coalition. Reflects coalition balance, not just the lead party.
- **Cabinet spell expansion:** each cabinet spell (start date → end date) is expanded to annual observations by carry-forward. The coalition active on 1 January of a given year determines that year's ideology scores.
- **Imputation:** missing party ideology scores are imputed from the mean of parties in the same ParlGov family group. Imputed observations are flagged in `any_lr_imputed` and `any_eu_imputed`.
- **NaN handling:** `NaN` arises from 0/0 when a cabinet has zero total seats after filtering. These are set to `NA` and documented in `14_parlgov_quality_check.R`.

Output: `scripts/output/data/parlgov/parlgov_country_year.rds`, `parlgov_country_year.csv`

### parlgov/03_merge_parlgov.R

Left-joins ParlGov political variables (`gov_left_right`, `gov_eu_position`, `election_year`, `any_lr_imputed`, `any_eu_imputed`) into `panel_full.rds`. Checks for duplicate `country`+`year` keys in the ParlGov table before joining. Asserts row count is preserved.

Output: `scripts/output/data/panel_full.rds` (updated, v2 — political variables added)

---

## Quality Checks

Quality checks run in two batches. Checks 09–11 and 13 run on the pre-ParlGov panel (v1). Check 14 and 12 run after ParlGov merge (v2) so political variables are present. The unit root check (12) runs last in this batch because it tests all variables including `gov_left_right` and `gov_eu_position`.

### quality/09_coverage_check.R

Missingness analysis by country and variable. Produces a heatmap of missing data and flags countries with more than 30% missing `defence_gdp`. Runs on panel v1 (pre-ParlGov).

Output: `scripts/output/quality_reports/missingness_by_country.csv`, `missingness_heatmap.png`

### quality/10_balance_check.R

Panel balance diagnostic. Checks whether every country is observed in every year (1995–2023) and produces observation count plots by country and by year. Runs on panel v1.

Output: `scripts/output/quality_reports/balance_by_country.csv`, `balance_by_year.png`

### quality/11_outlier_check.R

IQR-based and year-on-year jump outlier detection for all numeric variables. Produces time series plots for visual inspection. Sources `vis/helpers/helper_outliers.R` for the outlier detection helper functions. Runs on panel v1.

Output: `scripts/output/quality_reports/outlier_flags_iqr.csv`, `outlier_flags_yoy.csv`

### quality/12_unit_root_check.R

Panel unit root and stationarity tests on the **full panel v2** (post-ParlGov). Runs ADF and KPSS tests per country per variable, plus the Im-Pesaran-Shin (IPS) panel unit root test per variable. **Runs as pipeline step 17, after ParlGov merge** (step 15) and ParlGov quality check (step 16), so that `gov_left_right` and `gov_eu_position` are present.

Output: `scripts/output/data/unitroot_results.rds`, `scripts/output/quality_reports/unit_root_adf_summary.csv`

### quality/13_summary_report.R

Compiles all quality check outputs (coverage, balance, outliers) into a descriptive statistics table and an HTML report. Falls back to CSV if pandoc is not installed. Runs on panel v1.

Output: `scripts/output/quality_reports/descriptive_stats.csv`, `data_quality_report.html`

### quality/14_parlgov_quality_check.R

Quality check for political variables added by `03_merge_parlgov.R`. Checks: (1) variable presence, (2) NaN detection in `gov_left_right` / `gov_eu_position`, (3) coverage by country (% of years non-NA), (4) coverage by year, (5) range validation (ParlGov scale 0–10 for both variables). Runs on panel v2.

Output: `scripts/output/quality_reports/parlgov_quality_report.csv`

---

## Regression

### regression/01_spatial_weights.R

Builds three spatial weight matrices for the **W-matrix sample (23 countries)** — `nato_eu_core` minus Norway. Norway is excluded because it has insufficient Eurostat fiscal coverage for the regression controls; Luxembourg is retained in the W-matrix sample but excluded from regressions later. Finland has no land border with any other sample country and receives distance-based fallback neighbours within 2,000 km (sensitivity tested in M10a). All distances computed in ETRS89-LAEA (EPSG:3035).

Matrices produced:
- `W_queen` — queen contiguity with 2,000 km distance fallback for isolated nodes
- `W_inv_dist` — inverse distance, capped at 2,000 km
- `W_dist_band` — binary distance band, 1,000 km threshold

Output: `scripts/output/data/spatial_weights.rds`

### regression/02_baseline_ols.R

Estimates four baseline panel models (no spatial component):

| Model | Type | Fixed effects |
|-------|------|---------------|
| M1 | Pooled OLS | None |
| M2 | Country FE | Country |
| M3 | Two-way FE | Country + year — primary non-spatial baseline |
| M4 | Two-way FE + regime interactions | Country + year |

**Regression sample:** 22 countries (`regression_countries` = `nato_eu_core` minus LU). Norway is not in `nato_eu_core`. GB is included in the sample object but dropped by `plm`/`lm` from any model including `immigration_rate` (NA for all GB years). Effective N: **529 observations, 22 countries, 1998–2023**. Standard errors: Driscoll-Kraay HC3.

Output: `scripts/output/data/baseline_ols_results.rds`

### regression/03_spatial_tests.R

Tests for spatial autocorrelation in M3 (two-way FE) residuals — not pooled OLS residuals, which would conflate panel heterogeneity with spatial signal. Two tests:

1. **Moran's I** on FE residuals under queen W and distance-band W. Result: 0 of 26 years significant — no residual spatial autocorrelation after FE.
2. **Robust Score (RS) tests** (Anselin et al. 1996) for SAR vs SEM selection. Result: SAR preferred in 57.7% of years.

Output: `scripts/output/data/spatial_test_results.rds`

### regression/04_spatial_panel.R

Estimates nine spatial panel models using block-diagonal weight matrices (two-way FE implemented via country and year dummy variables):

| Model | Specification | Key feature |
|-------|--------------|-------------|
| M5 | SAR queen W | **Primary spatial model** |
| M6 | SEM queen W | Compared to M5 via LR test (SAR preferred, p = 0.062) |
| M7 | SAR + regime × threat | Core finding: fiscal austerity broke rational response |
| M8 | SAR, `threat_score_log` | Robustness: all-events threat (no land filter) |
| M9 | SAR, inverse-distance W | Robustness: alternative weight matrix |
| M10a | SAR, Finland excluded | Robustness: sensitivity to distance fallback |
| M10b | SAR, post-2014 subsample | Threat non-significant: year FE absorbs 2022 common shock |
| M10c | SAR, pre-2014 subsample | Spatial lag not significant pre-2014 |
| M12 | SAR + lagged DV | Persistence vs diffusion test: ρ collapses to 0.061 |

All models use `zero.policy = TRUE` (isolated nodes in block-diagonal W get spatial lag = 0).

Output: `scripts/output/data/spatial_panel_results.rds`

### regression/05_results_table.R

Extracts and compiles results from all models (M1–M12 plus M13 when available) into three formats:

- `regression_results_long.csv` — one row per model × term (includes FE dummies)
- `regression_results_core.csv` — as above, FE dummies excluded
- `regression_results_wide.csv` — terms as rows, models as columns
- `lr_test_sar_sem.csv` — LR test SAR vs SEM (stat = 3.49, p = 0.062)

Output: `scripts/output/data/regression_tables.rds`, `regression_results_core.csv`, `regression_results_wide.csv`

### regression/06_publication_table.R

Produces publication-ready outputs. **Runs as pipeline step 27** (after all diagnostics and robustness checks), so tables reflect the final validated models:

- HTML and LaTeX regression tables (requires `\\usepackage{booktabs}` in LaTeX preamble)
- Coefficient forest plot across all specifications (`coef_plot.png`)
- Marginal effects plot for regime interaction model M7 (`marginal_effects_regime4.png`)
- Model fit statistics table (`fit_summary.html`)

Output: `scripts/output/tables/regression_table.html`, `regression_table.tex`, `coef_plot.png`, `marginal_effects_regime4.png`

### regression/07_diagnostics.R

Regression diagnostics on M3 (two-way FE OLS):

- **VIF:** `threat_land_log` = 13.2, `debt_gdp` = 10.9 — severe flags, but benign: source is shared temporal structure with year FE, not structural collinearity (bivariate r = 0.017). Confirmed by Check C in `09_revision_checks.R`.
- **Breusch-Pagan:** stat = 143.5, p < 0.001 — heteroskedasticity confirmed (expected; addressed by DK-HC3 and ML standard errors).
- **Chow tests:** breaks confirmed at 2003 (F = 2.34, p = 0.014) and 2022 (F = 2.63, p = 0.006); 2014 marginal (F = 1.68, p = 0.090).
- **Cook's distance:** 33 observations flagged (> 4/N threshold). Highest: Bulgaria 2019 (D = 0.099) — F-16 procurement contract recorded as a single-year budget item. Sensitivity checked in `09_revision_checks.R` Check H.

Output: `scripts/output/data/diagnostics_results.rds`, `vif_results.csv`, `heteroskedasticity_test.csv`, `structural_break_tests.csv`, `influence_diagnostics.csv`, `influence_by_country.csv`

### regression/08_structural_breaks.R

Structural break analysis and regime validation. Seven blocks:

1. **BIC-optimal breakpoint via `strucchange`** — supplementary only (applied to stacked panel, not true time series)
2. **Bai-Perron supF test** — formal break detection
3. **Regime specification comparison** (AIC/BIC/LR) — four-regime (M7) preferred: AIC 656.4 vs no-regime 662.4, LR p = 0.006
4. **SAR with data-driven break** — cross-validation of theoretically chosen dates
5. **Pre/post-2014 spatial lag asymmetry** — ρ pre-2014 = 0.019 (p = 0.696), post-2014 = 0.082 (p = 0.194), full sample = 0.177 (p < 0.001): full-sample spatial interdependence driven by post-2022 common surge
6. **`gov_eu_position` reversal validation** — pre-2014: β = +0.024 (p = 0.053); post-2014: β = −0.052 (p < 0.001); z-test: p = 0.008
7. **Persistence vs diffusion decomposition (FD-SAR)** — FD-SAR ρ = −0.091 (p = 0.032): short-run burden-sharing substitution; levels SAR ρ = +0.177: long-run complementarity

Output: `scripts/output/data/structural_break_results.rds`, `regime_comparison_aic.csv`, `regime_sar_lr_test.csv`, `spatial_lag_asymmetry_test.csv`, `gov_eu_position_subperiod.csv`, `bai_perron_tests.csv`

### regression/09_revision_checks.R

Ten targeted robustness and revision checks. Each block prints a `STABLE` / `CONFIRMED` / `INCONCLUSIVE` verdict to the console.

| Check | Tests | Key result |
|-------|-------|------------|
| **A** | Persistence vs diffusion summary | ρ: M5 = +0.177 → M12 = +0.061 → FD-SAR = −0.091 |
| **B** | Regime 4 power analysis | N = 44, power = 28% — confirmed underpowered |
| **C** | Orthogonalisation (VIF concern) | 0% coefficient change after orthogonalising threat on debt — VIF benign |
| **D** | Threat score correlation matrix | r(threat_land_log, debt_gdp) = 0.017 — no structural collinearity |
| **E** | Defence source consistency | Retained for future multi-source designs |
| **F** | SAR without immigration_rate | GB enters sample; ideology coefficients reverse sign — immigration belongs in model |
| **G** | GB structural outlier documentation | Threat 58% below mean, spending 53% above — structural misfit confirmed |
| **H** | Bulgaria 2019 sensitivity | Removing Cook's D = 0.099 obs: all key coefficients < 1 SE change — **STABLE** |
| **I** | Cross-sectional OLS 2022/2023 | β = +0.381 (p = 0.001, R² = 0.56) — threat-defence gradient real; M10b year FE absorption explained |
| **J** | Immigration × post-2022 interaction | No distinct refugee-driven mechanism (p = 0.058) |

Output: `scripts/output/data/revision_checks_results.rds`, `revision_checks_summary.csv`, `bg2019_sensitivity.csv`, `gb_structural_outlier_summary.csv`

### regression/10_gpr_comparison.R

Compares `threat_land_log` against the Caldara-Iacoviello Geopolitical Risk index (GPR) and estimates M13. Four blocks:

1. **Download and parse** GPR data (`GPRC_` country columns)
2. **Annualise and merge** with the panel for the 13 countries with GPR coverage (BE, DE, DK, ES, FI, FR, GB, HU, IT, NL, NO, PL, PT). The 10 absent countries (BG, CZ, EE, GR, HR, LT, LV, RO, SI, SK) are the highest-threat Eastern European states — their absence biases GPR comparisons against UCDP.
3. **Correlation analysis** — pooled r = 0.082 (not significant); Nordic/Eastern countries r > 0.5; Western countries (FR, GB, PT, ES) r ≈ 0 (global media coverage inflates GPR for non-exposed countries). Crimea 2014: GPR spikes, UCDP flat (kinetic bias documented).
4. **M13 estimation** — SAR with `gpr` replacing `threat_land_log` on the 13-country subsample. Key result: UCDP outperforms GPR by **ΔAIC = 17.6**; spatial lag reverses sign (UCDP ρ = +0.177, GPR ρ = −0.210) — physical threat generates complementarity; perceived risk generates free-riding substitution.

Output: `scripts/output/data/gpr_comparison_results.rds`, `scripts/output/data/gpr_correlation_summary.csv`

### regression/11_app_data.R

Prepares all Shiny app data from pipeline outputs. Reads from `scripts/output/data/` and `scripts/output/quality_reports/`; writes **23 flat CSV files** to `scripts/output/app/`. The app folder is fully autonomous — the Shiny app reads only CSVs and `*.md` files; no R model objects or pipeline packages are required at app runtime.

Seven blocks:

| Block | Content | Key output files |
|-------|---------|-----------------|
| 1 | Core panel + conflict events | `app_threat_panel.csv`, `app_conflict_events.csv` |
| 2 | Threat country summaries + GPR time series | `app_threat_country.csv`, `app_issue1_crimea.csv` |
| 3 | Unified coefficient table M1–M13 | `app_coef_long.csv` |
| 4 | Model fit, ρ comparison, country FE, regime effects | `app_model_fit.csv`, `app_rho_comparison.csv`, `app_regime_effects.csv` |
| 5 | Robustness check tables (Checks A–J) | `app_checks_summary.csv`, `app_check_h.csv`, `app_check_i.csv`, `app_check_j.csv` |
| 6 | Specific issues (kinetic bias, Greece, GPR coverage) | `app_issue2_greece.csv`, `app_issue3_coverage.csv`, `app_issue4_gpr_coverage.csv` |
| 7 | Markdown path lookup for dynamic content tabs | `app_md_paths.csv` |

Output: 23 CSV files in `scripts/output/app/`
