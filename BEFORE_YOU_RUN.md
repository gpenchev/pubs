# Before You Run the Pipeline

This document lists everything you need to download or prepare **manually**
before running `scripts/run_pipeline.R`. One item requires manual download;
everything else is fetched automatically by the pipeline scripts.

---

## 1. Manual download required — UCDP GED 26.1

**This is the only file the pipeline cannot download automatically.**

| Item | Detail |
|------|--------|
| Dataset | UCDP Georeferenced Event Dataset (GED) Global version 26.1 |
| Coverage | All conflict events worldwide, 1989–2025 |
| Format | ZIP containing a single CSV |
| Licence | Free for academic use, registration not required |

### Steps

1. Go to **https://ucdp.uu.se/downloads/**
2. Find **"UCDP GED — Georeferenced Event Dataset"**, version **26.1**
3. Download the file: `ged261-csv.zip`
4. Unzip it — you will get a file called `GEDEvent_v26_1.csv`
5. Rename it to `ucdp_ged_raw.csv`
6. Place it at exactly this path inside the project:

```
scripts/output/data/ucdp_ged_raw.csv
```

If the directory does not exist yet, create it:

```bash
mkdir -p scripts/output/data
```

**The pipeline stops immediately at step 06 if this file is missing.**

### Required columns

The CSV must contain these columns (all present in GED 26.1):

| Column | Description |
|--------|-------------|
| `year` | Event year |
| `type_of_violence` | 1 = state-based, 2 = non-state, 3 = one-sided |
| `latitude` | Event latitude (decimal degrees) |
| `longitude` | Event longitude (decimal degrees) |
| `best` | Best fatality estimate |

---

## 2. Automatic downloads (internet connection required)

These are fetched by pipeline scripts at runtime. No manual action needed,
but you need a working internet connection for all of them.

### Defence spending — World Bank WDI
- **Script:** `scripts/eurostat/01_download_defence.R`
- **Source:** World Bank API via R package `WDI`
- **Indicator:** `MS.MIL.XPND.GD.ZS` (Military expenditure % of GDP, SIPRI-sourced)
- **Fallback:** If the API is down, the script loads a cached `defence_raw.rds`
  if one exists from a previous successful run.

### Fiscal data (debt, deficit) — IMF World Economic Outlook
- **Script:** `scripts/eurostat/02_download_fiscal.R`
- **Source:** IMF WEO API via R package `imfapi`
- **Indicators:** `GGXWDG_NGDP` (gross debt % GDP), `GGXCNL_NGDP` (net lending % GDP)
- **Note:** No registration required.

### GDP per capita and GDP growth — Eurostat + WDI
- **Script:** `scripts/eurostat/03_download_gdp.R`
- **Primary source:** Eurostat API (`nama_10_pc`, `nama_10_gdp`) via R package `eurostat`
- **Supplement for GB:** World Bank WDI (Eurostat has no GB data post-Brexit)
- **Currency conversion for GB:** ECB annual EUR/USD rate from Eurostat `ert_bil_eur_a`

### Immigration data — Eurostat
- **Script:** `scripts/eurostat/04_download_migration.R`
- **Source:** Eurostat API (`migr_imm1ctz`) via R package `eurostat`
- **Coverage note:** Data starts from 2000 for most countries; 1995–1999 will
  be NA. GB has no Eurostat immigration data — GB rows are excluded from
  all models that include `immigration_rate`.

### ParlGov political variables
- **Script:** `scripts/parlgov/01_download_parlgov.R`
- **Source:** http://www.parlgov.org/data/parlgov-development_csv-utf-8.zip
- **Tables used:** `view_cabinet`, `view_election`, `view_party`
- **Note:** The "development" dataset is updated regularly; download at
  pipeline run time ensures the most current data.

### GPR index — Caldara-Iacoviello (M13 comparison only)
- **Script:** `scripts/regression/10_gpr_comparison.R` (step 27)
- **Source:** https://www.matteoiacoviello.com/gpr_files/data_gpr_export.dta
- **Format:** Stata `.dta` file, read directly via R package `haven`
- **Note:** This download runs near the end of the pipeline (after all
  regressions). The pipeline does **not** stop if this fails — M13 is an
  appendix robustness check, not a primary model.
- **Citation:** Caldara, D. & Iacoviello, M. (2022). Measuring Geopolitical
  Risk. *American Economic Review* 112(4): 1194–1225.

---

## 3. R packages

All required packages are installed automatically by `scripts/00_setup.R`
(sourced at the top of every script). The first run will install missing
packages from CRAN. Ensure you have write access to your R library.

Key packages: `eurostat`, `WDI`, `imfapi`, `sf`, `spdep`, `spatialreg`,
`splm`, `plm`, `strucchange`, `tseries`, `rnaturalearth`, `haven`,
`parallel`, `purrr`, `here`, `readr`, `dplyr`, `tidyr`, `ggplot2`.

---

## 4. System requirements

| Requirement | Detail |
|-------------|--------|
| OS | Linux or macOS (the UCDP threat computation uses `parallel::mclapply` with fork-based parallelism — does **not** work on Windows) |
| RAM | ≥ 8 GB recommended (spatial operations on GED load ~500k rows as sf objects) |
| Cores | The script auto-detects and uses all available cores minus one |
| Disk | ~200 MB for all downloaded and processed data |
| Internet | Required for steps 1–5 and 27; steps 6–26 run offline |

---

## 5. Pipeline run order summary

Once the UCDP CSV is in place, run the full pipeline with:

```r
source("scripts/run_pipeline.R")
```

Or run individual steps in this order if you want to proceed incrementally:

```
Step 01  01_download_defence.R    → WDI API (auto)
Step 02  02_download_fiscal.R     → IMF WEO API (auto)
Step 03  03_download_gdp.R        → Eurostat + WDI APIs (auto)
Step 04  04_download_migration.R  → Eurostat API (auto)
Step 05  05_merge_eurostat.R      → local merge (offline)
Step 06  06_download_ucdp.R       → reads ucdp_ged_raw.csv ← MANUAL FILE NEEDED
Step 07  07_process_ucdp.R        → ~20–40 min depending on cores
Step 08  08_merge_threat.R        → local merge (offline)
Steps 09–13  quality checks       → offline
Steps 14–17  ParlGov download + merge → ParlGov website (auto)
Step 18  12_unit_root_check.R     → offline (full panel required)
Steps 19–26  regression models    → offline
Step 27  10_gpr_comparison.R      → Iacoviello website (auto)
Step 28  06_publication_table.R   → offline
```

---

## 6. Quick checklist before first run

- [ ] `scripts/output/data/ucdp_ged_raw.csv` exists and is GED version 26.1
- [ ] Internet connection active
- [ ] Running on Linux or macOS (not Windows)
- [ ] R ≥ 4.2 installed
- [ ] At least 8 GB RAM available
- [ ] `here::here()` resolves to the project root (open the `.Rproj` file or
      set working directory to the project root before sourcing)
