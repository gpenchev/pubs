# Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023

Replication repository for the paper **"Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023"**.

**Interactive application:** [pub.e-dnrs.org/article1](https://pub.e-dnrs.org/article1/) &nbsp;·&nbsp; **Code:** [github.com/gpenchev/pubs](https://github.com/gpenchev/pubs)

---

## Overview

This repository contains the complete replication pipeline, analysis code, and interactive Shiny visualisation for a study of NATO-EU defence spending determinants over 1995–2023. The study constructs a novel georeferenced territorial threat proximity measure from UCDP conflict event data and estimates its effect on defence spending using spatial panel models.

**Core findings:**
- Threat proximity is a significant positive predictor of defence spending across all 11 of 13 specifications (β = +0.088, SE = 0.023, p < 0.001 in the primary spatial model)
- Fiscal deficit is a robust negative predictor (β = −0.023, p < 0.001 in every specification) — fiscal constraints override strategic imperatives
- The threat-defence relationship broke down during 2005–2013 (net elasticity = −0.156): austerity completely overrode rational threat response
- Spatial lag (ρ = +0.177) reflects spending inertia rather than genuine strategic diffusion — disappears after controlling for persistence
- Government EU integration position reversed sign after 2014 (z-test p = 0.008): Eurosceptic nationalist governments became the primary rearmament drivers

---

## Repository structure

```
pubs/
├── README.md                    ← this file
│
├── scripts/                     ← full analysis pipeline (31 scripts)
│   ├── 00_setup.R               ← packages, paths, global constants
│   ├── run_pipeline.R           ← master runner (sources all 28 steps)
│   ├── helpers/
│   │   └── spatial_helpers.R    ← shared spatial/regression functions
│   ├── eurostat/                ← steps 01–05: data downloads + panel merge
│   ├── ucdp/                    ← steps 06–08: threat score construction
│   ├── parlgov/                 ← steps 13–15: political variables
│   ├── quality/                 ← steps 09–12, 16–17: data quality checks
│   ├── regression/              ← steps 18–28: all models + app data
│   └── output/                  ← generated data (git-ignored; reproduced by pipeline)
│
├── vis/                         ← Shiny interactive application
│   ├── app.R                    ← main app layout (5 tabs)
│   ├── global.R                 ← package loading, data loading
│   ├── helpers/                 ← plot theme, map helpers, data loaders
│   └── modules/                 ← one module per tab/subtab
│
└── methodology/                 ← documentation
    ├── methodology.md           ← full methodology description
    └── abbreviations.md         ← variable and abbreviation definitions
```

---

## Data sources

| Variable | Source | Indicator / table | Coverage |
|---|---|---|---|
| Defence spending (% GDP) | World Bank WDI / SIPRI | `MS.MIL.XPND.GD.ZS` | 24 countries, 1995–2023 |
| Government gross debt (% GDP) | IMF World Economic Outlook | `GGXWDG_NGDP` | 24 countries, 1995–2023 |
| Fiscal deficit (% GDP) | IMF World Economic Outlook | `GGXCNL_NGDP` | 24 countries, 1995–2023 |
| GDP per capita (EUR) | Eurostat / WDI supplement for GB | `nama_10_pc` CP_EUR_HAB | 24 countries, 1995–2023 |
| GDP growth (%) | Eurostat / WDI supplement for GB | `nama_10_gdp` CLV_PCH_PRE | 24 countries, 1995–2023 |
| Immigration rate (per 1,000) | Eurostat | `migr_imm1ctz` | 23 countries, 2000–2023 |
| Conflict events | **UCDP GED 26.1** — manual download required | georeferenced fatalities | European theatre, 1995–2023 |
| Government ideology | ParlGov development dataset | cabinet + party views | 24 countries, 1995–2023 |
| Geopolitical Risk index | Caldara & Iacoviello (2022) | `GPRC_` country columns | 13 countries, 1995–2023 (M13 only) |

---

## Sample

- **Panel download:** 24 countries — all NATO-EU members plus Norway and Great Britain
- **Study period:** 1995–2023 (29 years)
- **Primary regression sample:** 529 observations, 22 countries, 1998–2023
  - Luxembourg excluded: structural outlier (defence/GDP < 0.2% throughout)
  - Great Britain excluded from primary models: island geography causes systematic underestimation of land-border threat; empirically, GB threat score is 58% below the sample mean while spending is 53% above — the opposite of the theory-predicted direction
  - Norway excluded from regressions: insufficient Eurostat fiscal coverage
- **Excluded by design:** Austria, Cyprus, Ireland, Malta (EU non-NATO neutrals); Sweden (NATO from March 2024, outside study period)

---

## How to run

### Step 1 — Install R packages

```r
source("scripts/00_setup.R")
```

Packages are installed automatically from CRAN on first run.

### Step 2 — Download UCDP GED 26.1 manually

This is the **only** file the pipeline cannot download automatically.

1. Go to <https://ucdp.uu.se/downloads/>
2. Download `ged261-csv.zip` (GED Global version 26.1)
3. Unzip and rename to `ucdp_ged_raw.csv`
4. Place at: `scripts/output/data/ucdp_ged_raw.csv`

The pipeline stops at step 06 with a clear error if this file is missing.

### Step 3 — Run the full pipeline

```sh
Rscript -e "source(here::here('scripts', 'run_pipeline.R'))"
```

All other data (WDI, IMF WEO, Eurostat, ParlGov, GPR) downloads automatically. The UCDP threat computation (step 07) takes 20–40 minutes depending on available cores.

### Step 4 — Launch the Shiny app

```sh
Rscript -e "shiny::runApp('vis')"
```

The app reads only from `scripts/output/app/` (23 flat CSVs generated by step 28). No model objects or pipeline packages are required at runtime.

---

## Shiny application

The interactive application has five main tabs:

| Tab | Content |
|-----|---------|
| **Threat Index** | Time series, UCDP vs GPR scatter, conflict event map, threat index validation results |
| **Panel Models** | Coefficient forest plot (M1–M13), regime effects chart (M7), spatial lag comparison |
| **Robustness** | Check I (2022 cross-section), Check H (Bulgaria 2019), VIF diagnostics, Cook's D outliers |
| **Specific Issues** | Pre-prepared answers to the four main methodological challenges: kinetic bias, Greece, immigration, GPR coverage |
| **About** | Paper abstract, methodology links, replication instructions |

---

## Documentation

| File | Contents |
|------|----------|
| [`scripts/README.md`](scripts/README.md) | Every script described: pipeline execution order table, inputs, outputs |
| [`methodology/methodology.md`](methodology/methodology.md) | Threat measure construction, model specifications, regime analysis, sensitivity checks |
| [`methodology/abbreviations.md`](methodology/abbreviations.md) | All variables, country codes, model names, spatial terms |

---

## Requirements

| Requirement | Detail |
|-------------|--------|
| OS | **Linux or macOS** — the UCDP threat computation uses `parallel::mclapply` (fork-based); does not work on Windows |
| R | ≥ 4.2 |
| RAM | ≥ 8 GB recommended (spatial operations on ~120,000 GED events as `sf` objects) |
| Disk | ~200 MB for all downloaded and processed data |
| Internet | Required for pipeline steps 01–05 (downloads) and step 26 (GPR); steps 06–25 and 27–28 run offline |

---

## Citation

> [Author]. (2025). *Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023*. [Conference / Journal — forthcoming].

---

## Acknowledgements

Research design, data, and all analytical decisions by the author. Scripts and interactive application developed with the assistance of **Claude Sonnet 4.5** accessed through **AiderDesk 0.70.0**.

---

## License

MIT
