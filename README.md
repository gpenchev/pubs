------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# EU Defence Spending Panel — Code Repository

**Interactive visualization of the data:** [pub.e-dnrs.org](https://pub.e-dnrs.org)

This repository contains the full replication pipeline, data download scripts, and interactive Shiny visualisation for the study of NATO-EU defence spending determinants, 1995-2023.

------------------------------------------------------------------------

## Research Question

What drives defence spending among NATO-EU member states in the post-Cold War period? This study constructs a continuous, country-specific, time-varying threat proximity measure from georeferenced conflict event data and estimates its effect on defence spending using spatial panel models, controlling for fiscal constraints and political orientation.

------------------------------------------------------------------------

## Repository Structure

```         
defence-spending-companion/
├── scripts/
│   ├── 00_setup.R
│   ├── run_pipeline.R
│   ├── helpers/
│   │   └── spatial_helpers.R
│   ├── eurostat/
│   ├── ucdp/
│   ├── parlgov/
│   ├── regression/
│   └── quality/
├── vis/
│   ├── app.R
│   ├── global.R
│   ├── helpers/
│   └── modules/
└── methodology/
    ├── methodology.md
    └── abbreviations.md
```

------------------------------------------------------------------------

## Data Sources

| Variable | Source | Coverage |
|------------------------|------------------------|------------------------|
| Defence spending (% GDP) | World Bank WDI / SIPRI | 24 countries, 1995-2023 |
| Government debt (% GDP) | IMF World Economic Outlook | 24 countries, 1995-2023 |
| Fiscal deficit (% GDP) | IMF World Economic Outlook | 24 countries, 1995-2023 |
| GDP per capita (EUR) | Eurostat / WDI supplement for GB | 24 countries, 1995-2023 |
| GDP growth (%) | Eurostat / WDI supplement for GB | 24 countries, 1995-2023 |
| Immigration rate (per 1000) | Eurostat migr_imm1ctz | 23 countries, 2000-2023 |
| Conflict events | UCDP GED 25.1 | European theatre, 1995-2023 |
| Government ideology | ParlGov development dataset | 24 countries, 1995-2023 |

------------------------------------------------------------------------

## Sample

- 24 NATO-EU member states plus Norway and Great Britain
- Study period: 1995-2023 (29 years)
- Primary regression sample: 530 observations, 22 countries
- Luxembourg excluded as structural outlier (defence/GDP below 0.2%)
- Great Britain excluded from primary models due to structural incompatibility with the land-border threat measure

------------------------------------------------------------------------

## Key Results

- Threat proximity is a significant positive predictor of defence spending across all specifications (coef = 0.090, SE = 0.023, p \< 0.001)
- Fiscal deficit is a robust negative predictor (coef = -0.023, p \< 0.001)
- Spatial lag rho = 0.177 (p \< 0.001) but disappears after controlling for persistence, suggesting spending inertia rather than genuine diffusion
- Structural breaks detected at 2003 and 2022
- Four-regime specification preferred on AIC/BIC

------------------------------------------------------------------------

## How to Run

### Step 1 — Install R packages

Run the following in an R session:

``` r
options(repos = c(CRAN = "https://cloud.r-project.org"))
source("scripts/00_setup.R")
```

### Step 2 — Download UCDP GED manually

Download GED 25.1 from [ucdp.uu.se/downloads](https://ucdp.uu.se/downloads/) and place the CSV file at:

``` diff
+ scripts/output/data/ucdp_ged_raw.csv
```

### Step 3 — Run the full pipeline

Execute from the repository root:

``` sh
Rscript -e "source(here::here('scripts', 'run_pipeline.R'))"
```

### Step 4 — Launch the Shiny app

Run the following command from the repository root:

``` sh
Rscript -e "shiny::runApp('vis')"
```

------------------------------------------------------------------------

## Documentation

- [Methodology](methodology/methodology.md) — threat measure construction, model specifications, structural break analysis
- [Script descriptions](scripts/README.md) — description of every script
- [Abbreviations](methodology/abbreviations.md) — all abbreviations used

------------------------------------------------------------------------

## License

MIT
