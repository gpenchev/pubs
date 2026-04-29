# =============================================================================
# 00_setup.R
# Global setup: packages, paths, constants
#
# This file is sourced at the top of every pipeline script. It must remain
# free of side effects beyond package loading, directory creation, and
# constant definition.
# =============================================================================

# --- Packages -----------------------------------------------------------------
# All packages required by any script in the pipeline are loaded here so that
# missing-package errors surface immediately rather than mid-pipeline.
required_packages <- c(
  "eurostat",
  "WDI",
  "imfapi",
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "lubridate",
  "ggplot2",
  "plotly",
  "leaflet",
  "leaflet.extras",
  "knitr",
  "kableExtra",
  "sf",
  "spdep",
  "spatialreg",
  "splm",
  "plm",
  "lmtest",
  "car",
  "strucchange",
  "tseries",
  "urca",
  "countrycode",
  "here",
  "purrr",
  "furrr",
  "future",
  "parallel",
  "rnaturalearth",
  "rnaturalearthdata",
  "rmarkdown",
  "httr",
  "httr2",
  "bslib",
  "htmltools",
  "DT",
  "tibble",
  "modelsummary"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# --- Paths --------------------------------------------------------------------
path_root        <- here::here()
path_scripts     <- file.path(path_root, "scripts")
path_helpers     <- file.path(path_root, "scripts", "helpers")
path_data        <- file.path(path_root, "scripts", "output", "data")
path_reports     <- file.path(path_root, "scripts", "output", "quality_reports")
path_parlgov     <- file.path(path_data, "parlgov")

dir.create(path_helpers, showWarnings = FALSE, recursive = TRUE)
dir.create(path_data,    showWarnings = FALSE, recursive = TRUE)
dir.create(path_reports, showWarnings = FALSE, recursive = TRUE)
dir.create(path_parlgov, showWarnings = FALSE, recursive = TRUE)

# --- Country lists ------------------------------------------------------------
# Primary sample: EU member states that are also NATO members as of the study
# period, plus Norway (NATO non-EU) and GB (NATO, left EU 2020 but in sample
# for the full 1995-2023 period).
#
# Intentionally excluded from nato_eu_core:
#   AT — Austria:  EU member, NATO non-member (permanent neutrality)
#   CY — Cyprus:   EU member, NATO non-member (political dispute with Turkey)
#   IE — Ireland:  EU member, NATO non-member (permanent neutrality)
#   MT — Malta:    EU member, NATO non-member (permanent neutrality)
#   SE — Sweden:   NATO member only from March 2024, outside study period
#
# Greece is coded as "GR" throughout. Eurostat uses "EL" internally;
# all Eurostat download scripts recode "EL" -> "GR" before filtering.
nato_eu_core <- c(
  "BE", "BG", "HR", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IT", "LV", "LT", "LU", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "NO", "GB"
)

# Robustness sample is identical to core sample as of this revision.
nato_eu_robustness <- nato_eu_core

# Regression sample: nato_eu_core minus Luxembourg.
# Luxembourg is excluded as a structural outlier — its defence/GDP ratio
# is below 0.2% for most of the study period, far outside the range of
# all other NATO members and inconsistent with the theoretical model.
# This constant is used in 01_spatial_weights.R and 02_baseline_ols.R
# to avoid hardcoding the exclusion in multiple places.
regression_countries <- nato_eu_core[nato_eu_core != "LU"]

# GB exclusion from primary regression models (theoretical grounds):
#   GB is excluded from any model that includes immigration_rate because
#   immigration_rate is NA for all GB years. This NA is a deliberate
#   modelling decision, not a data gap. As an island nation, the
#   land-border threat measure (threat_land_log) systematically
#   underestimates GB's threat environment, while GB's defence commitments
#   reflect global power projection (nuclear deterrent, expeditionary
#   forces) rather than European territorial defence. Empirically, GB's
#   mean threat score is 58% below the sample mean while its mean defence
#   spending is 53% above the sample mean — the opposite of the
#   theory-predicted direction. GB is retained in the panel and enters
#   the robustness specification without immigration_rate (Check F in
#   09_revision_checks.R). See also Check G for formal outlier statistics.
#
# NO exclusion note:
#   Norway has 3 missing immigration_rate years (1995-1997) due to
#   Eurostat coverage starting in 1998 for NO. Those rows are dropped
#   from models including immigration_rate.

# --- Temporal coverage --------------------------------------------------------
year_start <- 1995
year_end   <- 2023

# --- Regime boundaries --------------------------------------------------------
# Single source of truth for the four analytical regimes.
# Regime 1: Post-Cold War consolidation (1995-2004)
# Regime 2: Pre-crisis stability (2005-2013)
# Regime 3: Post-Crimea rearmament (2014-2021)
# Regime 4: Post-Ukraine invasion surge (2022-2023)
#
# Used in: 05_merge_eurostat.R (panel coding).
# Note: vis/helpers/helper_plot_theme.R and helper_regime.R in the Shiny
# app cannot source this file directly and therefore hardcode these
# boundaries. If boundaries change here, update those files manually.
regimes <- list(
  regime_1 = c(1995, 2004),
  regime_2 = c(2005, 2013),
  regime_3 = c(2014, 2021),
  regime_4 = c(2022, 2023)
)

# --- Reproducibility ----------------------------------------------------------
set.seed(20240101)
