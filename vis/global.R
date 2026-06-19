library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(tibble)
library(bslib)
library(bsicons)
library(readr)
library(here)

# --- Helpers ------------------------------------------------------------------
source(here::here("vis", "helpers", "helper_data.R"))
source(here::here("vis", "helpers", "helper_plot_theme.R"))
source(here::here("vis", "helpers", "helper_regime.R"))
source(here::here("vis", "helpers", "helper_outliers.R"))
source(here::here("vis", "helpers", "helper_map.R"))

# --- Modules ------------------------------------------------------------------
source(here::here("vis", "modules", "mod_timeseries.R"))
source(here::here("vis", "modules", "mod_scatter.R"))
source(here::here("vis", "modules", "mod_outliers.R"))
source(here::here("vis", "modules", "mod_map.R"))
source(here::here("vis", "modules", "mod_unitroot.R"))
source(here::here("vis", "modules", "mod_about.R"))
source(here::here("vis", "modules", "mod_results.R"))
source(here::here("vis", "modules", "mod_coefficients.R"))
source(here::here("vis", "modules", "mod_regime.R"))
source(here::here("vis", "modules", "mod_issues.R"))

# --- Load all app data from scripts/output/app/ -------------------------------
# The app is autonomous: reads only flat CSVs, no RDS model objects needed.
app_data <- tryCatch(
  load_app_data(),
  error = function(e) {
    message("WARNING: Could not load app data — ", e$message)
    list()
  }
)

# Convenience aliases used by existing modules (panel data + unit root)
panel_data <- tryCatch(
  app_data[["app_threat_panel"]],
  error = function(e) NULL
)

# Patch country_name and defence_source if absent (app_threat_panel is lean)
country_name_map <- c(
  AT="Austria", BE="Belgium", BG="Bulgaria", CZ="Czechia",
  DE="Germany", DK="Denmark", EE="Estonia", ES="Spain",
  FI="Finland", FR="France", GB="Great Britain", GR="Greece",
  HR="Croatia", HU="Hungary", IT="Italy", LT="Lithuania",
  LU="Luxembourg", LV="Latvia", NL="Netherlands", NO="Norway",
  PL="Poland", PT="Portugal", RO="Romania", SI="Slovenia",
  SK="Slovakia"
)
if (!is.null(panel_data)) {
  if (!"country_name" %in% names(panel_data)) {
    panel_data <- panel_data %>%
      dplyr::mutate(country_name = dplyr::coalesce(
        country_name_map[country], country
      ))
  }
  if (!"defence_source" %in% names(panel_data)) {
    panel_data <- panel_data %>%
      dplyr::mutate(defence_source = "WDI")
  }
}

# Unit root results — still loaded from original output for the unitroot module
unitroot_results <- tryCatch(
  readRDS(here::here("scripts", "output", "data", "unitroot_results.rds")),
  error = function(e) NULL
)

# Project root for .md reading
proj_root <- resolve_root_path()

# Convenience: conflict events (pre-filtered, 787 rows)
conflict_events <- app_data[["app_conflict_events"]]

# EU geometries — loaded lazily inside map module server, not here
# (avoids rnaturalearth network call blocking app startup)
