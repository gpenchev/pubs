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
# Use file.path() relative to the vis/ directory — works both locally
# (when vis/ is the working directory) and on Shiny Server (where here()
# resolves to vis/ and would produce vis/vis/... double-prefix paths).
source(file.path("helpers", "helper_data.R"))
source(file.path("helpers", "helper_plot_theme.R"))
source(file.path("helpers", "helper_regime.R"))
source(file.path("helpers", "helper_outliers.R"))
source(file.path("helpers", "helper_map.R"))

# --- Modules ------------------------------------------------------------------
source(file.path("modules", "mod_timeseries.R"))
source(file.path("modules", "mod_scatter.R"))
source(file.path("modules", "mod_outliers.R"))
source(file.path("modules", "mod_map.R"))
source(file.path("modules", "mod_unitroot.R"))
source(file.path("modules", "mod_about.R"))
source(file.path("modules", "mod_results.R"))
source(file.path("modules", "mod_coefficients.R"))
source(file.path("modules", "mod_regime.R"))
source(file.path("modules", "mod_issues.R"))

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

# Project root — kept for local interactive fallback (MD reading).
# On Shiny Server vis/data/md/ is used instead; proj_root is not critical.
proj_root <- resolve_root_path()

# Unit root results — primary: vis/data/rds/ (self-contained deployment)
#                     fallback: scripts/output/data/ (local interactive)
unitroot_results <- tryCatch({
  rds_candidates <- c(
    file.path("data", "rds", "unitroot_results.rds"),           # deployed: cwd = vis/
    file.path(proj_root, "scripts", "output", "data", "unitroot_results.rds")  # local
  )
  rds_path <- Filter(file.exists, rds_candidates)[1]
  if (!is.na(rds_path) && nchar(rds_path) > 0) readRDS(rds_path) else NULL
}, error = function(e) NULL)

# Convenience: conflict events (pre-filtered, 787 rows)
conflict_events <- app_data[["app_conflict_events"]]

# EU geometries — loaded lazily inside map module server, not here
# (avoids rnaturalearth network call blocking app startup)
