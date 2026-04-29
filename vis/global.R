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
library(here)

source(here::here("vis", "helpers", "helper_data.R"))
source(here::here("vis", "helpers", "helper_plot_theme.R"))
source(here::here("vis", "helpers", "helper_regime.R"))
source(here::here("vis", "helpers", "helper_outliers.R"))
source(here::here("vis", "helpers", "helper_map.R"))

source(here::here("vis", "modules", "mod_timeseries.R"))
source(here::here("vis", "modules", "mod_scatter.R"))
source(here::here("vis", "modules", "mod_outliers.R"))
source(here::here("vis", "modules", "mod_map.R"))
source(here::here("vis", "modules", "mod_unitroot.R"))

path_data <- here::here("scripts", "output", "data")

panel_data   <- load_panel(path_data)
var_labels   <- get_variable_labels()
country_labs <- get_country_labels(panel_data)
year_range   <- range(panel_data$year, na.rm = TRUE)

eu_geometries <- tryCatch(
  load_eu_geometries(),
  error = function(e) {
    message("Warning: could not load EU geometries — map tab will be unavailable.")
    NULL
  }
)

unitroot_results <- tryCatch(
  readRDS(file.path(path_data, "unitroot_results.rds")),
  error = function(e) NULL
)

ged_map_events <- tryCatch(
  readRDS(file.path(path_data, "ucdp_map_events.rds")),
  error = function(e) NULL
)
