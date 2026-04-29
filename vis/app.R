library(shiny)
library(here)

source(here::here("vis", "global.R"))

ui <- navbarPage(
  title = "EU Defence Panel Explorer",
  theme = bslib::bs_theme(bootswatch = "flatly"),

  tabPanel(
    "Time Series",
    mod_timeseries_ui("ts")
  ),

  tabPanel(
    "Map",
    mod_map_ui("map")
  ),

  tabPanel(
    "Scatter",
    mod_scatter_ui("scatter")
  ),

  tabPanel(
    "Outliers",
    mod_outliers_ui("outliers")
  ),

  tabPanel(
    "Unit Root",
    mod_unitroot_ui("unitroot")
  ),

  tabPanel(
    "Help",
    mod_help_ui("help")
  )
)

server <- function(input, output, session) {
  mod_timeseries_server("ts",       panel_data)
  mod_map_server("map",             panel_data,
                 ged_events    = ged_map_events,
                 eu_geometries = eu_geometries)
  mod_scatter_server("scatter",     panel_data)
  mod_outliers_server("outliers",   panel_data)
  mod_unitroot_server("unitroot",   unitroot_results)
  mod_help_server("help")
}

shinyApp(ui, server)
