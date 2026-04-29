#' Load the main panel dataset
#'
#' Tries here::here() first for interactive use; falls back to a relative
#' path for deployment on shinyapps.io where here::here() may not resolve
#' correctly.
#'
#' @param path_data Optional path to the data directory. If NULL, resolved
#'   automatically.
#' @return A data frame containing panel_full.rds.
load_panel <- function(path_data = NULL) {
  if (is.null(path_data)) {
    path_data <- tryCatch(
      here::here("scripts", "output", "data"),
      error = function(e) file.path("scripts", "output", "data")
    )
  }
  readRDS(file.path(path_data, "panel_full.rds"))
}

#' Get labelled variable choices for Shiny selectInput widgets
#'
#' Returns a named character vector where names are human-readable labels
#' and values are column names in the panel dataset.
#'
#' @return Named character vector of variable labels to column names.
get_variable_labels <- function() {
  c(
    "Defence Spending (% GDP)"       = "defence_gdp",
    "Government Debt (% GDP)"        = "debt_gdp",
    "Fiscal Deficit (% GDP)"         = "deficit_gdp",
    "GDP per Capita (EUR)"           = "gdp_pc",
    "GDP Growth Rate (%)"            = "gdp_growth",
    "Immigration Rate (per 1000)"    = "immigration_rate",
    "Threat Score (log)"             = "threat_score_log",
    "Threat Score Land (log)"        = "threat_land_log",
    "Government Left-Right Position" = "gov_left_right",
    "Government EU Position"         = "gov_eu_position",
    "Election Year"                  = "election_year"
  )
}

#' Get labelled country choices for Shiny selectInput widgets
#'
#' Returns a named character vector where names are full country names
#' and values are ISO2 country codes.
#'
#' @param panel A data frame containing `country` and `country_name` columns.
#' @return Named character vector of country names to ISO2 codes.
get_country_labels <- function(panel) {
  panel %>%
    dplyr::distinct(country, country_name) %>%
    dplyr::arrange(country_name) %>%
    tibble::deframe()
}

#' Filter the panel dataset by countries, years, and variable
#'
#' If `countries` is NULL or empty, all countries are returned.
#' If `years` is NULL, all years are returned.
#' If `variable` is NULL or not found in the data, all columns are returned.
#'
#' @param data      The panel data frame.
#' @param countries Character vector of ISO2 country codes, or NULL for all.
#' @param years     Numeric vector of length 1 (exact year) or 2 (range), or NULL.
#' @param variable  Column name to retain (plus standard ID columns), or NULL.
#' @return Filtered data frame.
filter_panel <- function(data, countries = NULL, years = NULL, variable = NULL) {
  out <- data
  if (!is.null(countries) && length(countries) > 0) {
    out <- out %>% dplyr::filter(country %in% countries)
  }
  if (!is.null(years)) {
    if (length(years) == 2) {
      out <- out %>% dplyr::filter(year >= years[1], year <= years[2])
    } else if (length(years) == 1) {
      out <- out %>% dplyr::filter(year == years[1])
    }
  }
  if (!is.null(variable) && variable %in% names(out)) {
    out <- out %>%
      dplyr::select(country, country_name, year, regime,
                    defence_source,
                    dplyr::all_of(variable))
  }
  out
}
