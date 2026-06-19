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

# =============================================================================
# App data loader — reads from scripts/output/app/ (autonomous CSV store)
# =============================================================================

#' Resolve the app data directory
#'
#' Tries here::here() first, then relative paths for shinyapps.io deployment.
#' @return Character path to scripts/output/app/
resolve_app_path <- function() {
  candidates <- c(
    tryCatch(here::here("scripts", "output", "app"),
             error = function(e) ""),
    file.path("scripts", "output", "app"),
    file.path("..", "scripts", "output", "app")
  )
  for (p in candidates) {
    if (nchar(p) > 0 && dir.exists(p)) return(p)
  }
  stop("Cannot locate scripts/output/app/ directory.")
}

#' Load all app-ready CSV files into a named list
#'
#' Each element is a data frame corresponding to one CSV in scripts/output/app/.
#' The element name is the filename without .csv.
#'
#' @param path_app Optional path override. If NULL, resolved automatically.
#' @return Named list of data frames.
load_app_data <- function(path_app = NULL) {
  if (is.null(path_app)) path_app <- resolve_app_path()

  csv_files <- list.files(path_app, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) stop("No CSV files found in ", path_app)

  out <- lapply(csv_files, function(f) {
    tryCatch(
      readr::read_csv(f, show_col_types = FALSE),
      error = function(e) {
        warning("Could not load ", basename(f), ": ", e$message)
        NULL
      }
    )
  })
  names(out) <- sub("\\.csv$", "", basename(csv_files))
  out
}

#' Resolve the project root for .md file reading
#'
#' @return Character path to project root (one level above vis/)
resolve_root_path <- function() {
  candidates <- c(
    tryCatch(here::here(), error = function(e) ""),
    file.path(".."),
    file.path(getwd(), "..")
  )
  for (p in candidates) {
    if (nchar(p) > 0 &&
        file.exists(file.path(p, "models", "results", "m1_m12.md"))) {
      return(normalizePath(p))
    }
  }
  # fallback: assume getwd() is vis/
  normalizePath(file.path(getwd(), ".."))
}

#' Read a section from a markdown file
#'
#' If heading is "full", returns the entire file content.
#' Otherwise extracts the section starting at the matching heading
#' up to (but not including) the next same-level heading.
#'
#' @param root      Project root path (from resolve_root_path()).
#' @param md_file   Relative path from root, e.g. "models/results/m1_m12.md".
#' @param heading   Either "full" or a heading prefix like "## 9.".
#' @return Character string of markdown content, or an error message.
read_md_section <- function(root, md_file, heading = "full") {
  path <- file.path(root, md_file)
  if (!file.exists(path)) {
    return(paste0("*File not found: `", md_file, "`*"))
  }
  lines <- readLines(path, warn = FALSE)
  if (heading == "full") return(paste(lines, collapse = "\n"))

  # Find the line that starts with the heading prefix
  start_idx <- which(startsWith(trimws(lines), trimws(heading)))
  if (length(start_idx) == 0) {
    return(paste0("*Section `", heading, "` not found in `", md_file, "`*"))
  }
  start_idx <- start_idx[1]

  # Determine heading level (number of leading #)
  level <- nchar(regmatches(heading, regexpr("^#+", heading)))

  # Find next heading of same or higher level after start
  heading_pattern <- paste0("^#{1,", level, "} ")
  end_idx <- which(grepl(heading_pattern, lines[(start_idx + 1):length(lines)]))
  if (length(end_idx) == 0) {
    end_line <- length(lines)
  } else {
    end_line <- start_idx + end_idx[1] - 1
  }

  paste(lines[start_idx:end_line], collapse = "\n")
}
