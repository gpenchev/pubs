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
# App data loader — reads from vis/data/ (self-contained deployment store)
# =============================================================================
# All data the app needs is copied into vis/data/ by 11_app_data.R Block 8.
# This makes the vis/ directory fully self-contained for Shiny Server.
#
# vis/data/app/  — 23 flat CSVs
# vis/data/md/   — all markdown files (flat, path encoded as __ separator)
# vis/data/rds/  — unitroot_results.rds
#
# Fallback to scripts/output/ paths for local interactive use when vis/data/
# has not yet been populated (e.g. mid-pipeline development).
# =============================================================================

#' Resolve the app CSV data directory
#'
#' Primary: vis/data/app/ (self-contained, works on Shiny Server)
#' Fallback: scripts/output/app/ (local interactive use)
#' @return Character path to the CSV directory
resolve_app_path <- function() {
  candidates <- c(
    file.path("data", "app"),                          # deployed: cwd = vis/
    file.path("..", "scripts", "output", "app"),        # deployed fallback
    file.path("scripts", "output", "app"),              # interactive: cwd = root
    tryCatch(here::here("scripts", "output", "app"), error = function(e) "")
  )
  for (p in candidates) {
    if (nchar(p) > 0 && dir.exists(p) &&
        length(list.files(p, pattern = "\\.csv$")) > 0)
      return(normalizePath(p))
  }
  stop("Cannot locate app CSV directory (vis/data/app/ or scripts/output/app/).")
}

#' Load all app-ready CSV files into a named list
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

#' Resolve the vis/data/md/ directory for markdown files
#'
#' Primary: vis/data/md/ (self-contained deployment)
#' Fallback: project root (local interactive use when vis/data/md/ is empty)
#' @return Character path to the md directory
resolve_md_path <- function() {
  candidates <- c(
    file.path("data", "md"),          # deployed: cwd = vis/
    file.path("..", "vis", "data", "md")  # interactive: cwd = project root
  )
  for (p in candidates) {
    if (nchar(p) > 0 && dir.exists(p) &&
        length(list.files(p, pattern = "\\.md$")) > 0)
      return(normalizePath(p))
  }
  NULL  # signals fallback to proj_root-based reading
}

#' Resolve the project root (kept for local interactive fallback)
#'
#' @return Character path to project root
resolve_root_path <- function() {
  # vis/data/md/ is the primary source; this is only used as fallback
  candidates <- c(
    file.path(".."),
    file.path(getwd(), ".."),
    tryCatch(here::here(), error = function(e) "")
  )
  for (p in candidates) {
    if (nchar(p) > 0 && dir.exists(file.path(p, "scripts")))
      return(normalizePath(p))
  }
  normalizePath(file.path(getwd(), ".."))
}

#' Read a section from a markdown file
#'
#' Resolves the file in this order:
#'   1. vis/data/md/<flat_name>  — flat name with "__" path separator
#'      (self-contained deployment; populated by 11_app_data.R Block 8)
#'   2. file.path(root, md_file) — local interactive fallback using proj_root
#'
#' The flat name is derived from md_file by replacing "/" with "__", e.g.:
#'   "models/results/m1_m12.md" → "models__results__m1_m12.md"
#'
#' If heading is "full", returns the entire file content.
#' Otherwise extracts the section starting at the matching heading
#' up to (but not including) the next same-level heading.
#'
#' @param root      Project root path (from resolve_root_path()). Used as
#'                  fallback only; may be NULL if vis/data/md/ is populated.
#' @param md_file   Relative path from root, e.g. "models/results/m1_m12.md".
#' @param heading   Either "full" or a heading prefix like "## 9.".
#' @return Character string of markdown content, or an error message.
read_md_section <- function(root, md_file, heading = "full") {
  # --- Resolve file path ------------------------------------------------------
  # Primary: vis/data/md/ flat file (deployment-safe)
  flat_name <- gsub("/", "__", md_file, fixed = TRUE)
  md_dir    <- resolve_md_path()
  path      <- NULL

  if (!is.null(md_dir)) {
    candidate <- file.path(md_dir, flat_name)
    if (file.exists(candidate)) path <- candidate
  }

  # Fallback: proj_root + relative path (local interactive)
  if (is.null(path) && !is.null(root) && nchar(root) > 0) {
    candidate <- file.path(root, md_file)
    if (file.exists(candidate)) path <- candidate
  }

  if (is.null(path)) {
    return(paste0("*File not found: `", md_file, "`*"))
  }

  # --- Extract section --------------------------------------------------------
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
