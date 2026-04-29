# =============================================================================
# 09_coverage_check.R
# Missingness analysis by country and year
# Produces: missingness heatmap, summary table, country drop recommendations
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")

# --- Load panel ---------------------------------------------------------------
panel <- readRDS(file.path(path_data, "panel_full.rds"))

# --- Variables to check — intersect with panel columns to avoid errors --------
vars_candidate <- c(
  "defence_gdp",
  "debt_gdp",
  "deficit_gdp",
  "gdp_pc",
  "gdp_growth",
  "immigration_rate",
  "threat_score_log",
  "threat_land_log",
  "gov_left_right",
  "gov_eu_position"
)

vars_to_check <- intersect(vars_candidate, names(panel))

missing_vars <- setdiff(vars_candidate, vars_to_check)
if (length(missing_vars) > 0) {
  message("Variables not yet in panel (will be checked after ParlGov merge): ",
          paste(missing_vars, collapse = ", "))
}

# --- Missingness by country and variable --------------------------------------
miss_country <- panel %>%
  dplyr::group_by(country, country_name) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_to_check),
      ~ mean(is.na(.)) * 100,
      .names = "miss_{.col}"
    ),
    n_years = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    miss_mean = rowMeans(dplyr::across(dplyr::starts_with("miss_")))
  ) %>%
  dplyr::arrange(dplyr::desc(miss_mean))

# --- Missingness by year and variable -----------------------------------------
miss_year <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_to_check),
      ~ mean(is.na(.)) * 100,
      .names = "miss_{.col}"
    ),
    .groups = "drop"
  )

# --- Heatmap: missingness by country x variable -------------------------------
miss_long <- panel %>%
  dplyr::select(country, year, dplyr::all_of(vars_to_check)) %>%
  tidyr::pivot_longer(
    cols      = dplyr::all_of(vars_to_check),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(missing = is.na(value)) %>%
  dplyr::group_by(country, variable) %>%
  dplyr::summarise(miss_pct = mean(missing) * 100, .groups = "drop")

heatmap_plot <- ggplot2::ggplot(
  miss_long,
  ggplot2::aes(x = variable, y = country, fill = miss_pct)
) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::scale_fill_gradient(
    low  = "#f7fbff",
    high = "#08306b",
    name = "Missing %"
  ) +
  ggplot2::labs(
    title = "Missingness by Country and Variable",
    x     = "Variable",
    y     = "Country"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_reports, "missingness_heatmap.png"),
    plot     = heatmap_plot,
    width    = 10,
    height   = 8,
    dpi      = 150,
    device   = "cairo_png"
  ),
  error = function(e) message("missingness_heatmap.png save failed: ", e$message)
)

# --- Missingness over time plot -----------------------------------------------
miss_year_long <- miss_year %>%
  tidyr::pivot_longer(
    cols      = dplyr::starts_with("miss_"),
    names_to  = "variable",
    values_to = "miss_pct"
  ) %>%
  dplyr::mutate(variable = stringr::str_remove(variable, "^miss_"))

time_plot <- ggplot2::ggplot(
  miss_year_long,
  ggplot2::aes(x = year, y = miss_pct, colour = variable)
) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::labs(
    title  = "Missingness Over Time by Variable",
    x      = "Year",
    y      = "Missing %",
    colour = "Variable"
  ) +
  ggplot2::theme_minimal()

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_reports, "missingness_over_time.png"),
    plot     = time_plot,
    width    = 10,
    height   = 6,
    dpi      = 150,
    device   = "cairo_png"
  ),
  error = function(e) message("missingness_over_time.png save failed: ", e$message)
)

# --- Country drop recommendation ----------------------------------------------
# Guard against miss_defence_gdp not existing if defence_gdp was not in panel
if ("miss_defence_gdp" %in% names(miss_country)) {
  drop_candidates <- miss_country %>%
    dplyr::filter(miss_defence_gdp > 30) %>%
    dplyr::select(country, country_name, miss_defence_gdp, miss_mean)
} else {
  message("miss_defence_gdp not found in miss_country — skipping drop candidates.")
  drop_candidates <- data.frame(
    country          = character(0),
    country_name     = character(0),
    miss_defence_gdp = numeric(0),
    miss_mean        = numeric(0)
  )
}

# --- Save outputs -------------------------------------------------------------
readr::write_csv(miss_country,    file.path(path_reports, "missingness_by_country.csv"))
readr::write_csv(miss_year,       file.path(path_reports, "missingness_by_year.csv"))
readr::write_csv(drop_candidates, file.path(path_reports, "drop_candidates.csv"))

message("Script 09 complete.")
message("Countries flagged for potential exclusion: ", nrow(drop_candidates))
if (nrow(drop_candidates) > 0) {
  print(drop_candidates)
}
