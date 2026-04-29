# =============================================================================
# 10_balance_check.R
# Panel balance diagnostic
# Checks whether the panel is balanced (every country observed every year)
# Produces: balance summary, observation count plot
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")

# --- Load panel ---------------------------------------------------------------
panel <- readRDS(file.path(path_data, "panel_full.rds"))

# --- Observation count by country ---------------------------------------------
obs_by_country <- panel %>%
  dplyr::group_by(country, country_name) %>%
  dplyr::summarise(
    n_obs          = dplyr::n(),
    n_obs_defence  = sum(!is.na(defence_gdp)),
    years_present  = paste(range(year), collapse = "-"),
    .groups        = "drop"
  ) %>%
  dplyr::arrange(n_obs_defence)

# --- Observation count by year ------------------------------------------------
obs_by_year <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_countries         = dplyr::n(),
    n_countries_defence = sum(!is.na(defence_gdp)),
    .groups             = "drop"
  )

# --- Is the panel balanced? ---------------------------------------------------
expected_obs <- length(nato_eu_robustness) * length(seq(year_start, year_end))
actual_obs   <- nrow(panel)
is_balanced  <- actual_obs == expected_obs

message("Expected observations: ", expected_obs)
message("Actual observations:   ", actual_obs)
message("Panel is balanced: ", is_balanced)

# --- Plot: countries per year -------------------------------------------------
balance_plot <- ggplot2::ggplot(
  obs_by_year,
  ggplot2::aes(x = year, y = n_countries_defence)
) +
  ggplot2::geom_col(fill = "#2171b5") +
  ggplot2::geom_hline(
    yintercept = length(nato_eu_robustness),
    linetype   = "dashed",
    colour     = "red"
  ) +
  ggplot2::labs(
    title    = "Number of Countries with Defence Spending Data by Year",
    subtitle = paste0("Dashed line = full sample (N=", length(nato_eu_robustness), ")"),
    x        = "Year",
    y        = "N countries"
  ) +
  ggplot2::theme_minimal()

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_reports, "balance_by_year.png"),
    plot     = balance_plot,
    width    = 10,
    height   = 5,
    dpi      = 150,
    device   = "cairo_png"
  ),
  error = function(e) message("balance_by_year.png save failed: ", e$message)
)

# --- Plot: observations per country -------------------------------------------
country_plot <- ggplot2::ggplot(
  obs_by_country,
  ggplot2::aes(
    x    = n_obs_defence,
    y    = reorder(country_name, n_obs_defence)
  )
) +
  ggplot2::geom_col(fill = "#2171b5") +
  ggplot2::geom_vline(
    xintercept = length(seq(year_start, year_end)),
    linetype   = "dashed",
    colour     = "red"
  ) +
  ggplot2::labs(
    title    = "Observations with Defence Spending Data by Country",
    subtitle = paste0("Dashed line = full time series (T=",
                      length(seq(year_start, year_end)), ")"),
    x        = "N years with data",
    y        = NULL
  ) +
  ggplot2::theme_minimal()

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_reports, "balance_by_country.png"),
    plot     = country_plot,
    width    = 10,
    height   = 8,
    dpi      = 150,
    device   = "cairo_png"
  ),
  error = function(e) message("balance_by_country.png save failed: ", e$message)
)

# --- Save outputs -------------------------------------------------------------
readr::write_csv(obs_by_country, file.path(path_reports, "balance_by_country.csv"))
readr::write_csv(obs_by_year,    file.path(path_reports, "balance_by_year.csv"))

message("Script 10 complete.")
