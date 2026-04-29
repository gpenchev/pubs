# =============================================================================
# 11_outlier_check.R
# Outlier and implausible value detection
# Runs on v1 panel (before ParlGov merge).
# Political variables (gov_left_right, gov_eu_position, election_year) are
# not yet present — they are checked in 14_parlgov_quality_check.R.
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")
source(here::here("vis", "helpers", "helper_outliers.R"))

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

# --- IQR-based outlier detection ----------------------------------------------
outlier_flags <- panel %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vars_to_check),
      ~ flag_iqr_outliers(.),
      .names = "outlier_{.col}"
    )
  ) %>%
  dplyr::select(country, country_name, year, regime,
                dplyr::starts_with("outlier_"))

outlier_summary <- outlier_flags %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::starts_with("outlier_"),
      ~ sum(., na.rm = TRUE),
      .names = "{.col}_n"
    )
  )

# --- Year-on-year change outliers ---------------------------------------------
yoy_flags <- panel %>%
  dplyr::arrange(country, year) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vars_to_check),
      ~ abs(. - dplyr::lag(.)),
      .names = "yoy_{.col}"
    )
  ) %>%
  dplyr::ungroup()

yoy_outliers <- yoy_flags %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("yoy_"),
      ~ flag_iqr_outliers(., multiplier = 4),
      .names = "flag_{.col}"
    )
  ) %>%
  dplyr::filter(
    dplyr::if_any(dplyr::starts_with("flag_yoy_"), ~ . == TRUE)
  ) %>%
  dplyr::select(country, country_name, year,
                dplyr::starts_with("yoy_"),
                dplyr::starts_with("flag_yoy_"))

# --- Time series plots for visual inspection ----------------------------------
for (v in vars_to_check) {
  tryCatch({
    p <- ggplot2::ggplot(
      panel %>% dplyr::filter(!is.na(.data[[v]])),
      ggplot2::aes(x = year, y = .data[[v]],
                   group = country, colour = country)
    ) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::labs(
        title  = paste("Time series:", v),
        x      = "Year",
        y      = v,
        colour = "Country"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right",
                     legend.text = ggplot2::element_text(size = 7))

    ggplot2::ggsave(
      filename = file.path(path_reports, paste0("timeseries_", v, ".png")),
      plot     = p,
      width    = 12,
      height   = 6,
      dpi      = 150,
      device   = "cairo_png"
    )
    message("Saved timeseries_", v, ".png")
  }, error = function(e) {
    message("timeseries_", v, ".png save failed: ", e$message)
  })
}

# --- Save outputs -------------------------------------------------------------
readr::write_csv(outlier_flags,   file.path(path_reports, "outlier_flags_iqr.csv"))
readr::write_csv(yoy_outliers,    file.path(path_reports, "outlier_flags_yoy.csv"))
readr::write_csv(outlier_summary, file.path(path_reports, "outlier_summary.csv"))

message("Script 11 complete.")
message("IQR outlier counts by variable:")
print(outlier_summary)
message("Year-on-year jump outliers: ", nrow(yoy_outliers), " observations flagged.")
