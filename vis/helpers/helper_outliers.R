# Note on outlier detection scope:
#   flag_iqr_outliers computes IQR across all countries and years globally.
#   This is appropriate for detecting data entry errors and implausible values.
#   It is not regime-stratified or per-country — a value that is an outlier
#   globally may be normal within a specific country or time period.
#
#   flag_yoy_outliers operates per country (via group_by) and is therefore
#   not directly comparable to flag_iqr_outliers. A value can be flagged by
#   one method but not the other.

#' Flag IQR-based outliers in a numeric vector
#'
#' A value is flagged if it falls below Q1 - multiplier*IQR or above
#' Q3 + multiplier*IQR. IQR is computed globally across all non-NA values.
#'
#' @param x          Numeric vector.
#' @param multiplier IQR multiplier (default 3). Higher values are more lenient.
#' @return Logical vector of the same length as x. TRUE = outlier.
flag_iqr_outliers <- function(x, multiplier = 3) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - multiplier * iqr) | x > (q3 + multiplier * iqr)
}

#' Flag year-on-year jump outliers per country
#'
#' For each country, computes the mean and SD of absolute year-on-year
#' changes. A value is flagged if its absolute change from the previous
#' year exceeds mean + threshold * SD.
#'
#' @param data      Data frame containing `country`, `year`, and `variable`.
#' @param variable  Column name of the variable to check.
#' @param threshold SD multiplier (default 3).
#' @return The input data frame with an additional logical column `yoy_flag`.
flag_yoy_outliers <- function(data, variable, threshold = 3) {
  data %>%
    dplyr::arrange(country, year) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      .yoy_val    = .data[[variable]],
      .yoy_lag    = dplyr::lag(.yoy_val),
      .yoy_diff   = abs(.yoy_val - .yoy_lag),
      .yoy_mean_d = mean(.yoy_diff, na.rm = TRUE),
      .yoy_sd_d   = sd(.yoy_diff,   na.rm = TRUE),
      yoy_flag    = !is.na(.yoy_diff) &
                    .yoy_diff > (.yoy_mean_d + threshold * .yoy_sd_d)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.yoy_val, .yoy_lag, .yoy_diff, .yoy_mean_d, .yoy_sd_d))
}

#' Build an outlier summary table for display in the Shiny app
#'
#' Returns only flagged rows, with columns appropriate for the selected method.
#'
#' @param data       Panel data frame filtered to the variable of interest.
#' @param variable   Column name of the variable to check.
#' @param method     One of "iqr", "yoy", or "both".
#' @param multiplier IQR multiplier passed to flag_iqr_outliers (default 3).
#' @param threshold  SD multiplier passed to flag_yoy_outliers (default 3).
#'   Note: threshold is not currently exposed in mod_outliers_ui but is
#'   retained here for future use.
#' @return Data frame of flagged observations.
get_outlier_table <- function(data, variable,
                               method     = c("iqr", "yoy", "both"),
                               multiplier = 3,
                               threshold  = 3) {
  method <- match.arg(method)

  base <- data %>%
    dplyr::filter(!is.na(.data[[variable]])) %>%
    dplyr::mutate(iqr_flag = flag_iqr_outliers(.data[[variable]], multiplier))

  if (method == "iqr") {
    base %>%
      dplyr::filter(iqr_flag) %>%
      dplyr::select(country, country_name, year, regime,
                    dplyr::all_of(variable), iqr_flag)
  } else if (method == "yoy") {
    flag_yoy_outliers(base, variable, threshold) %>%
      dplyr::filter(yoy_flag) %>%
      dplyr::select(country, country_name, year, regime,
                    dplyr::all_of(variable), yoy_flag)
  } else {
    yoy_data <- flag_yoy_outliers(base, variable, threshold)
    yoy_data %>%
      dplyr::filter(iqr_flag | yoy_flag) %>%
      dplyr::select(country, country_name, year, regime,
                    dplyr::all_of(variable), iqr_flag, yoy_flag)
  }
}
