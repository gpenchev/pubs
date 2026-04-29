# =============================================================================
# 07_diagnostics.R
# Regression diagnostics
#
# Checks performed:
#   1. VIF on baseline two-way FE OLS
#   2. Breusch-Pagan heteroskedasticity test
#   3. Chow structural break tests at 2014 and 2022
#   4. Influence diagnostics (Cook's distance)
#
# Note on Cook's distance with two-way FE:
#   Cook's distance is computed on the lm() representation of the two-way
#   FE model (with country and year dummies). With many dummy variables,
#   Cook's distance may be inflated for observations in small groups.
#   Results should be interpreted as indicative rather than definitive.
#
# Output files:
#   vif_results.csv, heteroskedasticity_test.csv,
#   structural_break_tests.csv, influence_diagnostics.csv,
#   influence_by_country.csv, cook_distance_plot.png
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")
source(here::here("scripts", "helpers", "spatial_helpers.R"))

# --- Load results -------------------------------------------------------------
baseline <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial  <- readRDS(file.path(path_data, "spatial_panel_results.rds"))

reg_data <- baseline$reg_data

path_tables  <- file.path(path_root, "scripts", "output", "tables")
dir.create(path_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(path_reports, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Check 1: VIF on baseline OLS
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK 1: Variance Inflation Factors")
message(strrep("=", 60))

reg_data_lm <- reg_data %>%
  dplyr::mutate(
    country_f = as.factor(country),
    year_f    = as.factor(year)
  )

lm_twoway <- lm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year +
    country_f + year_f,
  data = reg_data_lm
)

vif_vals <- tryCatch(
  car::vif(lm_twoway),
  error = function(e) {
    message("VIF failed: ", e$message)
    NULL
  }
)

if (!is.null(vif_vals)) {
  if (is.matrix(vif_vals)) {
    vif_df <- data.frame(
      term     = rownames(vif_vals),
      vif      = round(vif_vals[, "GVIF"], 3),
      vif_adj  = round(vif_vals[, "GVIF^(1/(2*Df))"]^2, 3),
      stringsAsFactors = FALSE
    )
  } else {
    vif_df <- data.frame(
      term    = names(vif_vals),
      vif     = round(as.numeric(vif_vals), 3),
      vif_adj = round(as.numeric(vif_vals), 3),
      stringsAsFactors = FALSE
    )
  }

  vif_df <- vif_df %>%
    dplyr::filter(!grepl("^country_f|^year_f", term)) %>%
    dplyr::mutate(
      flag = dplyr::case_when(
        vif_adj > 10 ~ "SEVERE",
        vif_adj > 5  ~ "MODERATE",
        TRUE         ~ "OK"
      )
    )

  message("VIF results (substantive variables only):")
  print(vif_df)

  readr::write_csv(vif_df, file.path(path_data, "vif_results.csv"))
  message("VIF results saved.")
} else {
  vif_df <- NULL
}

# =============================================================================
# Check 2: Breusch-Pagan heteroskedasticity test
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK 2: Breusch-Pagan Heteroskedasticity Test")
message(strrep("=", 60))

bp_test <- tryCatch(
  lmtest::bptest(lm_twoway),
  error = function(e) {
    message("BP test failed: ", e$message)
    NULL
  }
)

if (!is.null(bp_test)) {
  bp_df <- data.frame(
    test      = "Breusch-Pagan",
    statistic = round(as.numeric(bp_test$statistic), 4),
    df        = as.integer(bp_test$parameter),
    p_value   = round(bp_test$p.value, 4),
    conclusion = ifelse(bp_test$p.value < 0.05,
                        "Heteroskedasticity present",
                        "No evidence of heteroskedasticity")
  )

  message("Breusch-Pagan test result:")
  print(bp_df)

  readr::write_csv(bp_df, file.path(path_data, "heteroskedasticity_test.csv"))
  message("Heteroskedasticity test saved.")
} else {
  bp_df <- NULL
}

# BP test on SAR residuals using lmtest::bptest directly
if (!is.null(spatial$m5_sar)) {
  sar_resid <- residuals(spatial$m5_sar)
  sar_lm    <- lm(sar_resid ~ fitted(spatial$m5_sar))
  sar_bp    <- tryCatch(
    lmtest::bptest(sar_lm),
    error = function(e) NULL
  )

  if (!is.null(sar_bp)) {
    message("BP test on SAR residuals:")
    message("  Statistic = ", round(as.numeric(sar_bp$statistic), 4),
            ", p = ", round(sar_bp$p.value, 4))
  }
}

# =============================================================================
# Check 3: Structural break tests (Chow at 2014 and 2022)
# Uses chow_test() from spatial_helpers.R
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK 3: Structural Break Tests")
message(strrep("=", 60))

chow_formula <- defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
  gdp_growth + immigration_rate + gov_left_right +
  gov_eu_position + election_year

sb_df <- dplyr::bind_rows(
  chow_test(chow_formula, reg_data_lm, 2014),
  chow_test(chow_formula, reg_data_lm, 2022)
)

message("Structural break test results:")
print(sb_df)
readr::write_csv(sb_df, file.path(path_data, "structural_break_tests.csv"))
message("Structural break tests saved.")

# --- Data-driven breakpoints (strucchange) ------------------------------------
bp_struc <- tryCatch({
  reg_data_ts <- reg_data_lm %>%
    dplyr::arrange(year, country)

  bp_obj     <- strucchange::breakpoints(chow_formula,
                                          data = reg_data_ts, h = 0.15)
  bp_summary <- summary(bp_obj)

  message("Data-driven breakpoints (strucchange):")
  print(bp_summary)

  list(
    breakpoints = bp_obj$breakpoints,
    bp_object   = bp_obj,
    bp_summary  = bp_summary
  )
}, error = function(e) {
  message("strucchange breakpoints failed: ", e$message)
  NULL
})

# =============================================================================
# Check 4: Influence diagnostics (Cook's distance)
# =============================================================================
message("\n", strrep("=", 60))
message("CHECK 4: Influence Diagnostics (Cook's Distance)")
message(strrep("=", 60))

cook_vals <- tryCatch(
  cooks.distance(lm_twoway),
  error = function(e) {
    message("Cook's distance failed: ", e$message)
    NULL
  }
)

if (!is.null(cook_vals)) {
  n_obs     <- length(cook_vals)
  threshold <- 4 / n_obs

  used_rows <- as.integer(rownames(model.frame(lm_twoway)))

  influence_df <- reg_data_lm %>%
    dplyr::slice(used_rows) %>%
    dplyr::select(country, year, defence_gdp) %>%
    dplyr::mutate(
      cooks_d   = as.numeric(cook_vals),
      flagged   = cooks_d > threshold,
      threshold = round(threshold, 6)
    ) %>%
    dplyr::arrange(dplyr::desc(cooks_d))

  n_flagged <- sum(influence_df$flagged, na.rm = TRUE)
  message("Observations flagged (Cook's D > 4/n = ",
          round(threshold, 4), "): ", n_flagged)

  top20 <- influence_df %>%
    dplyr::slice_head(n = 20)

  message("Top 20 high-leverage observations:")
  print(top20 %>% dplyr::select(country, year, defence_gdp, cooks_d, flagged))

  readr::write_csv(
    influence_df %>% dplyr::filter(flagged),
    file.path(path_data, "influence_diagnostics.csv")
  )
  message("Influence diagnostics saved.")

  plot_data <- influence_df %>%
    dplyr::mutate(
      label  = ifelse(flagged, paste0(country, " ", year), NA_character_),
      obs_id = dplyr::row_number()
    )

  cook_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = obs_id, y = cooks_d)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = flagged),
      width = 0.8
    ) +
    ggplot2::geom_hline(
      yintercept = threshold,
      linetype   = "dashed",
      colour     = "red",
      linewidth  = 0.6
    ) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "grey70", "TRUE" = "#d73027"),
      labels = c("FALSE" = "Normal", "TRUE" = "High leverage"),
      name   = NULL
    ) +
    ggplot2::labs(
      title    = "Cook's Distance by Observation",
      subtitle = paste0("Red dashed line = threshold (4/n = ",
                        round(threshold, 4), "); ",
                        n_flagged, " observations flagged"),
      x        = "Observation index",
      y        = "Cook's distance"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  tryCatch(
    ggplot2::ggsave(
      filename = file.path(path_reports, "cook_distance_plot.png"),
      plot     = cook_plot,
      width    = 12,
      height   = 5,
      dpi      = 300,
      device   = "cairo_png"
    ),
    error = function(e) message("cook_distance_plot.png save failed: ", e$message)
  )
  message("Cook's distance plot saved.")

  country_influence <- influence_df %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      n_flagged    = sum(flagged, na.rm = TRUE),
      max_cooks_d  = round(max(cooks_d, na.rm = TRUE), 6),
      mean_cooks_d = round(mean(cooks_d, na.rm = TRUE), 6),
      .groups      = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_flagged), dplyr::desc(max_cooks_d))

  message("Country-level influence summary:")
  print(country_influence)

  readr::write_csv(
    country_influence,
    file.path(path_data, "influence_by_country.csv")
  )
} else {
  influence_df      <- NULL
  country_influence <- NULL
}

# =============================================================================
# Save all diagnostics together
# =============================================================================
diagnostics_results <- list(
  vif_results       = vif_df,
  bp_test           = bp_df,
  structural_breaks = sb_df,
  influence_df      = influence_df,
  country_influence = country_influence,
  chow_2014         = sb_df[sb_df$break_year == 2014, ],
  chow_2022         = sb_df[sb_df$break_year == 2022, ],
  strucchange_bp    = if (!is.null(bp_struc)) bp_struc$breakpoints else NULL
)

saveRDS(diagnostics_results,
        file.path(path_data, "diagnostics_results.rds"))

message("\nScript 07_diagnostics complete.")
