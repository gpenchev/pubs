# =============================================================================
# 06_publication_table.R
# Publication-ready regression tables, coefficient plots, marginal effects
#
# Output files:
#   regression_table.html          — HTML regression table
#   regression_table.tex           — LaTeX regression table
#                                    (requires \usepackage{booktabs} in preamble)
#   coef_plot.png                  — coefficient forest plot across models
#   marginal_effects_regime4.png   — marginal effect of threat by regime (M7)
#   fit_summary.html               — model fit statistics table
# =============================================================================

source(here::here("scripts", "00_setup.R"))
options(bitmapType = "cairo")
source(here::here("scripts", "helpers", "spatial_helpers.R"))

if (!requireNamespace("modelsummary", quietly = TRUE)) install.packages("modelsummary")
if (!requireNamespace("kableExtra",  quietly = TRUE)) install.packages("kableExtra")

library(modelsummary)
library(kableExtra)

# --- Load results -------------------------------------------------------------
baseline <- readRDS(file.path(path_data, "baseline_ols_results.rds"))
spatial  <- readRDS(file.path(path_data, "spatial_panel_results.rds"))
tables   <- readRDS(file.path(path_data, "regression_tables.rds"))

path_out <- file.path(path_root, "scripts", "output", "tables")
dir.create(path_out, showWarnings = FALSE, recursive = TRUE)

# --- Variable labels ----------------------------------------------------------
coef_labels <- c(
  "threat_land_log"          = "Threat score (land, log)",
  "threat_score_log"         = "Threat score (all, log)",
  "lag_defence_gdp"          = "Defence spending (lagged)",
  "debt_gdp"                 = "Government debt (% GDP)",
  "deficit_gdp"              = "Fiscal deficit (% GDP)",
  "gdp_growth"               = "GDP growth (%)",
  "immigration_rate"         = "Immigration rate (per 1000)",
  "gov_left_right"           = "Government left-right",
  "gov_eu_position"          = "Government EU position",
  "election_year"            = "Election year",
  "regime2"                  = "Regime 2 (2005-2013)",
  "regime3"                  = "Regime 3 (2014-2021)",
  "regime4"                  = "Regime 4 (2022-2023)",
  "threat_land_log:regime2"  = "Threat x Regime 2",
  "threat_land_log:regime3"  = "Threat x Regime 3",
  "threat_land_log:regime4"  = "Threat x Regime 4",
  "rho"                      = "Spatial lag (rho)",
  "lambda"                   = "Spatial error (lambda)"
)

# --- Core terms to display ---------------------------------------------------
core_terms <- c(
  "threat_land_log",
  "threat_score_log",
  "lag_defence_gdp",
  "debt_gdp",
  "deficit_gdp",
  "gdp_growth",
  "immigration_rate",
  "gov_left_right",
  "gov_eu_position",
  "election_year",
  "regime2",
  "regime3",
  "regime4",
  "threat_land_log:regime2",
  "threat_land_log:regime3",
  "threat_land_log:regime4",
  "rho",
  "lambda"
)

# --- Build results table from results_core -----------------------------------
pub_table <- tables$results_core %>%
  dplyr::filter(term %in% core_terms) %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    coef_str = paste0(
      formatC(estimate,  format = "f", digits = 3), stars
    ),
    se_str = paste0(
      "(", formatC(std_error, format = "f", digits = 3), ")"
    ),
    term_label = dplyr::recode(term, !!!coef_labels)
  )

# --- Wide coefficient table --------------------------------------------------
wide_coef <- pub_table %>%
  dplyr::select(model, term_label, coef_str) %>%
  tidyr::pivot_wider(names_from = model, values_from = coef_str)

wide_se <- pub_table %>%
  dplyr::select(model, term_label, se_str) %>%
  tidyr::pivot_wider(names_from = model, values_from = se_str)

interleaved <- purrr::map_dfr(seq_len(nrow(wide_coef)), function(i) {
  dplyr::bind_rows(
    wide_coef[i, ],
    wide_se[i, ] %>% dplyr::mutate(term_label = "")
  )
})

# --- Derive header dynamically from actual model columns ---------------------
model_cols <- names(interleaved)[-1]

n_baseline   <- sum(grepl("^M[34]:", model_cols))
n_spatial    <- sum(grepl("^M[5-9]:", model_cols))
n_robustness <- length(model_cols) - n_baseline - n_spatial

header_above <- c(
  " "          = 1L,
  "Baseline"   = n_baseline,
  "Spatial"    = n_spatial,
  "Robustness" = n_robustness
)

# --- LR test note ------------------------------------------------------------
lr_note <- if (!is.null(tables$lr_summary)) {
  paste0(
    "LR test SAR vs SEM: stat = ",
    round(tables$lr_summary$lr_stat, 3),
    ", p = ",
    round(tables$lr_summary$lr_pval, 4),
    ". Preferred: ",
    tables$lr_summary$preferred, "."
  )
} else {
  ""
}

footnote_text <- paste(
  "Standard errors in parentheses.",
  "* p<0.10, ** p<0.05, *** p<0.01.",
  "Models M3-M4: two-way FE (plm).",
  "Models M5-M12: spatial lag/error (spatialreg) with block-diagonal W.",
  "Country and year fixed effects included but not shown.",
  lr_note
)

# --- Save HTML table ---------------------------------------------------------
html_table <- interleaved %>%
  kableExtra::kbl(
    caption  = "Determinants of EU Defence Spending (% GDP), 1998-2023",
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 11
  ) %>%
  kableExtra::add_header_above(header_above) %>%
  kableExtra::footnote(general = footnote_text)

tryCatch(
  kableExtra::save_kable(
    html_table,
    file = file.path(path_out, "regression_table.html")
  ),
  error = function(e) {
    message("HTML table save failed: ", e$message,
            ". Check that the output directory is writable.")
  }
)

message("HTML table saved.")

# --- Save LaTeX table --------------------------------------------------------
latex_footnote <- paste(
  "Standard errors in parentheses.",
  "* p<0.10, ** p<0.05, *** p<0.01.",
  "Country and year fixed effects included but not shown.",
  lr_note
)

latex_table <- interleaved %>%
  kableExtra::kbl(
    format   = "latex",
    caption  = "Determinants of EU Defence Spending (\\% GDP), 1998--2023",
    booktabs = TRUE,
    label    = "tab:regression"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position", "scale_down")
  ) %>%
  kableExtra::add_header_above(header_above) %>%
  kableExtra::footnote(
    general = latex_footnote,
    escape  = FALSE
  )

tryCatch(
  writeLines(
    as.character(latex_table),
    file.path(path_out, "regression_table.tex")
  ),
  error = function(e) {
    message("LaTeX table save failed: ", e$message)
  }
)

message("LaTeX table saved.")

# --- Coefficient forest plot -------------------------------------------------
plot_terms <- c(
  "threat_land_log",
  "debt_gdp",
  "deficit_gdp",
  "gdp_growth",
  "immigration_rate",
  "gov_left_right",
  "gov_eu_position",
  "election_year"
)

plot_models <- c(
  "M3: Two-way FE",
  "M4: FE + Regime",
  "M5: SAR",
  "M6: SEM",
  "M7: SAR + Regime",
  "M8: SAR Robust threat",
  "M12: SAR lagged DV"
)

coef_plot_data <- tables$results_core %>%
  dplyr::filter(
    term  %in% plot_terms,
    model %in% plot_models,
    !is.na(estimate),
    !is.na(std_error)
  ) %>%
  dplyr::mutate(
    ci_lo      = estimate - 1.96 * std_error,
    ci_hi      = estimate + 1.96 * std_error,
    term_label = dplyr::recode(term, !!!coef_labels),
    model      = factor(model, levels = rev(plot_models))
  )

coef_plot <- ggplot2::ggplot(
  coef_plot_data,
  ggplot2::aes(x = estimate, y = model, colour = model)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      colour = "grey50", linewidth = 0.5) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
    height = 0.2, linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::facet_wrap(
    ~ term_label,
    scales = "free_x",
    ncol   = 4
  ) +
  ggplot2::scale_colour_brewer(palette = "Dark2", guide = "none") +
  ggplot2::labs(
    title    = "Coefficient Estimates Across Model Specifications",
    subtitle = "Points = point estimate; bars = 95% CI",
    x        = "Estimate",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    strip.text       = ggplot2::element_text(face = "bold", size = 9),
    axis.text.y      = ggplot2::element_text(size = 8),
    panel.grid.minor = ggplot2::element_blank()
  )

tryCatch(
  ggplot2::ggsave(
    filename = file.path(path_out, "coef_plot.png"),
    plot     = coef_plot,
    width    = 14,
    height   = 8,
    dpi      = 300,
    device   = "cairo_png"
  ),
  error = function(e) message("coef_plot.png save failed: ", e$message)
)

message("Coefficient plot saved.")

# --- Marginal effects plot: threat_land_log x regime (M7) -------------------
regime_labels <- c(
  "1" = "Regime 1\n(1995-2004)",
  "2" = "Regime 2\n(2005-2013)",
  "3" = "Regime 3\n(2014-2021)",
  "4" = "Regime 4\n(2022-2023)"
)

m7_coefs <- extract_spatialreg(spatial$m7_sar_regime, "M7")

if (!is.null(m7_coefs)) {
  base_threat <- m7_coefs$estimate[m7_coefs$term == "threat_land_log"]
  base_se     <- m7_coefs$std_error[m7_coefs$term == "threat_land_log"]

  get_interaction <- function(regime_n) {
    term_name <- paste0("threat_land_log:regime", regime_n)
    row       <- m7_coefs[m7_coefs$term == term_name, ]
    if (nrow(row) == 0) return(list(est = 0, se = 0))
    list(est = row$estimate, se = row$std_error)
  }

  me_data <- purrr::map_dfr(1:4, function(r) {
    if (r == 1) {
      est <- base_threat
      se  <- base_se
    } else {
      int <- get_interaction(r)
      est <- base_threat + int$est
      se  <- sqrt(base_se^2 + int$se^2)
    }
    data.frame(
      regime   = as.character(r),
      estimate = est,
      ci_lo    = est - 1.96 * se,
      ci_hi    = est + 1.96 * se
    )
  }) %>%
    dplyr::mutate(
      regime_label = dplyr::recode(regime, !!!regime_labels),
      regime_label = factor(regime_label, levels = regime_labels)
    )

  me_plot <- ggplot2::ggplot(
    me_data,
    ggplot2::aes(x = regime_label, y = estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
      width = 0.15, linewidth = 0.8, colour = "#2171b5"
    ) +
    ggplot2::geom_point(size = 4, colour = "#2171b5") +
    ggplot2::labs(
      title    = "Marginal Effect of Threat Proximity on Defence Spending by Regime",
      subtitle = "Model M7: SAR with regime interactions; bars = 95% CI",
      x        = NULL,
      y        = "Marginal effect of threat_land_log on defence_gdp"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  tryCatch(
    ggplot2::ggsave(
      filename = file.path(path_out, "marginal_effects_regime4.png"),
      plot     = me_plot,
      width    = 8,
      height   = 5,
      dpi      = 300,
      device   = "cairo_png"
    ),
    error = function(e) message("marginal_effects_regime4.png save failed: ", e$message)
  )

  message("Marginal effects plot saved.")
} else {
  message("M7 coefficients not available — skipping marginal effects plot.")
}

# --- Fit summary table -------------------------------------------------------
if (!is.null(tables$fit_summary)) {
  fit_html <- tables$fit_summary %>%
    kableExtra::kbl(
      caption  = "Model Fit Statistics",
      booktabs = TRUE,
      digits   = 2
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed"),
      full_width        = FALSE
    )

  tryCatch(
    kableExtra::save_kable(
      fit_html,
      file = file.path(path_out, "fit_summary.html")
    ),
    error = function(e) message("Fit summary save failed: ", e$message)
  )
  message("Fit summary table saved.")
}

message("Script 06_publication_table complete.")
message("Outputs saved to: ", path_out)
