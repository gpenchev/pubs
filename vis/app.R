library(shiny)
library(here)

# When deployed on Shiny Server, here() resolves to the vis/ directory itself.
# Use a plain relative path so it works both locally (cwd = vis/) and deployed.
source("global.R")

# =============================================================================
# Helper: build a standard 4-sub-tab layout (Figures / Tables / Maps / Results)
# Inner figures section uses navset_card_tab for bslib-native card framing.
# Tables section wraps each table in a bslib::card for visual separation.
# =============================================================================
make_tab_ui <- function(figures_ui, tables_ui, maps_ui = NULL, results_id) {
  bslib::navset_pill(
    bslib::nav_panel("Figures", br(), figures_ui),
    bslib::nav_panel("Tables",  br(), tables_ui),
    bslib::nav_panel("Maps",
      br(),
      if (!is.null(maps_ui)) maps_ui
      else tags$p(class = "text-muted",
                  bsicons::bs_icon("map"), " No map available for this section.")
    ),
    bslib::nav_panel("Results", br(), results_id)
  )
}

# Helper: wrap a DT table + optional download button in a bslib card
table_card <- function(title, tbl_output, dl_id = NULL, icon = "table") {
  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      tags$span(bsicons::bs_icon(icon), " ", title),
      if (!is.null(dl_id))
        downloadButton(dl_id, "CSV", class = "btn btn-sm btn-outline-secondary")
    ),
    bslib::card_body(tbl_output)
  )
}

# Helper: shared context note above a bare plot
plot_note <- function(text) {
  tags$p(class = "text-muted mb-2", style = "font-size:0.875em;",
         bsicons::bs_icon("info-circle"), " ", text)
}

# =============================================================================
# Custom CSS
# =============================================================================
app_css <- tags$head(tags$style(HTML("
  /* Tighten base font size for data-dense layouts */
  body { font-size: 0.9rem; }

  /* Active pill tab: subtle left border accent */
  .nav-pills .nav-link.active {
    border-left: 3px solid #18BC9C;
    padding-left: calc(var(--bs-nav-link-padding-x) - 3px);
  }

  /* Card: subtle shadow */
  .card { box-shadow: 0 1px 4px rgba(0,0,0,0.07); }

  /* DT tables: match tighter font */
  .dataTables_wrapper { font-size: 0.875rem; }

  /* Download buttons in card headers */
  .card-header .btn-outline-secondary {
    font-size: 0.75rem;
    padding: 2px 8px;
  }

  /* Navbar brand spacing */
  .navbar-brand { font-weight: 600; }
")))

# =============================================================================
# UI
# =============================================================================
ui <- bslib::page_navbar(
  title        = tags$span(bsicons::bs_icon("shield-shaded"), " EU Defence Panel"),
  window_title = "EU Defence Panel",
  theme = bslib::bs_theme(
    bootswatch    = "flatly",
    version       = 5,
    "font-size-base" = "0.9rem"
  ),
  selected       = "About",
  navbar_options = bslib::navbar_options(collapsible = TRUE),
  header         = app_css,

  # --------------------------------------------------------------------------
  # TAB 0 — About
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "About",
    mod_about_ui("about")
  ),

  # --------------------------------------------------------------------------
  # TAB 1 — Threat Index
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "Threat Index",
    make_tab_ui(
      figures_ui = bslib::navset_card_tab(
        bslib::nav_panel("Time Series",
          br(), mod_timeseries_ui("threat_ts")
        ),
        bslib::nav_panel("UCDP vs GPR scatter",
          br(), mod_scatter_ui("threat_scatter")
        ),
        bslib::nav_panel("GPR divergence over time",
          br(),
          plot_note("Mean and maximum divergence between UCDP land-threat and GPR index per year across the 13-country GPR subsample. Spikes at 2014 and 2022 confirm kinetic bias."),
          plotly::plotlyOutput("gpr_divergence_plot", height = PLOT_HEIGHT_COMPACT)
        )
      ),
      tables_ui = tagList(
        table_card("Country threat summary", DT::dataTableOutput("threat_country_tbl"), "dl_threat_country", "bar-chart"),
        br(),
        table_card("GPR correlation by country", DT::dataTableOutput("gpr_corr_tbl"), "dl_gpr_corr", "graph-up")
      ),
      maps_ui = mod_map_ui("threat_map",
                           has_all_events = !is.null(all_events)),
      results_id = mod_results_ui("threat_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 2 — Estimation
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "Estimation",
    make_tab_ui(
      figures_ui = bslib::navset_card_tab(
        bslib::nav_panel("Coefficient forest plot",
          br(), mod_coefficients_ui("coef_plot")
        ),
        bslib::nav_panel("Regime effects",
          br(), mod_regime_ui("regime_chart")
        ),
        bslib::nav_panel("Gov EU position reversal",
          br(),
          plot_note("Pre/post-2014 sign reversal of gov_eu_position: before 2014 pro-EU cabinets spent marginally more; after 2014 they spent significantly less. Eurosceptic nationalist governments drove post-2014 rearmament."),
          plotly::plotlyOutput("eu_pos_reversal_plot", height = PLOT_HEIGHT_COMPACT)
        ),
        bslib::nav_panel("Spatial decomposition",
          br(),
          plot_note("Levels SAR \u03c1 (M5) vs lagged-DV SAR \u03c1 (M12) vs first-difference SAR \u03c1 (FD). The sign reversal from +0.177 to \u22120.091 reveals long-run complementarity vs short-run burden-sharing substitution."),
          plotly::plotlyOutput("rho_decomp_plot", height = PLOT_HEIGHT_COMPACT)
        ),
        bslib::nav_panel("Model AIC comparison",
          br(),
          plot_note("AIC values across all 13 model specifications. Lower is better. M5 SAR is the primary specification; M13 GPR shows the cost of using text-based instead of kinetic threat."),
          plotly::plotlyOutput("aic_comparison_plot", height = PLOT_HEIGHT_COMPACT)
        ),
        bslib::nav_panel("Unit root heatmap",
          br(),
          plot_note("ADF/KPSS unit root test results per country\u2013variable combination. Green = stationary, orange = mixed, red = unit root."),
          mod_unitroot_ui("unitroot")
        )
      ),
      tables_ui = tagList(
        table_card("Full model fit table (M1\u2013M13)", DT::dataTableOutput("model_fit_tbl"), "dl_model_fit", "table"),
        br(),
        table_card("Spatial lag (\u03c1) comparison across specifications", DT::dataTableOutput("rho_comparison_tbl"), "dl_rho", "diagram-3"),
        br(),
        table_card("Country fixed effects (M5 SAR)", DT::dataTableOutput("country_fe_tbl"), "dl_country_fe", "pin-map")
      ),
      maps_ui = tagList(
        tags$p(class = "text-muted mb-2",
               bsicons::bs_icon("info-circle"),
               " Higher values = country spends more than the model predicts from threat + fiscal variables alone."),
        tags$p(class = "text-warning mb-2", style = "font-size:0.85em;",
               bsicons::bs_icon("exclamation-triangle"),
               " Country fixed effects and Cook's D are",
               tags$strong("time-invariant"),
               "— the year slider has no effect for these variables.",
               " Switch to Defence Spending or another panel variable to see year-by-year changes."),
        mod_map_ui("est_map",
                   var_choices    = get_map_variable_labels(),
                   default_var    = "country_fe",
                   has_all_events = !is.null(all_events))
      ),
      results_id = mod_results_ui("est_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 3 — Robustness
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "Robustness",
    make_tab_ui(
      figures_ui = bslib::navset_card_tab(
        bslib::nav_panel("Check I \u2014 2022/23 cross-section",
          br(),
          plot_note("Within-year cross-sectional OLS for 2022 and 2023 (N=22, no fixed effects). Confirms threat-defence gradient is real: \u03b2=+0.381 (2022), \u03b2=+0.282 (2023). Year FE absorption in panel models is an identification artefact."),
          mod_scatter_ui("checkI_scatter")
        ),
        bslib::nav_panel("Check H \u2014 BG 2019 sensitivity",
          br(),
          plot_note("All coefficients change by less than 1 SE when Bulgaria 2019 (highest Cook\u2019s D) is excluded. Verdict: STABLE."),
          bslib::card(
            bslib::card_header(
              bsicons::bs_icon("check-circle-fill"), " Bulgaria 2019 exclusion \u2014 all results ",
              tags$span(class = "badge bg-success", "STABLE")
            ),
            bslib::card_body(
              plotly::plotlyOutput("checkH_plot", height = PLOT_HEIGHT_COMPACT)
            )
          )
        ),
        bslib::nav_panel("VIF",
          br(),
          plot_note("Variance Inflation Factors for M3 two-way FE. threat_land_log and debt_gdp have severe VIF (\u226510) due to shared temporal structure with year FEs, not bivariate collinearity. Orthogonalisation check confirms coefficients are stable."),
          bslib::card(
            bslib::card_header(bsicons::bs_icon("bar-chart-fill"), " Variance Inflation Factors"),
            bslib::card_body(
              plotly::plotlyOutput("vif_plot", height = PLOT_HEIGHT_COMPACT)
            )
          )
        ),
        bslib::nav_panel("Outliers / Cook\u2019s D",
          br(),
          mod_outliers_ui("outliers")
        ),
        bslib::nav_panel("Country influence",
          br(),
          plot_note("Maximum Cook\u2019s D per country across all M5 SAR observations. BG has the highest single-observation influence; Baltic states have the highest aggregate influence count."),
          plotly::plotlyOutput("influence_bar_plot", height = PLOT_HEIGHT_COMPACT)
        ),
        bslib::nav_panel("Checks A / B / F / G",
          br(),
          plot_note("Four summary diagnostics: (A) spatial persistence decomposition, (B) Regime 4 statistical power, (F) immigration sensitivity, (G) GB structural outlier."),
          plotly::plotlyOutput("checks_abfg_plot", height = PLOT_HEIGHT_STANDARD)
        )
      ),
      tables_ui = tagList(
        table_card("All checks A\u2013J summary", DT::dataTableOutput("checks_summary_tbl"), "dl_checks", "clipboard-check"),
        br(),
        table_card("LR tests \u2014 SAR with regime breaks", DT::dataTableOutput("regime_lr_tbl_lr"), "dl_regime_lr", "activity"),
        br(),
        table_card("AIC comparison \u2014 regime specifications", DT::dataTableOutput("regime_aic_tbl"), "dl_regime_aic", "bar-chart"),
        br(),
        table_card("Check I \u2014 Cross-section 2022/2023", DT::dataTableOutput("check_i_tbl"), "dl_check_i", "calendar-check"),
        br(),
        table_card("Check H \u2014 BG 2019 sensitivity", DT::dataTableOutput("check_h_tbl"), "dl_check_h", "shield-check"),
        br(),
        table_card("Check J \u2014 Immigration interaction", DT::dataTableOutput("check_j_tbl"), "dl_check_j", "people")
      ),
      maps_ui = tagList(
        tags$p(class = "text-muted mb-2",
               bsicons::bs_icon("info-circle"),
               " Choropleth of max Cook\u2019s D per country \u2014 shows which countries most influence SAR results."),
        tags$p(class = "text-warning mb-2", style = "font-size:0.85em;",
               bsicons::bs_icon("exclamation-triangle"),
               " Cook\u2019s D and country fixed effects are",
               tags$strong("time-invariant"),
               "— the year slider has no effect for these variables.",
               " Switch to Defence Spending or another panel variable to see year-by-year changes."),
        mod_map_ui("rob_map",
                   var_choices    = get_map_variable_labels(),
                   default_var    = "max_cooks_d",
                   has_all_events = !is.null(all_events))
      ),
      results_id = mod_results_ui("rob_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 4 — Issues (sidebar layout)
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "Issues",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        tags$p(style = "font-size:0.85em; color:#666;",
               bsicons::bs_icon("info-circle"),
               " Select an issue to explore the figure, data, and research notes.",
               "Each issue is a named methodological weakness with a pre-prepared response."),
        hr(),
        tags$p(tags$strong("Issue 1 \u2014 Kinetic bias"),
               tags$br(),
               tags$span(class = "badge bg-warning", "Discussion framing"),
               tags$br(),
               tags$small("UCDP does not capture hybrid warfare or diplomatic coercion.")),
        hr(),
        tags$p(tags$strong("Issue 2 \u2014 50km sea threshold"),
               tags$br(),
               tags$span(class = "badge bg-info", "Country FE absorbs residual"),
               tags$br(),
               tags$small("Greece Aegean threats excluded; Greece FE is highest in the sample.")),
        hr(),
        tags$p(tags$strong("Issue 3 \u2014 Regime 1 truncation"),
               tags$br(),
               tags$span(class = "badge bg-secondary", "Disclosed"),
               tags$br(),
               tags$small("Immigration data missing 1995\u20131999; Regime 1 estimated on 2000\u20132004 only.")),
        hr(),
        tags$p(tags$strong("Issue 4 \u2014 GPR sample bias"),
               tags$br(),
               tags$span(class = "badge bg-danger", "Conservative estimate"),
               tags$br(),
               tags$small("Highest-threat CEE countries missing from GPR; UCDP advantage is understated."))
      ),
      mod_issues_ui("issues")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 5 — Help
  # --------------------------------------------------------------------------
  bslib::nav_panel(
    "Help",
    mod_help_ui("help")
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # --- About ------------------------------------------------------------------
  mod_about_server("about", proj_root)

  # --- Threat Index -----------------------------------------------------------
  mod_timeseries_server("threat_ts", panel_data)
  mod_scatter_server("threat_scatter", panel_data)

  mod_map_server("threat_map",
                 panel_data     = panel_data,
                 ged_events     = conflict_events,
                 ged_all_events = all_events)

  # GPR divergence time series
  output$gpr_divergence_plot <- plotly::renderPlotly({
    df <- app_data[["app_gpr_divergence_year"]]
    req(!is.null(df), nrow(df) > 0)
    df_long <- dplyr::bind_rows(
      df %>% dplyr::transmute(year, value = mean_divergence, series = "Mean divergence"),
      df %>% dplyr::transmute(year, value = max_divergence,  series = "Max divergence")
    )
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = year, y = value,
                                                colour = series, group = series)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_vline(xintercept = c(2014, 2022),
                          linetype = "dashed", colour = "red", linewidth = 0.7) +
      ggplot2::annotate("text", x = 2014.2, y = max(df$max_divergence) * 0.95,
                        label = "Crimea", colour = "red", hjust = 0, size = 3.2) +
      ggplot2::annotate("text", x = 2022.2, y = max(df$max_divergence) * 0.95,
                        label = "Ukraine", colour = "red", hjust = 0, size = 3.2) +
      ggplot2::scale_colour_manual(
        values = c("Mean divergence" = "#2166ac", "Max divergence" = "#d73027"), name = NULL
      ) +
      ggplot2::labs(
        title    = "UCDP vs GPR divergence over time (13-country subsample)",
        subtitle = "Larger values = GPR and UCDP give more different threat signals",
        x = "Year", y = "Divergence (normalised units)"
      ) +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "gpr_divergence_timeseries"
    )
  })

  # Threat tables
  output$threat_country_tbl <- DT::renderDataTable({
    df <- app_data[["app_threat_country"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))) %>%
      DT::datatable(rownames = FALSE, options = list(pageLength = 12, dom = "tip"),
                    class = "table-sm table-hover")
  })
  output$dl_threat_country <- downloadHandler(
    filename = "threat_country_summary.csv",
    content  = function(f) readr::write_csv(app_data[["app_threat_country"]], f)
  )

  output$gpr_corr_tbl <- DT::renderDataTable({
    df <- app_data[["app_gpr_correlation"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))) %>%
      DT::datatable(rownames = FALSE, options = list(pageLength = 13, dom = "tip"),
                    class = "table-sm table-hover")
  })
  output$dl_gpr_corr <- downloadHandler(
    filename = "gpr_correlation.csv",
    content  = function(f) readr::write_csv(app_data[["app_gpr_correlation"]], f)
  )

  mod_results_server("threat_results", proj_root, list(
    list(label = "Threat Index Results", file = "models/results/threat_index.md",  heading = "full"),
    list(label = "Methodology",          file = "methodology/models/threat.md",    heading = "full"),
    list(label = "Variables",            file = "methodology/models/variables.md", heading = "full")
  ))

  # --- Estimation -------------------------------------------------------------
  mod_coefficients_server("coef_plot",  app_data[["app_coef_long"]])
  mod_regime_server("regime_chart",     app_data[["app_regime_effects"]])
  mod_unitroot_server("unitroot",       unitroot_results)

  # Gov EU position pre/post-2014 reversal
  output$eu_pos_reversal_plot <- plotly::renderPlotly({
    df <- app_data[["app_coef_long"]]
    req(!is.null(df))
    df_eu <- df %>%
      dplyr::filter(term == "gov_eu_position",
                    model %in% c("M10b: SAR post-2014", "M10c: SAR pre-2014")) %>%
      dplyr::mutate(
        period  = dplyr::if_else(grepl("post", model), "Post-2014 (M10b)", "Pre-2014 (M10c)"),
        ci_lo   = estimate - 1.96 * std_error,
        ci_hi   = estimate + 1.96 * std_error,
        colour  = dplyr::if_else(estimate > 0, "#2166ac", "#d73027"),
        tooltip = paste0(period, "\nCoef: ", round(estimate, 4),
                         "\nSE: ", round(std_error, 4),
                         "\np: ", round(p_value, 4))
      )
    p <- ggplot2::ggplot(df_eu, ggplot2::aes(x = period, y = estimate,
                                              fill = period, text = tooltip)) +
      ggplot2::geom_col(width = 0.5, alpha = 0.85) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
                             width = 0.15, colour = "grey30", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::scale_fill_manual(values = c("Pre-2014 (M10c)" = "#2166ac",
                                            "Post-2014 (M10b)" = "#d73027"), name = NULL) +
      ggplot2::labs(
        title    = "Gov. EU position coefficient: pre vs post-2014",
        subtitle = "z-test for difference p = 0.008 \u2014 significant reversal confirmed",
        x = NULL, y = "Coefficient (95% CI)"
      ) +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(showlegend = FALSE),
      fname = "eu_position_reversal"
    )
  })

  # Spatial decomposition dot-plot
  output$rho_decomp_plot <- plotly::renderPlotly({
    df <- app_data[["app_rho_comparison"]]
    req(!is.null(df))
    df_decomp <- df %>%
      dplyr::filter(model %in% c("M5: SAR", "M12: SAR lagged DV",
                                 "FD SAR (first-difference)")) %>%
      dplyr::mutate(
        ci_lo   = rho - 1.96 * rho_se,
        ci_hi   = rho + 1.96 * rho_se,
        model   = factor(model, levels = c("M5: SAR", "M12: SAR lagged DV",
                                           "FD SAR (first-difference)")),
        colour  = dplyr::if_else(rho > 0, "#2166ac", "#d73027"),
        tooltip = paste0(model, "\n\u03c1: ", round(rho, 4),
                         "\nSE: ", round(rho_se, 4),
                         "\np: ", round(rho_p, 4),
                         "\n", note)
      )
    p <- ggplot2::ggplot(df_decomp,
                         ggplot2::aes(x = rho, y = model, text = tooltip)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
                              height = 0.2, colour = "grey60") +
      ggplot2::geom_point(ggplot2::aes(colour = rho > 0), size = 4) +
      ggplot2::scale_colour_manual(
        values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
        labels = c("TRUE" = "Positive \u03c1", "FALSE" = "Negative \u03c1"), name = NULL
      ) +
      ggplot2::annotate("text", x = 0.18, y = 3.4,
                        label = "Levels: long-run complementarity",
                        colour = "#2166ac", size = 3, hjust = 0) +
      ggplot2::annotate("text", x = -0.09, y = 0.6,
                        label = "FD: short-run substitution",
                        colour = "#d73027", size = 3, hjust = 0) +
      ggplot2::labs(title = "Spatial lag (\u03c1) persistence vs diffusion decomposition",
                    subtitle = "Sign reversal from levels to first-differences: both consistent with NATO dynamics",
                    x = "\u03c1 estimate (95% CI)", y = NULL) +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "rho_decomposition"
    )
  })

  # Model AIC comparison
  output$aic_comparison_plot <- plotly::renderPlotly({
    df <- app_data[["app_model_fit"]]
    req(!is.null(df))
    df_aic <- df %>%
      dplyr::filter(!is.na(aic_val)) %>%
      dplyr::mutate(
        primary  = grepl("^M5", model),
        gpr      = grepl("GPR|M13", model),
        category = dplyr::case_when(
          primary ~ "Primary (M5)",
          gpr     ~ "GPR comparison (M13)",
          TRUE    ~ "Other"
        ),
        tooltip  = paste0(model, "\nAIC: ", round(aic_val, 2), "\nN: ", n_obs)
      )
    p <- ggplot2::ggplot(df_aic,
                         ggplot2::aes(x = stats::reorder(model, aic_val),
                                      y = aic_val, fill = category, text = tooltip)) +
      ggplot2::geom_col(alpha = 0.85) +
      ggplot2::scale_fill_manual(
        values = c("Primary (M5)" = "#18BC9C",
                   "GPR comparison (M13)" = "#d73027",
                   "Other" = "#aaaaaa"), name = NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Model AIC comparison (lower = better fit)",
                    subtitle = "M5 SAR is primary; M13 GPR has \u0394AIC = +17.6 vs M5",
                    x = NULL, y = "AIC") +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "model_aic_comparison"
    )
  })

  # Estimation tables
  output$model_fit_tbl <- DT::renderDataTable({
    df <- app_data[["app_model_fit"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 14),
                    class = "table-sm table-hover")
  })
  output$dl_model_fit <- downloadHandler(
    filename = "model_fit.csv",
    content  = function(f) readr::write_csv(app_data[["app_model_fit"]], f)
  )

  output$rho_comparison_tbl <- DT::renderDataTable({
    df <- app_data[["app_rho_comparison"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 12),
                    class = "table-sm table-hover")
  })
  output$dl_rho <- downloadHandler(
    filename = "rho_comparison.csv",
    content  = function(f) readr::write_csv(app_data[["app_rho_comparison"]], f)
  )

  output$country_fe_tbl <- DT::renderDataTable({
    df <- app_data[["app_country_fe"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 23),
                    class = "table-sm table-hover")
  })
  output$dl_country_fe <- downloadHandler(
    filename = "country_fixed_effects.csv",
    content  = function(f) readr::write_csv(app_data[["app_country_fe"]], f)
  )

  # Map: country FE as choropleth — panel_data patched with fe_value and max_cooks_d
  country_fe_panel <- tryCatch({
    fe  <- app_data[["app_country_fe"]]
    inf <- app_data[["app_influence_country"]]
    out <- panel_data
    if (!is.null(fe) && !is.null(out))
      out <- out %>%
        dplyr::left_join(fe %>% dplyr::select(country, country_fe = fe_value),
                         by = "country") %>%
        dplyr::mutate(country_fe = dplyr::coalesce(country_fe, 0))
    if (!is.null(inf) && !is.null(out))
      out <- out %>%
        dplyr::left_join(inf %>% dplyr::select(country, max_cooks_d), by = "country") %>%
        dplyr::mutate(max_cooks_d = dplyr::coalesce(max_cooks_d, 0))
    out
  }, error = function(e) panel_data)

  mod_map_server("est_map",
                 panel_data     = country_fe_panel,
                 ged_events     = conflict_events,
                 ged_all_events = all_events)

  mod_results_server("est_results", proj_root, list(
    list(label = "M1\u2013M13 Results", file = "models/results/m1_m12.md",      heading = "full"),
    list(label = "Model Methods",       file = "methodology/models/models.md",  heading = "full"),
    list(label = "Plain language",      file = "methodology/models/naive.md",   heading = "full")
  ))

  # --- Robustness -------------------------------------------------------------
  mod_scatter_server("checkI_scatter", panel_data)

  output$checkH_plot <- plotly::renderPlotly({
    df <- app_data[["app_check_h"]]
    req(!is.null(df))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = comparison)) +
      ggplot2::geom_segment(
        ggplot2::aes(x = comparison, xend = comparison,
                     y = coef_full, yend = coef_no_bg19),
        colour = "grey60", linewidth = 0.8
      ) +
      ggplot2::geom_point(ggplot2::aes(y = coef_full,    colour = "Full sample"),      size = 3.5) +
      ggplot2::geom_point(ggplot2::aes(y = coef_no_bg19, colour = "Without BG 2019"), size = 3.5) +
      ggplot2::scale_colour_manual(
        values = c("Full sample" = "#2166ac", "Without BG 2019" = "#d73027"), name = NULL
      ) +
      ggplot2::labs(x = NULL, y = "Coefficient") +
      ggplot2::coord_flip() +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p) %>% plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "check_h_sensitivity"
    )
  })

  output$vif_plot <- plotly::renderPlotly({
    df <- app_data[["app_vif"]]
    req(!is.null(df), nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x    = stats::reorder(term, vif),
      y    = vif,
      fill = flag
    )) +
      ggplot2::geom_col(alpha = 0.85) +
      ggplot2::geom_hline(yintercept = 10, linetype = "dashed", colour = "red") +
      ggplot2::scale_fill_manual(
        values = c("OK" = "#2166ac", "SEVERE" = "#d73027", "MODERATE" = "#f28e2b"), name = NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Variance Inflation Factors (M3 two-way FE)",
                    subtitle = "Red dashed line = VIF 10 threshold",
                    x = NULL, y = "VIF") +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p) %>% plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "vif_plot"
    )
  })

  # Country influence bar chart
  output$influence_bar_plot <- plotly::renderPlotly({
    df <- app_data[["app_influence_country"]]
    req(!is.null(df))
    p <- ggplot2::ggplot(
      df %>% dplyr::arrange(dplyr::desc(max_cooks_d)),
      ggplot2::aes(
        x    = stats::reorder(country, max_cooks_d),
        y    = max_cooks_d,
        fill = max_cooks_d > 0.04,
        text = paste0(country, "\nMax Cook's D: ", round(max_cooks_d, 4),
                      "\nN flagged: ", n_flagged)
      )
    ) +
      ggplot2::geom_col(alpha = 0.85) +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
        labels = c("FALSE" = "Low influence", "TRUE" = "High influence"), name = NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Country influence on M5 SAR results (max Cook\u2019s D)",
                    subtitle = "BG has highest single-observation influence; Baltics highest aggregate count",
                    x = NULL, y = "Max Cook\u2019s D") +
      theme_defence()
    configure_plotly(
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.12)),
      fname = "country_influence"
    )
  })

  # Checks A/B/F/G panel (4-panel subplot via patchwork-style plotly subplots)
  output$checks_abfg_plot <- plotly::renderPlotly({
    # Check A: persistence decomposition (rho values)
    df_a <- app_data[["app_rho_comparison"]] %>%
      dplyr::filter(model %in% c("M5: SAR", "M12: SAR lagged DV",
                                 "FD SAR (first-difference)")) %>%
      dplyr::mutate(label = c("M5\nLevels", "M12\nLagged", "FD\nFirst-diff"))

    p_a <- ggplot2::ggplot(df_a, ggplot2::aes(x = label, y = rho,
                                               fill = rho > 0)) +
      ggplot2::geom_col(width = 0.5, alpha = 0.85) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
                                 guide = "none") +
      ggplot2::labs(title = "A: Persistence decomposition (\u03c1)",
                    x = NULL, y = "\u03c1") +
      theme_defence()

    # Check B: regime 4 power
    df_b <- data.frame(
      label = c("Observed N", "Required N"),
      n     = c(44, 184),
      flag  = c(FALSE, TRUE)
    )
    p_b <- ggplot2::ggplot(df_b, ggplot2::aes(x = label, y = n, fill = flag)) +
      ggplot2::geom_col(width = 0.5, alpha = 0.85) +
      ggplot2::geom_hline(yintercept = 184, linetype = "dashed", colour = "red") +
      ggplot2::scale_fill_manual(values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
                                 guide = "none") +
      ggplot2::annotate("text", x = 1.5, y = 190,
                        label = "80% power threshold", size = 3, colour = "red") +
      ggplot2::labs(title = "B: Regime 4 statistical power",
                    subtitle = "N=44 observed; N=184 required for 80% power",
                    x = NULL, y = "N observations") +
      theme_defence()

    # Check F: immigration sensitivity (threat and political coefs M5 vs no-immig)
    df_f <- app_data[["app_coef_long"]] %>%
      dplyr::filter(
        term %in% c("gov_left_right", "gov_eu_position"),
        model %in% c("M5: SAR", "M3: Two-way FE")
      ) %>%
      dplyr::mutate(
        spec    = dplyr::if_else(grepl("M5", model), "M5 (with immigration)", "M3 (no immigration)"),
        tooltip = paste0(term, " / ", spec, "\nCoef: ", round(estimate, 4))
      )
    p_f <- ggplot2::ggplot(df_f, ggplot2::aes(x = term, y = estimate,
                                               fill = spec, text = tooltip)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::scale_fill_manual(values = c("M5 (with immigration)" = "#2166ac",
                                            "M3 (no immigration)" = "#f28e2b"), name = NULL) +
      ggplot2::labs(title = "F: Immigration sensitivity",
                    subtitle = "Political ideology signs depend on immigration control",
                    x = NULL, y = "Coefficient") +
      theme_defence()

    # Check G: GB structural outlier
    df_g <- data.frame(
      measure  = c("Threat (relative)", "Defence (relative)"),
      gb_value = c(0.607, 2.35),
      avg_value = c(1.0, 1.60)
    ) %>%
      tidyr::pivot_longer(c(gb_value, avg_value),
                          names_to = "group", values_to = "value") %>%
      dplyr::mutate(group = dplyr::if_else(group == "gb_value", "Great Britain", "Sample mean"))

    p_g <- ggplot2::ggplot(df_g, ggplot2::aes(x = measure, y = value,
                                               fill = group)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
      ggplot2::scale_fill_manual(values = c("Great Britain" = "#d73027",
                                            "Sample mean" = "#2166ac"), name = NULL) +
      ggplot2::labs(title = "G: GB structural outlier",
                    subtitle = "GB: 39% below mean threat, 47% above mean defence",
                    x = NULL, y = "Value") +
      theme_defence()

    # Combine as 2x2 subplot
    plotly::subplot(
      plotly::ggplotly(p_a) %>% plotly::layout(showlegend = FALSE),
      plotly::ggplotly(p_b) %>% plotly::layout(showlegend = FALSE),
      plotly::ggplotly(p_f, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.15)),
      plotly::ggplotly(p_g) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.15)),
      nrows      = 2,
      shareX     = FALSE,
      shareY     = FALSE,
      titleX     = TRUE,
      titleY     = TRUE,
      margin     = 0.08
    ) %>%
      configure_plotly(fname = "checks_abfg") %>%
      plotly::layout(title = "Diagnostic checks A / B / F / G")
  })

  mod_outliers_server("outliers", panel_data)

  # Robustness tables — regime_lr split into LR test and AIC comparison
  output$regime_lr_tbl_lr <- DT::renderDataTable({
    df <- app_data[["app_regime_lr"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    df_lr <- df %>%
      dplyr::filter(!is.na(lr_stat)) %>%
      dplyr::select(test, lr_stat, df, p_value, preferred) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))
    rename_dt_cols(df_lr) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 5),
                    class = "table-sm table-hover")
  })
  output$dl_regime_lr <- downloadHandler(
    filename = "regime_lr_tests.csv",
    content  = function(f) {
      df <- app_data[["app_regime_lr"]]
      if (!is.null(df)) readr::write_csv(df, f)
    }
  )

  output$regime_aic_tbl <- DT::renderDataTable({
    df <- app_data[["app_regime_lr"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    df_aic <- df %>%
      dplyr::filter(!is.na(model), !is.na(aic)) %>%
      dplyr::select(model, n_params, log_lik, aic, bic) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3)))
    rename_dt_cols(df_aic) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "t"),
                    class = "table-sm table-hover")
  })
  output$dl_regime_aic <- downloadHandler(
    filename = "regime_aic_comparison.csv",
    content  = function(f) {
      df <- app_data[["app_regime_lr"]]
      if (!is.null(df)) {
        readr::write_csv(
          df %>% dplyr::filter(!is.na(model), !is.na(aic)) %>%
            dplyr::select(model, n_params, log_lik, aic, bic),
          f
        )
      }
    }
  )

  output$checks_summary_tbl <- DT::renderDataTable({
    df <- app_data[["app_checks_summary"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df) %>%
      DT::datatable(
        rownames = FALSE,
        options  = list(
          dom = "tip",
          pageLength = 12,
          columnDefs = list(list(className = "dt-wrap", targets = 1))
        ),
        class = "table-sm"
      )
  })
  output$dl_checks <- downloadHandler(
    filename = "revision_checks_summary.csv",
    content  = function(f) readr::write_csv(app_data[["app_checks_summary"]], f)
  )

  output$check_i_tbl <- DT::renderDataTable({
    df <- app_data[["app_check_i"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip"), class = "table-sm table-hover")
  })
  output$dl_check_i <- downloadHandler(
    filename = "check_i_crosssection.csv",
    content  = function(f) readr::write_csv(app_data[["app_check_i"]], f)
  )

  output$check_h_tbl <- DT::renderDataTable({
    df <- app_data[["app_check_h"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip"), class = "table-sm table-hover")
  })
  output$dl_check_h <- downloadHandler(
    filename = "check_h_bg2019.csv",
    content  = function(f) readr::write_csv(app_data[["app_check_h"]], f)
  )

  output$check_j_tbl <- DT::renderDataTable({
    df <- app_data[["app_check_j"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
      DT::datatable(rownames = FALSE, options = list(dom = "tip"), class = "table-sm table-hover")
  })
  output$dl_check_j <- downloadHandler(
    filename = "check_j_immigration_interaction.csv",
    content  = function(f) readr::write_csv(app_data[["app_check_j"]], f)
  )

  # Robustness map: Cook's D choropleth — panel already patched in country_fe_panel
  mod_map_server("rob_map",
                 panel_data     = country_fe_panel,
                 ged_events     = conflict_events,
                 ged_all_events = all_events)

  mod_results_server("rob_results", proj_root, list(
    list(label = "Diagnostics & Breaks", file = "models/results/diagnostics_breaks.md", heading = "full"),
    list(label = "Check I & J",          file = "models/results/revision_checks_ij.md", heading = "full"),
    list(label = "All Weaknesses",       file = "methodology/models/weak.md",           heading = "full")
  ))

  # --- Issues -----------------------------------------------------------------
  mod_issues_server("issues", app_data, proj_root)

  # --- Help -------------------------------------------------------------------
  mod_help_server("help")
}

shinyApp(ui, server)
