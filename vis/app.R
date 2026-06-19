library(shiny)
library(here)

# When deployed on Shiny Server, here() resolves to the vis/ directory itself.
# Use a plain relative path so it works both locally (cwd = vis/) and deployed.
source("global.R")

# =============================================================================
# Helper: build a standard 4-sub-tab layout (Figures / Tables / Maps / Results)
# =============================================================================
make_tab_ui <- function(
    figures_ui,
    tables_ui,
    maps_ui    = NULL,
    results_id,
    maps_label = "Maps"
) {
  tabsetPanel(
    type = "pills",
    tabPanel("Figures",  br(), figures_ui),
    tabPanel("Tables",   br(), tables_ui),
    tabPanel(maps_label,
      br(),
      if (!is.null(maps_ui)) maps_ui
      else tags$p(class = "text-muted",
                  bsicons::bs_icon("map"), " No map available for this section.")
    ),
    tabPanel("Results",  br(), results_id)
  )
}

# =============================================================================
# UI
# =============================================================================
ui <- navbarPage(
  title = tags$span(
    bsicons::bs_icon("shield-shaded"), " EU Defence Panel"
  ),
  theme    = bslib::bs_theme(bootswatch = "flatly", version = 5),
  selected = "About",
  collapsible = TRUE,

  # --------------------------------------------------------------------------
  # TAB 0 — About
  # --------------------------------------------------------------------------
  tabPanel(
    "About",
    mod_about_ui("about")
  ),

  # --------------------------------------------------------------------------
  # TAB 1 — Threat Index
  # --------------------------------------------------------------------------
  tabPanel(
    "Threat Index",
    make_tab_ui(
      figures_ui = tagList(
        tabsetPanel(
          type = "tabs",
          tabPanel("Time Series",
            br(), mod_timeseries_ui("threat_ts")
          ),
          tabPanel("UCDP vs GPR scatter",
            br(), mod_scatter_ui("threat_scatter")
          )
        )
      ),
      tables_ui = tagList(
        h5("Country threat summary"),
        DT::dataTableOutput("threat_country_tbl"),
        downloadButton("dl_threat_country", "Download CSV"),
        hr(),
        h5("GPR correlation by country"),
        DT::dataTableOutput("gpr_corr_tbl"),
        downloadButton("dl_gpr_corr", "Download CSV")
      ),
      maps_ui = mod_map_ui("threat_map"),
      results_id = mod_results_ui("threat_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 2 — Panel Estimation
  # --------------------------------------------------------------------------
  tabPanel(
    "Panel Estimation",
    make_tab_ui(
      figures_ui = tagList(
        tabsetPanel(
          type = "tabs",
          tabPanel("Coefficient forest plot",
            br(), mod_coefficients_ui("coef_plot")
          ),
          tabPanel("Regime effects",
            br(), mod_regime_ui("regime_chart")
          ),
          tabPanel("Spatial lag comparison",
            br(),
            DT::dataTableOutput("rho_comparison_tbl"),
            downloadButton("dl_rho", "Download CSV")
          )
        )
      ),
      tables_ui = tagList(
        h5("Full model fit table (M1–M13)"),
        DT::dataTableOutput("model_fit_tbl"),
        downloadButton("dl_model_fit", "Download CSV"),
        hr(),
        h5("Country fixed effects (M5 SAR)"),
        DT::dataTableOutput("country_fe_tbl"),
        downloadButton("dl_country_fe", "Download CSV"),
        hr(),
        h5("Unit root tests"),
        mod_unitroot_ui("unitroot")
      ),
      maps_ui = tagList(
        h5("Country fixed effects map (M5 SAR)"),
        tags$p(class="text-muted",
               "Higher values = country spends more than the model predicts from threat + fiscal variables alone."),
        mod_map_ui("est_map")
      ),
      results_id = mod_results_ui("est_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 3 — Robustness Checks
  # --------------------------------------------------------------------------
  tabPanel(
    "Robustness Checks",
    make_tab_ui(
      figures_ui = tagList(
        tabsetPanel(
          type = "tabs",
          tabPanel("Check I — 2022/23 cross-section",
            br(),
            mod_scatter_ui("checkI_scatter")
          ),
          tabPanel("Check H — BG 2019 sensitivity",
            br(),
            plotly::plotlyOutput("checkH_plot", height = "380px")
          ),
          tabPanel("VIF",
            br(),
            plotly::plotlyOutput("vif_plot", height = "380px")
          ),
          tabPanel("Outliers / Cook's D",
            br(),
            mod_outliers_ui("outliers")
          )
        )
      ),
      tables_ui = tagList(
        h5("All checks A–J summary"),
        DT::dataTableOutput("checks_summary_tbl"),
        downloadButton("dl_checks", "Download CSV"),
        hr(),
        h5("Check I — Cross-section 2022/2023"),
        DT::dataTableOutput("check_i_tbl"),
        hr(),
        h5("Check H — BG 2019 sensitivity"),
        DT::dataTableOutput("check_h_tbl"),
        hr(),
        h5("Check J — Immigration interaction"),
        DT::dataTableOutput("check_j_tbl"),
        downloadButton("dl_check_j", "Download CSV"),
        hr(),
        h5("Regime LR tests"),
        DT::dataTableOutput("regime_lr_tbl")
      ),
      maps_ui = tagList(
        h5("Cook's distance by country (M5 SAR)"),
        tags$p(class="text-muted",
               "Choropleth of max Cook's D per country — shows which countries most influence results."),
        mod_map_ui("rob_map")
      ),
      results_id = mod_results_ui("rob_results")
    )
  ),

  # --------------------------------------------------------------------------
  # TAB 4 — Specific Issues
  # --------------------------------------------------------------------------
  tabPanel(
    "Specific Issues",
    fluidPage(
      br(),
      mod_issues_ui("issues")
    )
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

  # Scatter: threat_land_log vs gpr_mean using gpr_panel data
  gpr_panel_for_scatter <- if (!is.null(app_data[["app_gpr_panel"]])) {
    app_data[["app_gpr_panel"]] %>%
      # patch to match filter_panel expectations
      dplyr::mutate(
        country_name   = country,
        defence_source = "GPR data"
      )
  } else panel_data

  mod_scatter_server("threat_scatter", panel_data)

  mod_map_server("threat_map",
                 panel_data    = panel_data,
                 ged_events    = conflict_events)

  output$threat_country_tbl <- DT::renderDataTable({
    df <- app_data[["app_threat_country"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,3))),
                  rownames=FALSE, options=list(pageLength=12, dom="tip"),
                  class="table-sm table-hover")
  })
  output$dl_threat_country <- downloadHandler(
    filename = "threat_country_summary.csv",
    content  = function(f) readr::write_csv(app_data[["app_threat_country"]], f)
  )

  output$gpr_corr_tbl <- DT::renderDataTable({
    df <- app_data[["app_gpr_correlation"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,3))),
                  rownames=FALSE, options=list(pageLength=13, dom="t"),
                  class="table-sm table-hover")
  })
  output$dl_gpr_corr <- downloadHandler(
    filename = "gpr_correlation.csv",
    content  = function(f) readr::write_csv(app_data[["app_gpr_correlation"]], f)
  )

  mod_results_server("threat_results", proj_root, list(
    list(label="Threat Index Results",  file="models/results/threat_index.md",   heading="full"),
    list(label="Methodology",           file="methodology/models/threat.md",      heading="full"),
    list(label="Variables",             file="methodology/models/variables.md",   heading="full")
  ))

  # --- Panel Estimation -------------------------------------------------------
  mod_coefficients_server("coef_plot",    app_data[["app_coef_long"]])
  mod_regime_server("regime_chart",       app_data[["app_regime_effects"]])
  mod_unitroot_server("unitroot",         unitroot_results)

  output$rho_comparison_tbl <- DT::renderDataTable({
    df <- app_data[["app_rho_comparison"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,4))),
                  rownames=FALSE, options=list(dom="t", pageLength=12),
                  class="table-sm table-hover")
  })
  output$dl_rho <- downloadHandler(
    filename = "rho_comparison.csv",
    content  = function(f) readr::write_csv(app_data[["app_rho_comparison"]], f)
  )

  output$model_fit_tbl <- DT::renderDataTable({
    df <- app_data[["app_model_fit"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,3))),
                  rownames=FALSE, options=list(dom="t", pageLength=14),
                  class="table-sm table-hover")
  })
  output$dl_model_fit <- downloadHandler(
    filename = "model_fit.csv",
    content  = function(f) readr::write_csv(app_data[["app_model_fit"]], f)
  )

  output$country_fe_tbl <- DT::renderDataTable({
    df <- app_data[["app_country_fe"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,4))),
                  rownames=FALSE, options=list(dom="t", pageLength=23),
                  class="table-sm table-hover")
  })
  output$dl_country_fe <- downloadHandler(
    filename = "country_fixed_effects.csv",
    content  = function(f) readr::write_csv(app_data[["app_country_fe"]], f)
  )

  # Map: country FE as choropleth — patch panel_data with fe_value
  country_fe_panel <- tryCatch({
    fe <- app_data[["app_country_fe"]]
    if (is.null(fe) || is.null(panel_data)) panel_data
    else {
      panel_data %>%
        dplyr::left_join(fe %>% dplyr::select(country, country_fe = fe_value),
                         by = "country") %>%
        dplyr::mutate(country_fe = dplyr::coalesce(country_fe, 0))
    }
  }, error = function(e) panel_data)

  mod_map_server("est_map",
                 panel_data = country_fe_panel,
                 ged_events = NULL)

  mod_results_server("est_results", proj_root, list(
    list(label="M1–M13 Results",  file="models/results/m1_m12.md",        heading="full"),
    list(label="Model Methods",   file="methodology/models/models.md",     heading="full"),
    list(label="Plain language",  file="methodology/models/naive.md",      heading="full")
  ))

  # --- Robustness Checks ------------------------------------------------------
  # Check I scatter: threat vs defence in 2022 from panel_data
  mod_scatter_server("checkI_scatter", panel_data)

  output$checkH_plot <- plotly::renderPlotly({
    df <- app_data[["app_check_h"]]
    req(!is.null(df))
    p <- ggplot2::ggplot(df, ggplot2::aes(x=comparison)) +
      ggplot2::geom_segment(
        ggplot2::aes(x=comparison, xend=comparison,
                     y=coef_full, yend=coef_no_bg19),
        colour="grey60", linewidth=0.8
      ) +
      ggplot2::geom_point(ggplot2::aes(y=coef_full,    colour="Full sample"), size=3.5) +
      ggplot2::geom_point(ggplot2::aes(y=coef_no_bg19, colour="Without BG 2019"), size=3.5) +
      ggplot2::scale_colour_manual(
        values = c("Full sample"="#2166ac", "Without BG 2019"="#d73027"), name=NULL
      ) +
      ggplot2::labs(
        title="Check H: Bulgaria 2019 sensitivity",
        subtitle="All changes within 1 SE — results STABLE",
        x=NULL, y="Coefficient"
      ) +
      ggplot2::coord_flip() +
      theme_defence()
    plotly::ggplotly(p) %>% plotly::layout(legend=list(orientation="h", y=-0.2))
  })

  output$vif_plot <- plotly::renderPlotly({
    df <- app_data[["app_vif"]]
    req(!is.null(df), nrow(df) > 0)
    # CSV columns: term, vif, vif_adj, flag
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x    = stats::reorder(term, vif),
      y    = vif,
      fill = flag
    )) +
      ggplot2::geom_col(alpha=0.85) +
      ggplot2::geom_hline(yintercept=10, linetype="dashed", colour="red") +
      ggplot2::scale_fill_manual(
        values=c("OK"="#2166ac","SEVERE"="#d73027","MODERATE"="#f28e2b"), name=NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(title="VIF — Variance Inflation Factors", x=NULL, y="VIF") +
      theme_defence()
    plotly::ggplotly(p) %>% plotly::layout(legend=list(orientation="h", y=-0.2))
  })

  mod_outliers_server("outliers", panel_data)

  output$checks_summary_tbl <- DT::renderDataTable({
    df <- app_data[["app_checks_summary"]]
    if (is.null(df)) return(DT::datatable(data.frame()))
    DT::datatable(df, rownames=FALSE, options=list(dom="t", pageLength=12),
                  class="table-sm")
  })
  output$dl_checks <- downloadHandler(
    filename = "revision_checks_summary.csv",
    content  = function(f) readr::write_csv(app_data[["app_checks_summary"]], f)
  )

  render_simple_tbl <- function(key) {
    DT::renderDataTable({
      df <- app_data[[key]]
      if (is.null(df)) return(DT::datatable(data.frame()))
      DT::datatable(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x,4))),
                    rownames=FALSE, options=list(dom="t"), class="table-sm table-hover")
    })
  }
  output$check_i_tbl  <- render_simple_tbl("app_check_i")
  output$check_h_tbl  <- render_simple_tbl("app_check_h")
  output$check_j_tbl  <- render_simple_tbl("app_check_j")
  output$regime_lr_tbl <- render_simple_tbl("app_regime_lr")
  output$dl_check_j <- downloadHandler(
    filename = "check_j_immigration_interaction.csv",
    content  = function(f) readr::write_csv(app_data[["app_check_j"]], f)
  )

  # Robustness map: Cook's D choropleth
  rob_panel <- tryCatch({
    inf <- app_data[["app_influence_country"]]
    if (is.null(inf) || is.null(panel_data)) panel_data
    else {
      panel_data %>%
        dplyr::left_join(
          inf %>% dplyr::select(country, max_cooks_d),
          by = "country"
        ) %>%
        dplyr::mutate(max_cooks_d = dplyr::coalesce(max_cooks_d, 0))
    }
  }, error = function(e) panel_data)

  mod_map_server("rob_map", panel_data = rob_panel, ged_events = NULL)

  mod_results_server("rob_results", proj_root, list(
    list(label="Diagnostics & Breaks", file="models/results/diagnostics_breaks.md", heading="full"),
    list(label="Check I & J",          file="models/results/revision_checks_ij.md", heading="full"),
    list(label="All Weaknesses",       file="methodology/models/weak.md",           heading="full")
  ))

  # --- Specific Issues --------------------------------------------------------
  mod_issues_server("issues", app_data, proj_root)
}

shinyApp(ui, server)
