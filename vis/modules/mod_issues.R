mod_issues_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("issue"),
      "Select issue",
      choices = c(
        "Issue 1 \u2014 Kinetic bias (Crimea 2014)"            = "kinetic",
        "Issue 2 \u2014 50km sea threshold (Greece)"           = "threshold",
        "Issue 3 \u2014 Regime 1 data truncation"              = "truncation",
        "Issue 4 \u2014 GPR comparison sample bias"            = "gpr_bias"
      ),
      selected = "kinetic"
    ),
    br(),
    # Accordion with three panels: Figure, Table, Research Notes
    bslib::accordion(
      open     = "fig_panel",
      multiple = TRUE,

      bslib::accordion_panel(
        title = tagList(bsicons::bs_icon("graph-up"), " Figure"),
        value = "fig_panel",
        plotly::plotlyOutput(ns("issue_plot"), height = PLOT_HEIGHT_STANDARD)
      ),

      bslib::accordion_panel(
        title = tagList(bsicons::bs_icon("table"), " Table"),
        value = "tbl_panel",
        br(),
        DT::dataTableOutput(ns("issue_table")),
        br(),
        downloadButton(ns("dl_issue"), "Download CSV")
      ),

      bslib::accordion_panel(
        title = tagList(bsicons::bs_icon("journal-text"), " Research Notes"),
        value = "notes_panel",
        br(),
        uiOutput(ns("issue_text"))
      )
    )
  )
}

mod_issues_server <- function(id, app_data, proj_root) {
  moduleServer(id, function(input, output, session) {

    # --- Figure ---------------------------------------------------------------
    output$issue_plot <- plotly::renderPlotly({

      if (input$issue == "kinetic") {
        df <- app_data[["app_issue1_crimea"]]
        req(!is.null(df), nrow(df) > 0)

        df <- df %>%
          dplyr::group_by(country) %>%
          dplyr::mutate(
            threat_n = (threat_land_log - min(threat_land_log, na.rm = TRUE)) /
              (max(threat_land_log, na.rm = TRUE) - min(threat_land_log, na.rm = TRUE) + 1e-9),
            gpr_n    = (gpr_mean - min(gpr_mean, na.rm = TRUE)) /
              (max(gpr_mean, na.rm = TRUE) - min(gpr_mean, na.rm = TRUE) + 1e-9)
          ) %>%
          dplyr::ungroup()

        avg <- dplyr::bind_rows(
          df %>% dplyr::select(country, year, value = threat_n) %>%
            dplyr::mutate(series = "UCDP threat (normalised)"),
          df %>% dplyr::select(country, year, value = gpr_n) %>%
            dplyr::mutate(series = "GPR index (normalised)")
        ) %>%
          dplyr::group_by(year, series) %>%
          dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

        p <- ggplot2::ggplot(avg, ggplot2::aes(
          x = year, y = value, colour = series, group = series
        )) +
          ggplot2::geom_line(linewidth = 1.1) +
          ggplot2::geom_point(size = 2.5) +
          ggplot2::geom_vline(xintercept = 2014, linetype = "dashed",
                              colour = "red", linewidth = 0.8) +
          ggplot2::annotate("text", x = 2014.1, y = 0.95,
                            label = "Crimea\nannexation", colour = "red",
                            hjust = 0, size = 3.5) +
          ggplot2::scale_colour_manual(
            values = c("UCDP threat (normalised)" = "#2166ac",
                       "GPR index (normalised)"   = "#d73027"),
            name   = NULL
          ) +
          ggplot2::labs(
            title    = "UCDP threat vs GPR index around 2014 Crimea shock (13-country mean)",
            subtitle = "GPR spikes in 2014; UCDP barely moves — kinetic bias confirmed",
            x = "Year", y = "Normalised index (0-1)"
          ) +
          theme_defence()

        configure_plotly(
          plotly::ggplotly(p) %>%
            plotly::layout(legend = list(orientation = "h", y = -0.12)),
          fname = "issue_crimea_kinetic"
        )

      } else if (input$issue == "threshold") {
        df <- app_data[["app_issue2_greece"]]
        req(!is.null(df))

        df_long <- dplyr::bind_rows(
          df %>% dplyr::transmute(year, value = gr_defence,   series = "Greece defence (% GDP)"),
          df %>% dplyr::transmute(year, value = mean_defence, series = "Sample mean defence"),
          df %>% dplyr::transmute(year, value = gr_threat,    series = "Greece UCDP threat"),
          df %>% dplyr::transmute(year, value = mean_threat,  series = "Sample mean threat")
        )

        p <- ggplot2::ggplot(df_long,
                             ggplot2::aes(x = year, y = value,
                                          colour = series, group = series)) +
          ggplot2::geom_line(linewidth = 0.9) +
          ggplot2::geom_point(size = 1.8) +
          ggplot2::scale_colour_manual(values = c(
            "Greece defence (% GDP)" = "#d73027",
            "Sample mean defence"    = "#fc8d59",
            "Greece UCDP threat"     = "#2166ac",
            "Sample mean threat"     = "#74add1"
          ), name = NULL) +
          ggplot2::labs(
            title    = "Greece: defence spending vs UCDP land threat (50km threshold)",
            subtitle = "Greece spends far above average; UCDP threat near-average (Aegean threats excluded)",
            x = "Year", y = "Value"
          ) +
          theme_defence()

        configure_plotly(
          plotly::ggplotly(p) %>%
            plotly::layout(legend = list(orientation = "h", y = -0.12)),
          fname = "issue_greece_threshold"
        )

      } else if (input$issue == "truncation") {
        df <- app_data[["app_issue3_coverage"]]
        req(!is.null(df))

        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = year, y = country, fill = has_immigration)
        ) +
          ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
          ggplot2::scale_fill_manual(
            values = c("TRUE" = "#2166ac", "FALSE" = "#f7f7f7"),
            labels = c("TRUE" = "Available", "FALSE" = "Missing"),
            name   = "Immigration data"
          ) +
          ggplot2::geom_vline(xintercept = c(1999.5, 2004.5),
                              linetype = "dashed", colour = "red", linewidth = 0.6) +
          ggplot2::annotate("text", x = 1997, y = 1.5,
                            label = "Regime 1\ngap", colour = "red", size = 3) +
          ggplot2::labs(
            title    = "Immigration data coverage by country and year",
            subtitle = "Red dashes = Regime 1 boundaries — 1995-1999 fully missing",
            x = "Year", y = NULL
          ) +
          theme_defence() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))

        configure_plotly(plotly::ggplotly(p), fname = "issue_coverage_truncation")

      } else {
        df <- app_data[["app_issue4_gpr_coverage"]]
        req(!is.null(df))

        df <- df %>%
          dplyr::mutate(
            coverage = dplyr::if_else(in_gpr, "GPR covered", "GPR missing")
          ) %>%
          dplyr::arrange(region, dplyr::desc(mean_threat))

        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x    = stats::reorder(country, mean_threat),
            y    = mean_threat,
            fill = coverage
          )
        ) +
          ggplot2::geom_col(alpha = 0.85) +
          ggplot2::scale_fill_manual(
            values = c("GPR covered" = "#2166ac", "GPR missing" = "#d73027"),
            name   = NULL
          ) +
          ggplot2::facet_wrap(~region, scales = "free_x", nrow = 1) +
          ggplot2::labs(
            title    = "GPR coverage vs mean UCDP threat by country",
            subtitle = "Red = missing from GPR comparison; highest-threat Eastern EU countries are all missing",
            x = NULL, y = "Mean threat_land_log (1995-2023)"
          ) +
          theme_defence() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8))

        configure_plotly(
          plotly::ggplotly(p) %>%
            plotly::layout(legend = list(orientation = "h", y = -0.12)),
          fname = "issue_gpr_coverage"
        )
      }
    })

    # --- Table ----------------------------------------------------------------
    output$issue_table <- DT::renderDataTable({
      df <- switch(input$issue,
        kinetic    = app_data[["app_gpr_correlation"]],
        threshold  = {
          cfe <- app_data[["app_country_fe"]]
          if (!is.null(cfe))
            cfe %>%
              dplyr::filter(country %in% c("GR", "IT", "ES", "PT", "HR")) %>%
              dplyr::mutate(note = "Southern/Mediterranean EU countries")
          else NULL
        },
        truncation = {
          cov <- app_data[["app_issue3_coverage"]]
          if (!is.null(cov))
            cov %>%
              dplyr::group_by(country) %>%
              dplyr::summarise(
                years_available = sum(has_immigration, na.rm = TRUE),
                years_missing   = sum(!has_immigration, na.rm = TRUE),
                first_year      = min(year[has_immigration == TRUE], na.rm = TRUE),
                .groups         = "drop"
              )
          else NULL
        },
        gpr_bias   = app_data[["app_issue4_gpr_coverage"]]
      )
      if (is.null(df))
        return(DT::datatable(data.frame(message = "Data unavailable")))
      rename_dt_cols(df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))) %>%
        DT::datatable(rownames = FALSE,
                      options  = list(pageLength = 15, dom = "tip"),
                      class    = "table-sm table-hover")
    })

    output$dl_issue <- downloadHandler(
      filename = function() paste0("issue_", input$issue, ".csv"),
      content  = function(f) {
        df <- switch(input$issue,
          kinetic    = app_data[["app_issue1_crimea"]],
          threshold  = app_data[["app_issue2_greece"]],
          truncation = app_data[["app_issue3_coverage"]],
          gpr_bias   = app_data[["app_issue4_gpr_coverage"]]
        )
        readr::write_csv(df, f)
      }
    )

    # --- Research Notes -------------------------------------------------------
    output$issue_text <- renderUI({
      spec <- switch(input$issue,
        kinetic    = list(file = "methodology/models/weak.md",    heading = "## 9."),
        threshold  = list(file = "methodology/models/weak.md",    heading = "## 10."),
        truncation = list(file = "models/results/m1_m12.md",      heading = "## 1."),
        gpr_bias   = list(file = "models/results/gpr_results.md", heading = "## 7.")
      )
      content <- tryCatch(
        read_md_section(proj_root, spec$file, spec$heading),
        error = function(e) "*Content not available.*"
      )
      shiny::markdown(content)
    })
  })
}
