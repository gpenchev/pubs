mod_coefficients_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        selectInput(
          ns("term_filter"),
          "Variable",
          choices  = c(
            "Threat (land log)"      = "threat_land_log",
            "GPR (M13)"              = "gpr_log",
            "Fiscal Deficit"         = "deficit_gdp",
            "Government Debt"        = "debt_gdp",
            "GDP Growth"             = "gdp_growth",
            "Immigration Rate"       = "immigration_rate",
            "Gov Left-Right"         = "gov_left_right",
            "Gov EU Position"        = "gov_eu_position",
            "Spatial lag (rho/lambda)" = "rho",
            "Regime 2 interaction"   = "threat_land_log:regime2",
            "Regime 3 interaction"   = "threat_land_log:regime3",
            "Regime 4 interaction"   = "threat_land_log:regime4"
          ),
          selected = "threat_land_log"
        )
      ),
      column(3,
        checkboxGroupInput(
          ns("model_types"),
          "Model types",
          choices  = c("OLS/FE" = "ols", "SAR/SEM" = "sar", "GPR" = "gpr"),
          selected = c("ols", "sar")
        )
      ),
      column(3,
        checkboxInput(ns("show_insig"), "Show non-significant", value = TRUE)
      ),
      column(3,
        checkboxInput(ns("show_ci"), "Show 95% CI", value = TRUE)
      )
    ),
    plotly::plotlyOutput(ns("forest_plot"), height = PLOT_HEIGHT_STANDARD),
    br(),
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span(bsicons::bs_icon("table"), " Filtered coefficients"),
        downloadButton(ns("dl_coef"), "CSV",
                       class = "btn btn-sm btn-outline-secondary")
      ),
      bslib::card_body(DT::dataTableOutput(ns("coef_table")))
    )
  )
}

mod_coefficients_server <- function(id, coef_data) {
  moduleServer(id, function(input, output, session) {

    filtered <- reactive({
      req(input$term_filter, coef_data)

      type_filter <- c()
      if ("ols" %in% input$model_types)
        type_filter <- c(type_filter, "M1", "M2", "M3", "M4")
      if ("sar" %in% input$model_types)
        type_filter <- c(type_filter, "M5", "M6", "M7", "M8", "M9",
                         "M10a", "M10b", "M10c", "M12")
      if ("gpr" %in% input$model_types)
        type_filter <- c(type_filter, "M13")

      df <- coef_data %>%
        dplyr::filter(term == input$term_filter) %>%
        dplyr::filter(grepl(
          paste0("^(", paste(type_filter, collapse = "|"), "):"),
          model
        ))

      if (!isTRUE(input$show_insig)) {
        df <- df %>% dplyr::filter(significant == TRUE)
      }
      df
    })

    output$forest_plot <- plotly::renderPlotly({
      df <- filtered()
      req(nrow(df) > 0)

      df <- df %>%
        dplyr::mutate(
          ci_lo  = estimate - 1.96 * std_error,
          ci_hi  = estimate + 1.96 * std_error,
          colour = dplyr::if_else(significant, "#2166ac", "#b2b2b2"),
          label  = model,
          tip    = paste0(
            model, "\n",
            "Coef: ", round(estimate, 4), "\n",
            "SE: ",   round(std_error, 4), "\n",
            "p: ",    round(p_value, 4)
          )
        ) %>%
        dplyr::arrange(dplyr::desc(estimate))

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x    = estimate,
          y    = stats::reorder(model, estimate),
          text = tip
        )
      ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "grey50", linewidth = 0.6) +
        ggplot2::geom_point(
          ggplot2::aes(colour = significant),
          size = 3
        ) +
        ggplot2::scale_colour_manual(
          values = c("TRUE" = "#2166ac", "FALSE" = "#aaaaaa"),
          labels = c("TRUE" = "p < 0.05",  "FALSE" = "p \u2265 0.05"),
          name   = ""
        ) +
        ggplot2::labs(
          title = paste("Coefficient:", input$term_filter),
          x     = "Estimate",
          y     = NULL
        ) +
        theme_defence()

      if (isTRUE(input$show_ci)) {
        p <- p +
          ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
            height = 0.25, colour = "grey60", linewidth = 0.5
          )
      }

      configure_plotly(
        plotly::ggplotly(p, tooltip = "text") %>%
          plotly::layout(legend = list(orientation = "h", y = -0.12)),
        fname = "coefficients_forest"
      )
    })

    output$coef_table <- DT::renderDataTable({
      df <- filtered() %>%
        dplyr::select(model, term, estimate, std_error, p_value, significant) %>%
        dplyr::mutate(
          estimate  = round(estimate,  4),
          std_error = round(std_error, 4),
          p_value   = round(p_value,   4)
        )
      rename_dt_cols(df) %>%
        DT::datatable(
          rownames  = FALSE,
          options   = list(pageLength = 15, dom = "tip"),
          class     = "table-sm table-hover"
        ) %>%
        DT::formatStyle(
          "Sig.",
          target          = "row",
          backgroundColor = DT::styleEqual(TRUE, "#d1ecf1")
        )
    })

    output$dl_coef <- downloadHandler(
      filename = function() paste0("coefficients_", input$term_filter, ".csv"),
      content  = function(f) readr::write_csv(filtered(), f)
    )
  })
}
