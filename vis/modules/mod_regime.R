mod_regime_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        radioButtons(
          ns("show_type"),
          "Show",
          choices  = c("Net threat effect" = "net",
                       "Interaction only"  = "interaction",
                       "All components"    = "all"),
          selected = "net",
          inline   = TRUE
        )
      ),
      column(4,
        checkboxInput(ns("show_se"), "Show error bars (SE)", value = TRUE)
      )
    ),
    plotly::plotlyOutput(ns("regime_plot"), height = "420px"),
    hr(),
    DT::dataTableOutput(ns("regime_table")),
    downloadButton(ns("dl_regime"), "Download CSV")
  )
}

mod_regime_server <- function(id, regime_data) {
  moduleServer(id, function(input, output, session) {

    plot_df <- reactive({
      req(regime_data)
      df <- regime_data %>%
        dplyr::mutate(
          label = paste0("R", regime, ": ", label)
        )
      df
    })

    output$regime_plot <- plotly::renderPlotly({
      df <- plot_df()

      if (input$show_type == "net") {
        df <- df %>% dplyr::mutate(
          value  = net_coef,
          se_val = se_net,
          title  = "Net threat elasticity per regime (base + interaction)"
        )
      } else if (input$show_type == "interaction") {
        df <- df %>%
          dplyr::filter(regime > 1) %>%
          dplyr::mutate(
            value  = interaction_coef,
            se_val = NA_real_,
            title  = "Interaction term only (vs Regime 1 baseline)"
          )
      } else {
        df_long <- dplyr::bind_rows(
          df %>% dplyr::mutate(type = "Base", value = base_coef,        se_val = NA_real_),
          df %>% dplyr::filter(regime > 1) %>%
            dplyr::mutate(type = "Interaction", value = interaction_coef, se_val = NA_real_),
          df %>% dplyr::mutate(type = "Net", value = net_coef, se_val = se_net)
        )
        df <- df_long
      }

      regime_colours <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")
      names(regime_colours) <- unique(plot_df()$label)

      tip_col <- if (input$show_type == "all") "type" else "label"

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x    = label,
          y    = value,
          fill = if (input$show_type == "all") type else label,
          text = paste0(
            label, "\n",
            "Coef: ", round(value, 4), "\n",
            dplyr::if_else(!is.na(se_val), paste0("SE: ", round(se_val, 4)), "")
          )
        )
      ) +
        ggplot2::geom_col(alpha = 0.85, width = 0.6) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "grey40", linewidth = 0.6) +
        ggplot2::labs(
          title = "Regime-specific threat-defence elasticity (M4)",
          x     = NULL,
          y     = "Coefficient",
          fill  = NULL
        ) +
        theme_defence() +
        ggplot2::theme(legend.position = "bottom")

      if (isTRUE(input$show_se) && input$show_type == "net") {
        p <- p +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              ymin = value - se_val,
              ymax = value + se_val
            ),
            width     = 0.2,
            colour    = "grey30",
            linewidth = 0.7
          )
      }

      if (input$show_type != "all") {
        p <- p + ggplot2::scale_fill_manual(
          values = regime_colours,
          na.value = "grey70"
        )
      }

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(showlegend = (input$show_type == "all"))
    })

    output$regime_table <- DT::renderDataTable({
      df <- regime_data %>%
        dplyr::select(regime, label, base_coef, interaction_coef,
                      net_coef, se_net, p_interaction) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 4),
                    class   = "table-sm table-hover")
    })

    output$dl_regime <- downloadHandler(
      filename = "regime_effects.csv",
      content  = function(f) readr::write_csv(regime_data, f)
    )
  })
}
