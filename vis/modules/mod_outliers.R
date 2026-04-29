mod_outliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        selectInput(ns("variable"), "Variable",
                    choices  = get_variable_labels(),
                    selected = "defence_gdp")
      ),
      column(3,
        radioButtons(ns("method"), "Method",
                     choices  = c("IQR"               = "iqr",
                                  "Year-on-year jump" = "yoy",
                                  "Both"              = "both"),
                     selected = "iqr",
                     inline   = TRUE)
      ),
      column(3,
        selectInput(ns("countries"), "Countries",
                    choices  = NULL,
                    multiple = TRUE)
      ),
      column(2,
        numericInput(ns("multiplier"), "IQR multiplier",
                     value = 3, min = 1, max = 10, step = 0.5)
      )
    ),
    plotly::plotlyOutput(ns("plot"), height = "420px"),
    hr(),
    DT::dataTableOutput(ns("table"))
  )
}

mod_outliers_server <- function(id, panel_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      labs <- get_country_labels(panel_data)
      updateSelectInput(session, "countries",
                        choices  = labs,
                        selected = names(labs))
    })

    base_data <- reactive({
      req(input$variable)
      countries <- if (length(input$countries) == 0) NULL else input$countries
      filter_panel(panel_data, countries, NULL, NULL) %>%
        dplyr::filter(!is.na(.data[[input$variable]]))
    })

    flagged <- reactive({
      get_outlier_table(base_data(), input$variable,
                        method     = input$method,
                        multiplier = input$multiplier)
    })

    output$plot <- plotly::renderPlotly({
      df  <- base_data()
      var <- input$variable
      req(nrow(df) > 0)

      var_lab <- names(get_variable_labels())[get_variable_labels() == var]
      if (length(var_lab) == 0) var_lab <- var

      df <- df %>%
        dplyr::mutate(
          iqr_flag = flag_iqr_outliers(.data[[var]], input$multiplier)
        )

      if (input$method %in% c("yoy", "both")) {
        df <- flag_yoy_outliers(df, var) %>%
          dplyr::mutate(
            flag = dplyr::case_when(
              input$method == "yoy"  ~ yoy_flag,
              input$method == "both" ~ iqr_flag | yoy_flag,
              TRUE                   ~ iqr_flag
            )
          )
      } else {
        df <- df %>% dplyr::mutate(flag = iqr_flag)
      }

      # Source label mapping: only defence_gdp, fiscal, and GDP variables
      # have a meaningful source column. Threat score variables and political
      # variables do not have a source column and will show "unknown".
      source_col <- dplyr::case_when(
        var == "defence_gdp"                  ~ "defence_source",
        var %in% c("debt_gdp", "deficit_gdp") ~ "fiscal_source",
        var %in% c("gdp_pc", "gdp_growth")    ~ "gdp_source",
        TRUE                                   ~ NA_character_
      )

      has_source <- !is.na(source_col) && source_col %in% names(df)

      df <- df %>%
        dplyr::mutate(
          .source_label = if (has_source) .data[[source_col]] else "unknown",
          .tooltip = paste0(
            "Country: ", country_name, "<br>",
            "Year: ", year, "<br>",
            var_lab, ": ", round(.data[[var]], 3), "<br>",
            "Source: ", .source_label, "<br>",
            "Flagged: ", flag
          )
        )

      gg <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = year, y = .data[[var]], group = country)
      ) +
        ggplot2::geom_line(colour = "grey70", linewidth = 0.5) +
        ggplot2::geom_point(
          ggplot2::aes(colour = flag, text = .tooltip),
          size = 2
        ) +
        ggplot2::scale_colour_manual(
          values = palette_flag,
          labels = c("FALSE" = "Normal", "TRUE" = "Flagged"),
          name   = ""
        ) +
        ggplot2::labs(
          title = paste("Outlier inspection \u2014", var_lab),
          x     = "Year",
          y     = var_lab
        ) +
        theme_defence()

      gg <- add_regime_shading(gg)

      plotly::ggplotly(gg, tooltip = "text") %>%
        plotly::layout(
          legend = list(orientation = "h", x = 0, y = -0.15)
        )
    })

    output$table <- DT::renderDataTable({
      flagged() %>%
        dplyr::arrange(dplyr::desc(.data[[input$variable]])) %>%
        DT::datatable(
          options  = list(pageLength = 15, scrollX = TRUE),
          rownames = FALSE
        )
    })
  })
}
