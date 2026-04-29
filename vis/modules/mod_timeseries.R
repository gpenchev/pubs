mod_timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        selectInput(
          ns("variable"),
          "Variable",
          choices  = get_variable_labels(),
          selected = "defence_gdp"
        )
      ),
      column(4,
        selectInput(
          ns("countries"),
          "Countries",
          choices  = NULL,
          multiple = TRUE
        )
      ),
      column(3,
        sliderInput(
          ns("years"),
          "Year range",
          min   = 1995,
          max   = 2023,
          value = c(1995, 2023),
          step  = 1,
          sep   = ""
        )
      ),
      column(1,
        checkboxInput(ns("show_regimes"), "Regime bands", value = TRUE)
      ),
      column(1,
        checkboxInput(ns("show_source"), "Source overlay", value = FALSE)
      )
    ),
    plotly::plotlyOutput(ns("plot"), height = "500px")
  )
}

mod_timeseries_server <- function(id, panel_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      labs <- get_country_labels(panel_data)
      updateSelectInput(session, "countries",
                        choices  = labs,
                        selected = names(labs))
    })

    plot_data <- reactive({
      req(input$variable, input$years)
      countries <- if (length(input$countries) == 0) NULL else input$countries
      filter_panel(panel_data, countries, input$years, input$variable)
    })

    output$plot <- plotly::renderPlotly({
      df  <- plot_data()
      var <- input$variable
      req(nrow(df) > 0)

      lab <- names(get_variable_labels())[get_variable_labels() == var]
      if (length(lab) == 0) lab <- var

      # Source overlay is only meaningful for defence_gdp (which has a
      # defence_source column). For other variables, fall back to colouring
      # by country to avoid errors from missing source columns.
      use_source_overlay <- isTRUE(input$show_source) &&
                            var == "defence_gdp" &&
                            "defence_source" %in% names(df)

      colour_aes <- if (use_source_overlay) "defence_source" else "country"

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "Country: ", country_name, "<br>",
            "Year: ", year, "<br>",
            lab, ": ", round(.data[[var]], 3), "<br>",
            "Regime: ", regime, "<br>",
            "Source: ", defence_source
          )
        )

      gg <- ggplot2::ggplot(df, ggplot2::aes(
        x      = year,
        y      = .data[[var]],
        colour = .data[[colour_aes]],
        group  = country,
        text   = .tooltip
      )) +
        ggplot2::geom_line(linewidth = 0.7, alpha = 0.8) +
        ggplot2::geom_point(size = 1.2, alpha = 0.6) +
        ggplot2::labs(
          title  = lab,
          x      = "Year",
          y      = lab,
          colour = if (use_source_overlay) "Source" else "Country"
        ) +
        theme_defence()

      if (isTRUE(input$show_regimes)) {
        gg <- add_regime_shading(gg)
      }

      if (use_source_overlay) {
        gg <- gg + ggplot2::scale_colour_manual(values = palette_source)
      }

      plotly::ggplotly(gg, tooltip = "text") %>%
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.5)
        )
    })
  })
}
