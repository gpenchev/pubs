mod_scatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        selectInput(ns("x_var"), "X variable",
                    choices  = get_variable_labels(),
                    selected = "gdp_pc")
      ),
      column(3,
        selectInput(ns("y_var"), "Y variable",
                    choices  = get_variable_labels(),
                    selected = "defence_gdp")
      ),
      column(2,
        radioButtons(ns("colour_by"), "Colour by",
                     choices  = c("Regime"  = "regime",
                                  "Country" = "country",
                                  "Source"  = "defence_source"),
                     selected = "regime")
      ),
      column(2,
        sliderInput(ns("year"), "Year",
                    min   = 1995,
                    max   = 2023,
                    value = 2022,
                    step  = 1,
                    sep   = "")
      ),
      column(1,
        checkboxInput(ns("fit_line"), "Fit line", value = FALSE)
      ),
      column(1,
        checkboxInput(ns("show_labels"), "Labels", value = TRUE)
      )
    ),
    plotly::plotlyOutput(ns("plot"), height = "500px")
  )
}

mod_scatter_server <- function(id, panel_data) {
  moduleServer(id, function(input, output, session) {

    plot_data <- reactive({
      req(input$x_var, input$y_var, input$year)

      cols <- unique(c("country", "country_name", "year", "regime",
                       "defence_source", input$x_var, input$y_var))

      panel_data %>%
        dplyr::filter(year == input$year) %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::filter(!is.na(.data[[input$x_var]]),
                      !is.na(.data[[input$y_var]]))
    })

    output$plot <- plotly::renderPlotly({
      df <- plot_data()
      req(nrow(df) > 0)

      colour_col <- input$colour_by
      if (colour_col == "regime") {
        df <- df %>% dplyr::mutate(regime = as.character(regime))
      }

      x_lab <- names(get_variable_labels())[get_variable_labels() == input$x_var]
      y_lab <- names(get_variable_labels())[get_variable_labels() == input$y_var]
      if (length(x_lab) == 0) x_lab <- input$x_var
      if (length(y_lab) == 0) y_lab <- input$y_var

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "Country: ", country_name, "<br>",
            x_lab, ": ", round(.data[[input$x_var]], 3), "<br>",
            y_lab, ": ", round(.data[[input$y_var]], 3), "<br>",
            "Regime: ", regime
          )
        )

      gg <- ggplot2::ggplot(df, ggplot2::aes(
        x      = .data[[input$x_var]],
        y      = .data[[input$y_var]],
        colour = .data[[colour_col]],
        text   = .tooltip
      )) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::labs(
          title  = paste(input$year, "\u2014", x_lab, "vs", y_lab),
          x      = x_lab,
          y      = y_lab,
          colour = colour_col
        ) +
        theme_defence()

      if (isTRUE(input$show_labels)) {
        gg <- gg + ggplot2::geom_text(
          ggplot2::aes(label = country),
          size   = 3,
          colour = "grey20",
          vjust  = -0.8
        )
      }

      if (isTRUE(input$fit_line)) {
        gg <- gg + ggplot2::geom_smooth(
          method      = "lm",
          se          = TRUE,
          colour      = "grey30",
          linewidth   = 0.8,
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data[[input$x_var]],
            y = .data[[input$y_var]]
          )
        )
      }

      if (colour_col == "regime") {
        gg <- gg + scale_colour_regime()
      } else if (colour_col == "defence_source") {
        gg <- gg + ggplot2::scale_colour_manual(values = palette_source)
      }

      plotly::ggplotly(gg, tooltip = "text") %>%
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.5)
        )
    })
  })
}
