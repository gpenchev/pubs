mod_unitroot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        radioButtons(ns("test"), "Test",
                     choices  = c("ADF"  = "adf",
                                  "KPSS" = "kpss",
                                  "Both" = "both"),
                     selected = "both",
                     inline   = TRUE)
      ),
      column(4,
        radioButtons(ns("alpha"), "Significance level",
                     choices  = c("5%" = "0.05", "10%" = "0.10"),
                     selected = "0.05",
                     inline   = TRUE)
      )
    ),
    plotly::plotlyOutput(ns("heatmap"), height = "420px"),
    hr(),
    h4("Summary: % stationary per variable"),
    DT::dataTableOutput(ns("summary_table"))
  )
}

mod_unitroot_server <- function(id, unitroot_results) {
  moduleServer(id, function(input, output, session) {

    processed <- reactive({
      # req() handles the case where unitroot_results failed to load in global.R
      req(!is.null(unitroot_results))
      alpha <- as.numeric(input$alpha)

      df <- unitroot_results %>%
        dplyr::mutate(
          adf_stationary = !is.na(adf_pval)  & adf_pval  < alpha,
          kpss_nonstat   = !is.na(kpss_pval) & kpss_pval < alpha
        )

      if (input$test == "adf") {
        df <- df %>% dplyr::mutate(
          status = dplyr::case_when(
            is.na(adf_pval)  ~ "missing",
            adf_stationary   ~ "stationary",
            TRUE             ~ "unit_root"
          )
        )
      } else if (input$test == "kpss") {
        df <- df %>% dplyr::mutate(
          status = dplyr::case_when(
            is.na(kpss_pval) ~ "missing",
            kpss_nonstat     ~ "unit_root",
            TRUE             ~ "stationary"
          )
        )
      } else {
        df <- df %>% dplyr::mutate(
          status = dplyr::case_when(
            is.na(adf_pval) | is.na(kpss_pval) ~ "missing",
            adf_stationary  & !kpss_nonstat     ~ "stationary",
            !adf_stationary & kpss_nonstat      ~ "unit_root",
            TRUE                                ~ "mixed"
          )
        )
      }
      df
    })

    output$heatmap <- plotly::renderPlotly({
      df <- processed()
      req(nrow(df) > 0)

      status_num <- c(
        "stationary" = 0,
        "mixed"      = 0.33,
        "unit_root"  = 0.66,
        "missing"    = 1.0
      )

      status_labels <- c(
        "stationary" = "S",
        "mixed"      = "M",
        "unit_root"  = "U",
        "missing"    = "?"
      )

      df <- df %>%
        dplyr::mutate(
          status_num   = status_num[status],
          status_label = status_labels[status],
          adf_pval_r   = round(adf_pval,  3),
          kpss_pval_r  = round(kpss_pval, 3),
          tooltip_text = paste0(
            "Country: ",  country,  "<br>",
            "Variable: ", variable, "<br>",
            "Status: ",   status,   "<br>",
            "ADF p-val: ",  ifelse(is.na(adf_pval_r),  "N/A", adf_pval_r),  "<br>",
            "KPSS p-val: ", ifelse(is.na(kpss_pval_r), "N/A", kpss_pval_r)
          )
        )

      plotly::plot_ly(
        data   = df,
        x      = ~country,
        y      = ~variable,
        z      = ~status_num,
        type   = "heatmap",
        text   = ~tooltip_text,
        hoverinfo = "text",
        colorscale = list(
          list(0,    "#59A14F"),
          list(0.33, "#F28E2B"),
          list(0.66, "#E15759"),
          list(1,    "#d3d3d3")
        ),
        showscale = FALSE
      ) %>%
        plotly::add_annotations(
          x          = ~country,
          y          = ~variable,
          text       = ~status_label,
          showarrow  = FALSE,
          font       = list(size = 10, color = "white")
        ) %>%
        plotly::layout(
          title  = paste("Unit root heatmap \u2014", toupper(input$test)),
          xaxis  = list(title = "Country",  tickangle = -45),
          yaxis  = list(title = "Variable"),
          margin = list(b = 80)
        )
    })

    output$summary_table <- DT::renderDataTable({
      df <- processed()
      req(nrow(df) > 0)

      df %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(
          n_total        = dplyr::n(),
          n_tested       = sum(status != "missing", na.rm = TRUE),
          n_stationary   = sum(status == "stationary", na.rm = TRUE),
          n_unit_root    = sum(status == "unit_root",  na.rm = TRUE),
          n_mixed        = sum(status == "mixed",      na.rm = TRUE),
          n_missing      = sum(status == "missing",    na.rm = TRUE),
          pct_stationary = round(100 * n_stationary / pmax(n_tested, 1), 1),
          .groups        = "drop"
        ) %>%
        DT::datatable(options = list(pageLength = 10), rownames = FALSE)
    })
  })
}
