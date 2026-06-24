mod_map_ui <- function(id, var_choices = NULL, default_var = "defence_gdp",
                       has_all_events = FALSE) {
  ns <- NS(id)
  if (is.null(var_choices)) var_choices <- get_variable_labels()

  # Event-mode choices: always offer land-only; add "all" only when the
  # full-events dataset is available (has_all_events = TRUE).
  event_choices <- if (has_all_events) {
    c("Hide"                            = "none",
      "Land-contiguous (primary)"       = "land",
      "All events incl. sea-crossing"   = "all")
  } else {
    c("Hide"                            = "none",
      "Land-contiguous (primary)"       = "land")
  }

  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput(ns("variable"), "Variable",
                  choices  = var_choices,
                  selected = default_var),
      sliderInput(ns("year"), "Year",
                  min   = 1995,
                  max   = 2023,
                  value = 2022,
                  step  = 1,
                  sep   = ""),
      radioButtons(ns("palette"), "Colour palette",
                   choices  = c("Sequential" = "seq",
                                "Diverging"  = "div"),
                   selected = "seq",
                   inline   = TRUE),
      # Event layer toggle.  "land" = land-contiguous only (primary measure).
      # "all" = all state-based conflicts in region (shows Aegean/Black Sea
      # events excluded from main analysis). Only shown when app_all_events.csv
      # is present (has_all_events flag set in app.R based on !is.null(all_events)).
      radioButtons(ns("event_mode"),
                   bslib::tooltip(
                     trigger = list("Conflict events", bsicons::bs_icon("info-circle")),
                     "Land-contiguous: UCDP GED events passing the 50 km sea-crossing
                      filter (primary analysis measure).
                      All events: full state-based conflict set within 500 km
                      — includes Aegean/Black Sea crossings excluded from main model."
                   ),
                   choices  = event_choices,
                   selected = "none",
                   inline   = FALSE)
    ),
    leaflet::leafletOutput(ns("map"), height = PLOT_HEIGHT_MAP)
  )
}

mod_map_server <- function(id, panel_data, ged_events = NULL,
                           ged_all_events = NULL, eu_geometries = NULL) {
  moduleServer(id, function(input, output, session) {

    # Lazy-load geometries once per session (not at app startup)
    geo <- local({
      if (!is.null(eu_geometries)) eu_geometries else lazy_eu_geometries()
    })

    output$map <- leaflet::renderLeaflet({
      req(input$variable, input$year)
      tryCatch(
        build_base_map(
          panel         = panel_data,
          yr            = input$year,
          variable      = input$variable,
          palette_type  = input$palette,
          eu_geometries = geo
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Map error:", e$message),
            type     = "error",
            duration = 8
          )
          leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::addScaleBar(position = "bottomleft") %>%
            leaflet::fitBounds(lng1 = -15, lat1 = 33, lng2 = 50, lat2 = 73)
        }
      )
    })

    # React to event-mode radio and year slider together.
    # event_mode values: "none" | "land" | "all"
    observeEvent(
      list(input$event_mode, input$year),
      {
        proxy <- leaflet::leafletProxy(session$ns("map"))
        mode  <- if (is.null(input$event_mode)) "none" else input$event_mode

        if (mode == "none") {
          proxy %>%
            leaflet::clearGroup("Conflict events") %>%
            leaflet::removeControl("event_legend")
        } else {
          add_event_layer(
            map_proxy = proxy,
            ged_land  = ged_events,
            yr        = input$year,
            mode      = mode,
            ged_all   = ged_all_events
          )
        }
      },
      ignoreInit = TRUE
    )
  })
}
