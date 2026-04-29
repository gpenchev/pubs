mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        selectInput(ns("variable"), "Variable",
                    choices  = get_variable_labels(),
                    selected = "defence_gdp")
      ),
      column(3,
        sliderInput(ns("year"), "Year",
                    min   = 1995,
                    max   = 2023,
                    value = 2022,
                    step  = 1,
                    sep   = "")
      ),
      column(2,
        radioButtons(ns("palette"), "Colour palette",
                     choices  = c("Sequential" = "seq",
                                  "Diverging"  = "div"),
                     selected = "seq",
                     inline   = TRUE)
      ),
      column(2,
        checkboxInput(ns("show_events"), "Show conflict events",
                      value = FALSE)
      ),
      column(2,
        checkboxInput(ns("land_only"), "Land-contiguous only",
                      value = TRUE)
      )
    ),
    leaflet::leafletOutput(ns("map"), height = "560px")
  )
}

mod_map_server <- function(id, panel_data, ged_events = NULL,
                           eu_geometries = NULL) {
  moduleServer(id, function(input, output, session) {

    output$map <- leaflet::renderLeaflet({
      req(input$variable, input$year)
      tryCatch(
        build_base_map(
          panel         = panel_data,
          yr            = input$year,
          variable      = input$variable,
          palette_type  = input$palette,
          eu_geometries = eu_geometries
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Map error:", e$message),
            type     = "error",
            duration = 8
          )
          leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = 15, lat = 54, zoom = 4)
        }
      )
    })

    observeEvent(
      list(input$show_events, input$land_only, input$year),
      {
        proxy <- leaflet::leafletProxy(session$ns("map"))

        if (isTRUE(input$show_events)) {
          add_event_layer(
            map_proxy = proxy,
            ged_events = ged_events,
            yr         = input$year,
            land_only  = isTRUE(input$land_only)
          )
        } else {
          proxy %>%
            leaflet::clearGroup("Conflict events") %>%
            leaflet::removeControl("event_legend")
        }
      },
      ignoreInit = TRUE
    )
  })
}
