#' Load EU country geometries for the choropleth map
#'
#' Returns sf polygons for EU member states plus GB for geographic reference.
#' This country list intentionally differs from the regression sample
#' (nato_eu_core minus LU) to provide a complete geographic view. Countries
#' shown on the map but not in the regression sample will have NA for
#' regression-derived variables.
#'
#' Note: if this list is updated, also check helper_data.R and 00_setup.R
#' for consistency with nato_eu_core.
#'
#' @return An sf data frame with columns: country (ISO2), country_name, geometry.
load_eu_geometries <- function() {
  countries_map <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE",
    "ES", "FI", "FR", "GB", "GR", "HR", "HU", "IE",
    "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT",
    "RO", "SE", "SI", "SK"
  )

  geo <- rnaturalearth::ne_countries(
    scale       = "medium",
    returnclass = "sf"
  ) %>%
    dplyr::mutate(
      iso_a2 = dplyr::case_when(
        name == "France" ~ "FR",
        name == "Norway" ~ "NO",
        TRUE             ~ iso_a2
      )
    ) %>%
    dplyr::filter(iso_a2 %in% countries_map) %>%
    dplyr::select(country = iso_a2, country_name = name_long, geometry) %>%
    sf::st_transform(4326)

  geo
}

#' Build a leaflet choropleth map for a given variable and year
#'
#' @param panel        The panel data frame (panel_full.rds).
#' @param yr           Integer year to display.
#' @param variable     Column name of the variable to map.
#' @param palette_type "seq" for sequential (Blues) or "div" for diverging (RdBu).
#' @param eu_geometries Optional pre-loaded sf geometries from load_eu_geometries().
#'   If NULL, geometries are loaded on each call (slower).
#' @return A leaflet map object.
build_base_map <- function(panel,
                           yr,
                           variable,
                           palette_type   = "seq",
                           eu_geometries  = NULL) {

  geo <- if (!is.null(eu_geometries)) eu_geometries else load_eu_geometries()

  slice <- panel %>%
    dplyr::filter(year == yr) %>%
    dplyr::select(country, regime, defence_source, dplyr::all_of(variable))

  geo_data <- geo %>%
    dplyr::left_join(slice, by = "country")

  if (!variable %in% names(geo_data)) {
    stop("Variable '", variable, "' not found in geo_data after join.")
  }

  vals <- geo_data[[variable]]

  if (palette_type == "div") {
    pal <- leaflet::colorNumeric(
      palette  = "RdBu",
      domain   = vals,
      reverse  = TRUE,
      na.color = "#d3d3d3"
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette  = "Blues",
      domain   = vals,
      na.color = "#d3d3d3"
    )
  }

  var_lab <- names(get_variable_labels())[get_variable_labels() == variable]
  if (length(var_lab) == 0) var_lab <- variable

  popup_text <- paste0(
    "<strong>", geo_data$country_name, "</strong><br/>",
    var_lab, ": ",
    ifelse(is.na(vals), "N/A", round(vals, 3)), "<br/>",
    "Year: ", yr, "<br/>",
    "Regime: ", geo_data$regime, "<br/>",
    "Source: ", ifelse(is.na(geo_data$defence_source),
                       "N/A", geo_data$defence_source)
  )

  label_text <- paste0(
    geo_data$country_name, ": ",
    ifelse(is.na(vals), "N/A", round(vals, 3))
  )

  # Use .data[[variable]] via a local variable to avoid fragile get() calls
  var_sym <- variable

  leaflet::leaflet(geo_data) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      fillColor   = ~pal(geo_data[[var_sym]]),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 1,
      opacity     = 1,
      highlight   = leaflet::highlightOptions(
        weight       = 2,
        color        = "#444",
        fillOpacity  = 0.9,
        bringToFront = TRUE
      ),
      label  = lapply(label_text, htmltools::HTML),
      popup  = lapply(popup_text, htmltools::HTML)
    ) %>%
    leaflet::addLegend(
      pal      = pal,
      values   = geo_data[[var_sym]],
      title    = paste0(var_lab, "<br/>(", yr, ")"),
      position = "bottomright",
      na.label = "No data",
      layerId  = "choropleth_legend"
    ) %>%
    leaflet::setView(lng = 15, lat = 54, zoom = 4)
}

#' Add a conflict event layer to an existing leaflet map proxy
#'
#' Accepts the pre-aggregated app_conflict_events.csv format:
#'   columns: lon_grid, lat_grid, year, fatalities, n_events
#' All events are land-contiguous (pre-filtered). The land_only argument
#' is retained for API compatibility but has no effect on the aggregated data.
#'
#' @param map_proxy     A leaflet proxy object.
#' @param ged_events    Data frame of aggregated conflict events.
#' @param yr            Integer year to display.
#' @param land_only     Ignored (all app events are land-contiguous).
#' @return The updated leaflet proxy.
add_event_layer <- function(map_proxy,
                            ged_events,
                            yr,
                            land_only = TRUE) {
  map_proxy <- map_proxy %>%
    leaflet::clearGroup("Conflict events") %>%
    leaflet::removeControl("event_legend")

  if (is.null(ged_events) || nrow(ged_events) == 0) return(map_proxy)

  # Support both raw (has lon/lat) and aggregated (has lon_grid/lat_grid) formats
  if ("lon_grid" %in% names(ged_events)) {
    events_yr <- ged_events %>%
      dplyr::filter(year == yr) %>%
      dplyr::rename(lon = lon_grid, lat = lat_grid,
                    best = fatalities)
  } else {
    events_yr <- ged_events %>%
      dplyr::filter(year == yr)
    if (isTRUE(land_only) && "land_contiguous" %in% names(events_yr)) {
      events_yr <- events_yr %>% dplyr::filter(land_contiguous)
    }
  }

  if (nrow(events_yr) == 0) return(map_proxy)

  events_yr <- events_yr %>%
    dplyr::mutate(
      radius   = pmin(log(best + 1) * 2.5, 22),
      popup_ev = paste0(
        "<strong>Conflict cluster</strong><br/>",
        "Fatalities: ", best, "<br/>",
        "Events: ", if ("n_events" %in% names(events_yr)) n_events else 1, "<br/>",
        "Year: ", year
      )
    )

  map_proxy %>%
    leaflet::addCircleMarkers(
      data        = events_yr,
      lng         = ~lon,
      lat         = ~lat,
      radius      = ~radius,
      color       = "#E15759",
      fillColor   = "#E15759",
      fillOpacity = 0.65,
      opacity     = 0.85,
      weight      = 1,
      popup       = ~popup_ev,
      label       = ~paste0(best, " fatalities"),
      group       = "Conflict events"
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      colors   = "#E15759",
      labels   = "Land-contiguous conflict (fatalities)",
      title    = "Conflict events",
      opacity  = 0.8,
      layerId  = "event_legend"
    )
}

#' Lazy-load EU geometries (cached in caller environment)
#'
#' Call once inside a Shiny server function using a reactiveVal or
#' local variable. Falls back gracefully to NULL on error.
#'
#' @return sf data frame or NULL
lazy_eu_geometries <- function() {
  tryCatch(
    load_eu_geometries(),
    error = function(e) {
      message("Could not load EU geometries: ", e$message)
      NULL
    }
  )
}
