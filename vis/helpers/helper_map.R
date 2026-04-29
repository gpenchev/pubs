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
#' Clears any existing event layer before adding the new one.
#' Circle radius is scaled to log(fatalities + 1) capped at 20px.
#'
#' @param map_proxy  A leaflet proxy object.
#' @param ged_events Data frame of conflict events (ucdp_map_events.rds).
#' @param yr         Integer year to display.
#' @param land_only  Logical. If TRUE, only land-contiguous events are shown.
#' @return The updated leaflet proxy.
add_event_layer <- function(map_proxy,
                            ged_events,
                            yr,
                            land_only = TRUE) {
  map_proxy <- map_proxy %>%
    leaflet::clearGroup("Conflict events") %>%
    leaflet::removeControl("event_legend")

  if (is.null(ged_events) || nrow(ged_events) == 0) return(map_proxy)

  events_yr <- ged_events %>%
    dplyr::filter(year == yr)

  if (isTRUE(land_only)) {
    events_yr <- events_yr %>% dplyr::filter(land_contiguous)
  }

  if (nrow(events_yr) == 0) return(map_proxy)

  events_yr <- events_yr %>%
    dplyr::mutate(
      radius      = pmin(log(best + 1) * 2, 20),
      point_color = ifelse(land_contiguous, "#E15759", "#aaaaaa"),
      popup_ev    = paste0(
        "<strong>", conflict_name, "</strong><br/>",
        "Fatalities: ", best, "<br/>",
        "Land-contiguous: ", land_contiguous
      )
    )

  map_proxy %>%
    leaflet::addCircleMarkers(
      data        = events_yr,
      lng         = ~lon,
      lat         = ~lat,
      radius      = ~radius,
      color       = ~point_color,
      fillColor   = ~point_color,
      fillOpacity = 0.7,
      opacity     = 0.9,
      weight      = 1,
      popup       = ~popup_ev,
      label       = ~conflict_name,
      group       = "Conflict events"
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      colors   = c("#E15759", "#aaaaaa"),
      labels   = c("Land-contiguous", "Sea-separated"),
      title    = "Conflict events",
      opacity  = 0.8,
      layerId  = "event_legend"
    )
}
