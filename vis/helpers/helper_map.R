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
    "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT",
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

  all_labels <- get_map_variable_labels()
  var_lab <- names(all_labels)[all_labels == variable]
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

  # Compute bounding box from the loaded geometries so the view always frames
  # the data rather than a hardcoded centre-point. fitBounds covers the full
  # European theatre (including Eastern conflict events near the EU border).
  bbox <- sf::st_bbox(geo_data)
  # Expand slightly west and south to include Western Atlantic coast and
  # the Caucasus / Middle-East theatre that overlaps the study area.
  view_west  <- max(as.numeric(bbox["xmin"]) - 3,  -25)
  view_east  <- min(as.numeric(bbox["xmax"]) + 8,   50)
  view_south <- max(as.numeric(bbox["ymin"]) - 3,   33)
  view_north <- min(as.numeric(bbox["ymax"]) + 2,   73)

  leaflet::leaflet(geo_data) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::fitBounds(
      lng1 = view_west, lat1 = view_south,
      lng2 = view_east, lat2 = view_north
    ) %>%
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
    )
  # fitBounds is applied at the start of the pipe chain above; no setView needed here
}

#' Add a conflict event layer to an existing leaflet map proxy
#'
#' Accepts the pre-aggregated CSV format (lon_grid, lat_grid, year,
#' fatalities, n_events).  Two datasets are supported:
#'   - ged_land  : land-contiguous events only (app_conflict_events.csv, ~787 rows)
#'   - ged_all   : all state-based events in region (app_all_events.csv, ~34 k rows)
#'
#' @param map_proxy   A leaflet proxy object.
#' @param ged_land    Data frame of land-contiguous events. Required.
#' @param yr          Integer year to display.
#' @param mode        "land" (default) = land-contiguous only;
#'                    "all"            = all events (requires ged_all).
#' @param ged_all     Data frame of all events. Used only when mode == "all".
#'                    If NULL and mode == "all", falls back to ged_land.
#' @return The updated leaflet proxy.
add_event_layer <- function(map_proxy,
                            ged_land,
                            yr,
                            mode    = "land",
                            ged_all = NULL) {

  map_proxy <- map_proxy %>%
    leaflet::clearGroup("Conflict events") %>%
    leaflet::removeControl("event_legend")

  # Select the right dataset
  if (mode == "all" && !is.null(ged_all) && nrow(ged_all) > 0) {
    active_data  <- ged_all
    legend_label <- "All state-based conflicts (fatalities)"
    dot_colour   <- "#F28E2B"   # orange — distinct from land-only red
  } else {
    active_data  <- ged_land
    legend_label <- "Land-contiguous conflicts (fatalities)"
    dot_colour   <- "#E15759"   # red — primary measure
  }

  if (is.null(active_data) || nrow(active_data) == 0) return(map_proxy)

  # Normalise column names (aggregated format uses lon_grid/lat_grid)
  events_yr <- active_data %>%
    dplyr::filter(year == yr)

  if ("lon_grid" %in% names(events_yr)) {
    events_yr <- events_yr %>%
      dplyr::rename(lon = lon_grid, lat = lat_grid, best = fatalities)
  }

  if (nrow(events_yr) == 0) return(map_proxy)

  events_yr <- events_yr %>%
    dplyr::mutate(
      radius   = pmin(log(best + 1) * 2.5, 22),
      popup_ev = paste0(
        "<strong>Conflict cluster</strong><br/>",
        "Fatalities: ", best, "<br/>",
        "Events: ",
        if ("n_events" %in% names(events_yr)) n_events else 1L, "<br/>",
        "Year: ", yr
      )
    )

  map_proxy %>%
    leaflet::addCircleMarkers(
      data        = events_yr,
      lng         = ~lon,
      lat         = ~lat,
      radius      = ~radius,
      color       = dot_colour,
      fillColor   = dot_colour,
      fillOpacity = 0.55,
      opacity     = 0.80,
      weight      = 1,
      popup       = ~popup_ev,
      label       = ~paste0(best, " fatalities"),
      group       = "Conflict events"
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      colors   = dot_colour,
      labels   = legend_label,
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
