# =============================================================================
# 07_process_ucdp.R
# Aggregate UCDP GED events to country-year threat scores
#
# Two threat scores produced:
#   threat_score  — all state-based conflicts within 500 km (robustness)
#   threat_land   — land-contiguous conflicts only within 500 km (primary)
#
# Land-contiguity filter:
#   An event passes if the straight-line path from the event to the nearest
#   point on the EU external land border crosses no more than 50 km of open
#   sea. The 50 km threshold was chosen to allow for narrow straits (e.g.
#   the Danish straits, ~8 km) while excluding clearly sea-separated
#   conflicts (e.g. North Africa across the Mediterranean, ~150 km minimum).
#
# Threat formula:
#   threat(c, t) = sum_e [ log(fatalities_e + 1) * exp(-distance(c,e) / 500) ]
#
# Distance: nearest point on country border polygon to event location,
#           computed after projecting to ETRS89-LAEA (EPSG:3035).
# Filter:   state-based conflict only (type_of_violence == 1)
#
# Parallelisation note:
#   furrr/future multisession cannot serialise sf external pointers across
#   workers. This script uses parallel::mclapply (fork-based) which shares
#   memory with child processes and avoids serialisation entirely.
#   mclapply works on Linux and macOS only — not on Windows.
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Parallel backend ---------------------------------------------------------
n_workers <- max(1L, parallel::detectCores() - 1L)
message("Cores detected: ", parallel::detectCores())
message("Workers to use: ", n_workers)

# --- Load data ----------------------------------------------------------------
ged   <- readRDS(file.path(path_data, "ucdp_ged_europe.rds"))
panel <- readRDS(file.path(path_data, "panel_eurostat.rds"))

# =============================================================================
# PART 1: Build spatial reference objects
# =============================================================================

# France and Norway have iso_a2 = "-99" in Natural Earth — fix using name.
all_ne <- rnaturalearth::ne_countries(
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
  sf::st_transform(4326)

# Country polygons for threat score computation (all panel countries)
country_polygons <- all_ne %>%
  dplyr::filter(iso_a2 %in% nato_eu_core) %>%
  dplyr::select(country = iso_a2, geometry)

# Country centroids (used for robustness centroid-based threat score)
country_centroids <- country_polygons %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    lon_country = sf::st_coordinates(geometry)[, 1],
    lat_country = sf::st_coordinates(geometry)[, 2]
  ) %>%
  sf::st_drop_geometry()

# EU external land border — used for land-contiguity filter
eu_members_core <- nato_eu_core[!nato_eu_core %in% c("NO", "GB")]

eu_polygon <- all_ne %>%
  dplyr::filter(iso_a2 %in% eu_members_core) %>%
  sf::st_union() %>%
  sf::st_transform(4326)

eu_border <- sf::st_boundary(eu_polygon)

message("EU external land border built from ", length(eu_members_core),
        " countries.")

# Ocean mask — used to measure sea crossings in land-contiguity filter.
# Bounding box covers the European theatre and surrounding region.
world_land <- rnaturalearth::ne_countries(
  scale       = "medium",
  returnclass = "sf"
) %>%
  sf::st_union() %>%
  sf::st_transform(4326)

region_bbox <- sf::st_as_sfc(
  sf::st_bbox(
    c(xmin = -30, ymin = 10, xmax = 80, ymax = 80),
    crs = sf::st_crs(4326)
  )
)

ocean_mask <- sf::st_difference(region_bbox, world_land)

message("Ocean mask built.")

# =============================================================================
# PART 2: Prepare UCDP GED events
# =============================================================================

# Filter to state-based conflict only (type_of_violence == 1).
ged_sf <- ged %>%
  dplyr::filter(
    !is.na(longitude),
    !is.na(latitude),
    type_of_violence == 1
  ) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Remove events whose coordinates fall inside EU territory (geocoding errors).
inside_eu <- sf::st_within(ged_sf, eu_polygon, sparse = FALSE)[, 1]
n_inside  <- sum(inside_eu)
message("Events removed as EU interior geocoding errors: ", n_inside)
ged_sf <- ged_sf[!inside_eu, ]

message("UCDP events after state-based filter and EU interior exclusion: ",
        nrow(ged_sf))

# =============================================================================
# PART 3: Land-contiguity filter (parallel via mclapply)
# =============================================================================

# Serialise sf objects to WKT strings so forked child processes can
# reconstruct them. mclapply uses fork() which copies the parent memory
# space — the WKT strings are plain character vectors and copy safely.
eu_border_wkt  <- sf::st_as_text(eu_border)
ocean_mask_wkt <- sf::st_as_text(ocean_mask)

message("Serialising geometries for parallel transfer...")
geometries_wkt <- sf::st_as_text(sf::st_geometry(ged_sf))

message("Applying land-contiguity filter to ",
        length(geometries_wkt), " events using ", n_workers, " cores...")

land_contiguous <- parallel::mclapply(
  geometries_wkt,
  function(geom_wkt) {
    eu_border_sf <- sf::st_as_sfc(eu_border_wkt, crs = 4326)
    ocean_sf     <- sf::st_as_sfc(ocean_mask_wkt, crs = 4326)
    event_geom   <- sf::st_as_sfc(geom_wkt, crs = 4326)

    path_line <- sf::st_nearest_points(event_geom, eu_border_sf)

    sea_segment <- tryCatch(
      sf::st_intersection(path_line, ocean_sf),
      error = function(e) sf::st_geometrycollection()
    )

    if (is.null(sea_segment) ||
        length(sea_segment) == 0 ||
        sf::st_is_empty(sea_segment)) {
      return(TRUE)
    }

    sea_km <- tryCatch(
      as.numeric(sf::st_length(sea_segment)) / 1000,
      error = function(e) 0
    )

    return(sum(sea_km, na.rm = TRUE) <= 50)
  },
  mc.cores = n_workers
) %>% unlist()

ged_sf <- ged_sf %>%
  dplyr::mutate(land_contiguous = land_contiguous)

n_pass <- sum(ged_sf$land_contiguous)
n_fail <- sum(!ged_sf$land_contiguous)

message("Land-contiguity filter results:")
message("  Pass (land-contiguous): ", n_pass)
message("  Fail (sea-separated):   ", n_fail)

ged_all  <- ged_sf
ged_land <- ged_sf %>% dplyr::filter(land_contiguous)

# =============================================================================
# PART 4: Threat score functions
# =============================================================================

# Spatial decay bandwidth: 500 km. At this distance, an event contributes
# exp(-1) ~37% of its log-fatality weight. At 1000 km ~14%.
bandwidth_km <- 500

# Pre-compute all event coordinates as plain data frames (no sf geometry)
# so they can be passed to forked workers without serialisation issues.
event_coords <- sf::st_coordinates(ged_sf)

event_df_all <- ged_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    lon = event_coords[, 1],
    lat = event_coords[, 2]
  )

event_df_land <- ged_land %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    lon = event_coords[ged_sf$land_contiguous, 1],
    lat = event_coords[ged_sf$land_contiguous, 2]
  )

# Serialise country polygons to WKT for use in forked workers
country_polygons_wkt <- country_polygons %>%
  dplyr::mutate(geometry_wkt = sf::st_as_text(geometry)) %>%
  sf::st_drop_geometry()

# Compute border-based threat score for one country-year.
# Reconstructs sf objects from WKT inside the worker.
compute_threat_border_par <- function(country_iso,
                                      yr,
                                      country_polygons_wkt,
                                      event_df,
                                      bandwidth_km) {
  poly_row <- country_polygons_wkt[country_polygons_wkt$country == country_iso, ]
  if (nrow(poly_row) == 0) return(NA_real_)

  events_yr <- event_df[event_df$year == yr, ]
  if (nrow(events_yr) == 0) return(0)

  poly_sf <- sf::st_as_sfc(poly_row$geometry_wkt[1], crs = 4326)

  events_sf <- sf::st_as_sf(
    events_yr,
    coords = c("lon", "lat"),
    crs    = 4326
  )

  distances_m  <- as.numeric(sf::st_distance(poly_sf, events_sf))
  distances_km <- distances_m / 1000

  fat_in  <- events_yr$best
  fat_in[is.na(fat_in)] <- 0

  log_fat <- log(fat_in + 1)
  score   <- sum(log_fat * exp(-distances_km / bandwidth_km), na.rm = TRUE)
  return(score)
}

# Compute centroid-based threat score for one country-year.
compute_threat_centroid_par <- function(country_iso,
                                        yr,
                                        country_centroids,
                                        event_df,
                                        bandwidth_km) {
  centroid <- country_centroids[country_centroids$country == country_iso, ]
  if (nrow(centroid) == 0) return(NA_real_)

  events_yr <- event_df[event_df$year == yr, ]
  if (nrow(events_yr) == 0) return(0)

  centroid_sf <- sf::st_as_sf(
    centroid,
    coords = c("lon_country", "lat_country"),
    crs    = 4326
  )

  events_sf <- sf::st_as_sf(
    events_yr,
    coords = c("lon", "lat"),
    crs    = 4326
  )

  distances_m  <- as.numeric(sf::st_distance(centroid_sf, events_sf))
  distances_km <- distances_m / 1000

  fat_in  <- events_yr$best
  fat_in[is.na(fat_in)] <- 0

  log_fat <- log(fat_in + 1)
  score   <- sum(log_fat * exp(-distances_km / bandwidth_km), na.rm = TRUE)
  return(score)
}

# =============================================================================
# PART 5: Compute all threat scores (parallel via mclapply)
# =============================================================================

country_years <- panel %>%
  dplyr::select(country, year) %>%
  dplyr::distinct()

message("Computing threat scores for ",
        nrow(country_years), " country-year combinations using ",
        n_workers, " cores...")

results <- parallel::mclapply(
  seq_len(nrow(country_years)),
  function(i) {
    cy   <- country_years[i, ]
    ciso <- cy$country
    yr   <- cy$year

    ts  <- compute_threat_border_par(ciso, yr, country_polygons_wkt,
                                      event_df_all,  bandwidth_km)
    tsc <- compute_threat_centroid_par(ciso, yr, country_centroids,
                                        event_df_all,  bandwidth_km)
    tl  <- compute_threat_border_par(ciso, yr, country_polygons_wkt,
                                      event_df_land, bandwidth_km)

    list(
      country               = ciso,
      year                  = yr,
      threat_score          = ts,
      threat_score_centroid = tsc,
      threat_land           = tl
    )
  },
  mc.cores = n_workers
)

threat_scores <- dplyr::bind_rows(results)

if (nrow(threat_scores) != nrow(country_years)) {
  stop("Threat score computation produced ", nrow(threat_scores),
       " rows but expected ", nrow(country_years), ".")
}

message("Threat score computation complete.")

# =============================================================================
# PART 6: Normalise
# =============================================================================

threat_scores <- threat_scores %>%
  dplyr::mutate(
    threat_score_log    = log(threat_score + 1),
    threat_score_z      = as.numeric(scale(log(threat_score + 1))),
    threat_centroid_log = log(threat_score_centroid + 1),
    threat_centroid_z   = as.numeric(scale(log(threat_score_centroid + 1))),
    threat_land_log     = log(threat_land + 1),
    threat_land_z       = as.numeric(scale(log(threat_land + 1)))
  )

# =============================================================================
# PART 7: Diagnostics
# =============================================================================

message("Threat score summary:")
print(summary(threat_scores %>%
  dplyr::select(threat_score, threat_land,
                threat_score_log, threat_land_log)))

na_land <- threat_scores %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    pct_land_na = round(100 * mean(is.na(threat_land)), 1),
    .groups = "drop"
  ) %>%
  dplyr::filter(pct_land_na > 0)

if (nrow(na_land) > 0) {
  message("Countries with missing threat_land values:")
  print(na_land)
}

# =============================================================================
# PART 8: Export event data for map visualisation
# =============================================================================

ged_map_export <- ged_sf %>%
  dplyr::mutate(
    lon = sf::st_coordinates(geometry)[, 1],
    lat = sf::st_coordinates(geometry)[, 2]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    conflict_name,
    year,
    best,
    land_contiguous,
    lon,
    lat
  )

saveRDS(ged_map_export, file.path(path_data, "ucdp_map_events.rds"))
readr::write_csv(ged_map_export, file.path(path_data, "ucdp_map_events.csv"))

message("Map events exported: ", nrow(ged_map_export), " rows.")

# =============================================================================
# PART 9: Save threat scores
# =============================================================================

saveRDS(threat_scores, file.path(path_data, "threat_scores.rds"))
readr::write_csv(threat_scores, file.path(path_data, "threat_scores.csv"))

message("Script 07 complete: threat scores computed for ",
        nrow(threat_scores), " country-year observations.")
message("  Variables: threat_score, threat_land, centroid variants, ",
        "log and z transforms.")
