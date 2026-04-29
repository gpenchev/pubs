# =============================================================================
# 01_spatial_weights.R
# Build spatial weight matrices for the regression sample
# Three matrices:
#   W_queen     — queen contiguity with 2000 km distance fallback for isolated
#   W_inv_dist  — inverse distance, capped at 2000 km
#   W_dist_band — binary distance band, 1000 km threshold
#
# Distance computation note:
#   All distances are computed after projecting to ETRS89-LAEA (EPSG:3035),
#   a metric equal-area projection appropriate for Europe. Computing Euclidean
#   distance on raw WGS84 degree coordinates is incorrect because 1 degree of
#   longitude varies from ~111 km at the equator to ~55 km at 60N.
# =============================================================================

source(here::here("scripts", "00_setup.R"))

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

regression_countries <- nato_eu_core[nato_eu_core != "LU"]

country_polygons <- all_ne %>%
  dplyr::filter(iso_a2 %in% regression_countries) %>%
  dplyr::select(country = iso_a2, geometry) %>%
  dplyr::arrange(country)

message("Countries in regression sample: ", nrow(country_polygons))
message("Countries: ", paste(country_polygons$country, collapse = ", "))

# --- Queen contiguity ---------------------------------------------------------
nb_queen <- spdep::poly2nb(country_polygons, queen = TRUE)

W_queen <- spdep::nb2listw(nb_queen,
                            style       = "W",
                            zero.policy = TRUE)

message("Queen contiguity neighbours summary:")
print(summary(spdep::card(nb_queen)))

# --- Distance matrix — project to ETRS89-LAEA (EPSG:3035) before computing ---
# ETRS89-LAEA is a metric equal-area projection for Europe.
# Euclidean distance on projected coordinates gives correct km distances.
country_polygons_proj <- country_polygons %>%
  sf::st_transform(3035)

centroids_proj <- country_polygons_proj %>%
  sf::st_centroid() %>%
  sf::st_coordinates()

rownames(centroids_proj) <- country_polygons$country

dist_mat <- as.matrix(dist(centroids_proj)) / 1000

message("Distance matrix range (km): ",
        round(min(dist_mat[dist_mat > 0]), 1), " - ",
        round(max(dist_mat), 1))

# --- Inverse distance weight matrix -------------------------------------------
inv_dist <- 1 / dist_mat
diag(inv_dist) <- 0
inv_dist[dist_mat > 2000] <- 0

row_sums_inv <- rowSums(inv_dist)
row_sums_inv[row_sums_inv == 0] <- 1
W_inv_dist_mat <- inv_dist / row_sums_inv

nb_inv <- spdep::mat2listw(W_inv_dist_mat,
                            style       = "W",
                            zero.policy = TRUE)

# --- Distance band weight matrix (1000 km) ------------------------------------
# Pass raw binary matrix to mat2listw with style = "W" for row-normalisation.
# Do not pre-normalise — mat2listw with style = "W" handles normalisation.
dist_band <- ifelse(dist_mat > 0 & dist_mat <= 1000, 1, 0)
diag(dist_band) <- 0

W_dist_band <- spdep::mat2listw(dist_band,
                                 style       = "W",
                                 zero.policy = TRUE)

message("Distance-band (1000 km) neighbours summary:")
print(summary(rowSums(dist_band)))

# --- Fix isolated countries (zero queen contiguity neighbours) ----------------
card_queen   <- spdep::card(nb_queen)
isolated_idx <- which(card_queen == 0)

if (length(isolated_idx) > 0) {
  message("Isolated countries before fix: ",
          paste(country_polygons$country[isolated_idx], collapse = ", "))

  nb_queen_fixed <- nb_queen

  for (i in isolated_idx) {
    dists        <- dist_mat[i, ]
    dists[i]     <- Inf
    candidates   <- which(dists > 0 & dists <= 2000)
    if (length(candidates) == 0) {
      candidates <- order(dists)[1:2]
    }
    nb_queen_fixed[[i]] <- as.integer(candidates)
  }

  nb_queen <- nb_queen_fixed
  W_queen  <- spdep::nb2listw(nb_queen, style = "W", zero.policy = TRUE)
  message("Queen W recomputed with distance fallback for isolated countries.")
}

card_queen         <- spdep::card(nb_queen)
isolated_countries <- country_polygons$country[card_queen == 0]
message("Countries with zero queen contiguity neighbours after fix: ",
        paste(isolated_countries, collapse = ", "))

# --- Save ---------------------------------------------------------------------
spatial_weights <- list(
  countries          = country_polygons$country,
  nb_queen           = nb_queen,
  W_queen            = W_queen,
  W_inv_dist         = nb_inv,
  W_dist_band        = W_dist_band,
  dist_mat_km        = dist_mat,
  isolated_countries = isolated_countries
)

saveRDS(spatial_weights,
        file.path(path_data, "spatial_weights.rds"))

message("Script 01_spatial_weights complete.")
message("  W_queen:     ", length(regression_countries), "x",
        length(regression_countries), " contiguity matrix")
message("  W_inv_dist:  inverse distance, 2000 km cap")
message("  W_dist_band: binary distance band, 1000 km threshold")
