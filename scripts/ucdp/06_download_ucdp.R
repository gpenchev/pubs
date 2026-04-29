# =============================================================================
# 06_download_ucdp.R
# Download UCDP Georeferenced Event Dataset (GED)
# Source: https://ucdp.uu.se/downloads/
# The GED global file is downloaded as a CSV and saved locally.
# Version used: UCDP GED 25.1 (covers up to end 2024)
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Manual download note -----------------------------------------------------
# Download the global CSV from:
#   https://ucdp.uu.se/downloads/ged/ged251-csv.zip
# Unzip and place the file at:
#   scripts/output/data/ucdp_ged_raw.csv

ged_path <- file.path(path_data, "ucdp_ged_raw.csv")

if (!file.exists(ged_path)) {
  stop(
    "UCDP GED file not found at: ", ged_path, "\n",
    "Please download GED 25.1 from https://ucdp.uu.se/downloads/ ",
    "and place at the path above."
  )
}

# --- Validate expected columns ------------------------------------------------
ged_header <- readr::read_csv(
  ged_path,
  n_max          = 0,
  show_col_types = FALSE
)

required_cols <- c("year", "type_of_violence", "latitude", "longitude", "best")
missing_cols  <- setdiff(required_cols, names(ged_header))
if (length(missing_cols) > 0) {
  stop("UCDP GED file is missing required columns: ",
       paste(missing_cols, collapse = ", "),
       "\nCheck that the correct GED version (25.1) has been downloaded.")
}

# --- Load ---------------------------------------------------------------------
ged_raw <- readr::read_csv(
  ged_path,
  col_types = readr::cols(
    id               = readr::col_character(),
    year             = readr::col_integer(),
    type_of_violence = readr::col_integer(),
    country          = readr::col_character(),
    country_id       = readr::col_integer(),
    region           = readr::col_character(),
    latitude         = readr::col_double(),
    longitude        = readr::col_double(),
    deaths_a         = readr::col_double(),
    deaths_b         = readr::col_double(),
    deaths_civilians = readr::col_double(),
    deaths_unknown   = readr::col_double(),
    best             = readr::col_double(),
    .default         = readr::col_character()
  )
)

message("GED 25.1 loaded: ", nrow(ged_raw), " total events, years ",
        min(ged_raw$year, na.rm = TRUE), "-", max(ged_raw$year, na.rm = TRUE))

# --- Filter to European theatre and relevant years ----------------------------
# Bounding box: Latitude 30N-72N, Longitude 25W-45E
ged_europe <- ged_raw %>%
  dplyr::filter(
    year      >= year_start,
    year      <= year_end,
    latitude  >= 30,
    latitude  <= 72,
    longitude >= -25,
    longitude <= 45
  )

message("Events in European bounding box (", year_start, "-", year_end, "): ",
        nrow(ged_europe))

# --- Save ---------------------------------------------------------------------
saveRDS(ged_europe, file.path(path_data, "ucdp_ged_europe.rds"))
readr::write_csv(ged_europe, file.path(path_data, "ucdp_ged_europe.csv"))

message("Script 06 complete: ",
        nrow(ged_europe), " European conflict events loaded.")
