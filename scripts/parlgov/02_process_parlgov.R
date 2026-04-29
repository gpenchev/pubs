# =============================================================================
# 02_process_parlgov.R
# Process ParlGov raw tables into country-year political variables
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load raw tables ----------------------------------------------------------
view_cabinet  <- readRDS(file.path(path_parlgov, "view_cabinet_raw.rds"))
view_election <- readRDS(file.path(path_parlgov, "view_election_raw.rds"))
view_party    <- readRDS(file.path(path_parlgov, "view_party_raw.rds"))

# --- Country ISO2 mapping -----------------------------------------------------
country_map <- tibble::tibble(
  country_name_short = c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
    "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
    "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
    "SVN", "ESP", "SWE", "GBR", "NOR"
  ),
  country = c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE",
    "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LV",
    "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "GB", "NO"
  )
)

# --- Panel skeleton -----------------------------------------------------------
panel_skeleton <- tidyr::expand_grid(
  country = nato_eu_robustness,
  year    = seq(year_start, year_end, by = 1)
)

# =============================================================================
# PART 1: Cabinet ideology scores
# =============================================================================

cabinet_parties <- view_cabinet %>%
  dplyr::filter(cabinet_party == 1) %>%
  dplyr::select(
    country_name_short,
    cabinet_id,
    party_id,
    seats,
    start_date
  ) %>%
  dplyr::filter(!is.na(seats), seats > 0)

party_scores <- view_party %>%
  dplyr::select(party_id, left_right, eu_anti_pro)

cabinet_scored <- cabinet_parties %>%
  dplyr::left_join(party_scores, by = "party_id")

# --- Fallback: missing ideology — use party family mean -----------------------
family_means <- view_party %>%
  dplyr::group_by(family_name) %>%
  dplyr::summarise(
    lr_family_mean  = mean(left_right,  na.rm = TRUE),
    eu_family_mean  = mean(eu_anti_pro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lr_family_mean  = dplyr::na_if(lr_family_mean,  NaN),
    eu_family_mean  = dplyr::na_if(eu_family_mean,  NaN)
  )

cabinet_scored <- cabinet_scored %>%
  dplyr::left_join(
    view_party %>% dplyr::select(party_id, family_name),
    by = "party_id"
  ) %>%
  dplyr::left_join(family_means, by = "family_name") %>%
  dplyr::mutate(
    left_right_imputed  = is.na(left_right),
    eu_anti_pro_imputed = is.na(eu_anti_pro),
    left_right  = dplyr::coalesce(left_right,  lr_family_mean),
    eu_anti_pro = dplyr::coalesce(eu_anti_pro, eu_family_mean)
  )

# --- Seat-weighted mean per cabinet spell -------------------------------------
cabinet_ideology <- cabinet_scored %>%
  dplyr::group_by(country_name_short, cabinet_id, start_date) %>%
  dplyr::summarise(
    gov_left_right  = sum(left_right  * seats, na.rm = TRUE) / sum(seats, na.rm = TRUE),
    gov_eu_position = sum(eu_anti_pro * seats, na.rm = TRUE) / sum(seats, na.rm = TRUE),
    any_lr_imputed  = any(left_right_imputed),
    any_eu_imputed  = any(eu_anti_pro_imputed),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    gov_left_right  = dplyr::na_if(gov_left_right,  NaN),
    gov_eu_position = dplyr::na_if(gov_eu_position, NaN),
    start_year = lubridate::year(as.Date(start_date))
  ) %>%
  dplyr::left_join(country_map, by = "country_name_short") %>%
  dplyr::filter(!is.na(country))

# --- Expand cabinet spells to country-year (carry-forward) --------------------
expand_cabinet_to_years <- function(df_country) {
  df_country <- df_country %>%
    dplyr::arrange(start_year)

  if (anyDuplicated(df_country$start_year)) {
    warning("Duplicate cabinet start years detected for country: ",
            unique(df_country$country),
            " — keeping first cabinet per year.")
    df_country <- df_country %>%
      dplyr::distinct(start_year, .keep_all = TRUE)
  }

  n    <- nrow(df_country)
  rows <- vector("list", n)

  for (i in seq_len(n)) {
    yr_start <- df_country$start_year[i]
    yr_end   <- if (i < n) df_country$start_year[i + 1] - 1 else year_end
    yr_start <- max(yr_start, year_start)
    yr_end   <- min(yr_end,   year_end)

    if (yr_start > yr_end) next

    rows[[i]] <- tibble::tibble(
      country         = df_country$country[i],
      year            = seq(yr_start, yr_end),
      gov_left_right  = df_country$gov_left_right[i],
      gov_eu_position = df_country$gov_eu_position[i],
      any_lr_imputed  = df_country$any_lr_imputed[i],
      any_eu_imputed  = df_country$any_eu_imputed[i]
    )
  }

  dplyr::bind_rows(rows)
}

cabinet_country_year <- cabinet_ideology %>%
  dplyr::group_by(country) %>%
  dplyr::group_split() %>%
  purrr::map(expand_cabinet_to_years) %>%
  dplyr::bind_rows()

# =============================================================================
# PART 2: Election year flag
# =============================================================================

election_years <- view_election %>%
  dplyr::filter(election_type == "parliament") %>%
  dplyr::select(country_name_short, election_date) %>%
  dplyr::mutate(
    year          = lubridate::year(as.Date(election_date)),
    election_year = 1L
  ) %>%
  dplyr::left_join(country_map, by = "country_name_short") %>%
  dplyr::filter(!is.na(country)) %>%
  dplyr::distinct(country, year, election_year)

# =============================================================================
# PART 3: Merge into panel skeleton
# =============================================================================

parlgov_country_year <- panel_skeleton %>%
  dplyr::left_join(cabinet_country_year, by = c("country", "year")) %>%
  dplyr::left_join(election_years,       by = c("country", "year")) %>%
  dplyr::mutate(
    election_year = dplyr::coalesce(election_year, 0L)
  )

# =============================================================================
# PART 4: Coverage diagnostics
# =============================================================================

coverage <- parlgov_country_year %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    n_years        = dplyr::n(),
    n_lr_missing   = sum(is.na(gov_left_right)),
    n_eu_missing   = sum(is.na(gov_eu_position)),
    n_nan_lr       = sum(is.nan(gov_left_right)),
    n_nan_eu       = sum(is.nan(gov_eu_position)),
    n_elections    = sum(election_year, na.rm = TRUE),
    pct_lr_covered = round(100 * (1 - n_lr_missing / n_years), 1),
    .groups = "drop"
  )

message("ParlGov coverage by country:")
print(coverage)

if (any(coverage$n_nan_lr > 0) || any(coverage$n_nan_eu > 0)) {
  warning("NaN values detected in gov_left_right or gov_eu_position — ",
          "check party family score coverage and zero-seat cabinet handling.")
}

poor_coverage <- coverage %>%
  dplyr::filter(pct_lr_covered < 80)

if (nrow(poor_coverage) > 0) {
  warning("Countries with < 80% left-right coverage: ",
          paste(poor_coverage$country, collapse = ", "))
}

# =============================================================================
# PART 5: Save
# =============================================================================

saveRDS(parlgov_country_year, file.path(path_parlgov, "parlgov_country_year.rds"))
readr::write_csv(parlgov_country_year, file.path(path_parlgov, "parlgov_country_year.csv"))

message("Script 02_process_parlgov complete.")
message("  Rows: ", nrow(parlgov_country_year))
message("  Cols: ", ncol(parlgov_country_year))
