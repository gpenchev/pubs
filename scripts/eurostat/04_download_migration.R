# =============================================================================
# 04_download_migration.R
# Download migration data from Eurostat
# Table: migr_imm1ctz — Immigration by citizenship
# Used as proxy variable for population movement / threat perception
#
# Coverage note: Eurostat immigration data (migr_imm1ctz) is available from
# 2000 onwards for most countries. Years 1995-1999 will have NA immigration_rate
# for all countries. This means the effective regression sample starts in 2000
# even though the panel skeleton starts in 1995, because plm/lm silently drop
# rows with NA regressors. This truncation affects Regime 1 (1995-2004) which
# is effectively estimated on 2000-2004 only. See 02_baseline_ols.R for the
# explicit NA filter that documents this.
#
# GB note: Eurostat carries no immigration data for GB. GB immigration_rate
# will be NA for all years. This is a documented limitation. GB rows will be
# dropped from any model that includes immigration_rate as a regressor.
#
# Population denominator note: population is taken from demo_pjan (1 January).
# Dividing annual immigration flows by 1 January population creates a slight
# timing mismatch. Mid-year population would be more precise but is not
# available at the required country-year granularity from Eurostat.
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Download migration -------------------------------------------------------
raw_migration <- eurostat::get_eurostat(
  id          = "migr_imm1ctz",
  time_format = "num",
  keepFlags   = TRUE
)

raw_migration <- raw_migration %>%
  dplyr::mutate(
    TIME_PERIOD = as.numeric(.data$TIME_PERIOD),
    geo         = dplyr::recode(geo, "EL" = "GR")
  )

# --- Check agedef coverage ---------------------------------------------------
n_complet <- sum(raw_migration$agedef == "COMPLET", na.rm = TRUE)
n_reach   <- sum(raw_migration$agedef == "REACH",   na.rm = TRUE)
message("agedef coverage — COMPLET: ", n_complet, ", REACH: ", n_reach)

countries_complet <- raw_migration %>%
  dplyr::filter(agedef == "COMPLET", geo %in% nato_eu_robustness) %>%
  dplyr::distinct(geo) %>%
  dplyr::pull(geo)

countries_reach_only <- setdiff(nato_eu_robustness, countries_complet)
if (length(countries_reach_only) > 0) {
  warning("Countries with no COMPLET agedef data (may be dropped): ",
          paste(sort(countries_reach_only), collapse = ", "))
}

# --- Filter: total immigration into each country ------------------------------
migration_filtered <- raw_migration %>%
  dplyr::filter(
    agedef      == "COMPLET",
    age         == "TOTAL",
    sex         == "T",
    citizen     == "TOTAL",
    geo         %in% nato_eu_robustness,
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end
  ) %>%
  dplyr::select(
    country       = geo,
    year          = TIME_PERIOD,
    immigration_n = values,
    flag          = flags
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::arrange(country, year)

# --- Download population ------------------------------------------------------
raw_pop <- eurostat::get_eurostat(
  id          = "demo_pjan",
  time_format = "num",
  keepFlags   = FALSE
)

raw_pop <- raw_pop %>%
  dplyr::mutate(
    TIME_PERIOD = as.numeric(.data$TIME_PERIOD),
    geo         = dplyr::recode(geo, "EL" = "GR")
  )

pop_filtered <- raw_pop %>%
  dplyr::filter(
    age         == "TOTAL",
    sex         == "T",
    geo         %in% nato_eu_robustness,
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end
  ) %>%
  dplyr::select(
    country    = geo,
    year       = TIME_PERIOD,
    population = values
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE)

# --- Normalise by population --------------------------------------------------
migration_normalised <- migration_filtered %>%
  dplyr::left_join(pop_filtered, by = c("country", "year")) %>%
  dplyr::mutate(
    immigration_rate = (immigration_n / population) * 1000
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE)

# --- Warn if population join produces NA immigration_rate ---------------------
n_missing_pop <- sum(!is.na(migration_normalised$immigration_n) &
                       is.na(migration_normalised$population))
if (n_missing_pop > 0) {
  warning(n_missing_pop,
          " rows have immigration_n but missing population — immigration_rate will be NA.")
}

n_missing_rate <- sum(is.na(migration_normalised$immigration_rate))
message("immigration_rate missing: ", n_missing_rate, " rows (",
        round(100 * n_missing_rate / nrow(migration_normalised), 1), "%)")

gb_rows <- sum(migration_normalised$country == "GB")
if (gb_rows == 0) {
  message("Note: GB has no Eurostat immigration data — immigration_rate will be NA ",
          "for all GB rows. This is expected and documented.")
}

# --- Save ---------------------------------------------------------------------
readr::write_csv(migration_normalised,
                 file.path(path_data, "migration_raw.csv"))
saveRDS(migration_normalised,
        file.path(path_data, "migration_raw.rds"))

message("Script 04 complete: ", nrow(migration_normalised), " rows saved.")
