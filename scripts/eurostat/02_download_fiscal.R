# =============================================================================
# 02_download_fiscal.R
# Download fiscal data from IMF World Economic Outlook (WEO)
# Indicators:
#   GGXCNL_NGDP — Net lending (+) / net borrowing (-), % of GDP (deficit)
#   GGXWDG_NGDP — Gross debt, General government, % of GDP
# Coverage: all 24 countries, 1995-2023, complete, zero gaps confirmed
#
# Previous source: Eurostat gov_10dd_edpt1
# Reason for switch: Eurostat carries zero rows for NO and GB.
# IMF WEO provides consistent single-source coverage for all 24 countries.
# Sign convention: negative = deficit (consistent with Eurostat convention,
# confirmed against Greece 2009-2013 values).
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- ISO2 to ISO3 mapping for IMF WEO -----------------------------------------
iso2_to_iso3 <- c(
  "BE" = "BEL", "BG" = "BGR", "HR" = "HRV", "CZ" = "CZE",
  "DK" = "DNK", "EE" = "EST", "FI" = "FIN", "FR" = "FRA",
  "DE" = "DEU", "GR" = "GRC", "HU" = "HUN", "IT" = "ITA",
  "LV" = "LVA", "LT" = "LTU", "LU" = "LUX", "NL" = "NLD",
  "PL" = "POL", "PT" = "PRT", "RO" = "ROU", "SK" = "SVK",
  "SI" = "SVN", "ES" = "ESP", "NO" = "NOR", "GB" = "GBR"
)

iso3_codes <- unname(iso2_to_iso3)

# --- Download from IMF WEO ----------------------------------------------------
imf_raw <- imfapi::imf_get(
  dataflow_id = "WEO",
  dimensions  = list(
    COUNTRY   = iso3_codes,
    INDICATOR = c("GGXCNL_NGDP", "GGXWDG_NGDP"),
    FREQUENCY = "A"
  )
)

# --- Filter to study period ---------------------------------------------------
imf_filtered <- imf_raw %>%
  dplyr::mutate(year = as.integer(TIME_PERIOD)) %>%
  dplyr::filter(year >= year_start, year <= year_end) %>%
  dplyr::mutate(
    country = countrycode::countrycode(
      COUNTRY,
      origin      = "iso3c",
      destination = "iso2c"
    )
  ) %>%
  dplyr::filter(country %in% nato_eu_robustness)

# --- Split into deficit and debt ----------------------------------------------
deficit_filtered <- imf_filtered %>%
  dplyr::filter(INDICATOR == "GGXCNL_NGDP") %>%
  dplyr::select(
    country     = country,
    year        = year,
    deficit_gdp = OBS_VALUE
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::arrange(country, year)

debt_filtered <- imf_filtered %>%
  dplyr::filter(INDICATOR == "GGXWDG_NGDP") %>%
  dplyr::select(
    country  = country,
    year     = year,
    debt_gdp = OBS_VALUE
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::arrange(country, year)

# --- Coverage check -----------------------------------------------------------
deficit_gaps <- deficit_filtered %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(n_missing = sum(is.na(deficit_gdp)), .groups = "drop") %>%
  dplyr::filter(n_missing > 0)

debt_gaps <- debt_filtered %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(n_missing = sum(is.na(debt_gdp)), .groups = "drop") %>%
  dplyr::filter(n_missing > 0)

if (nrow(deficit_gaps) > 0) {
  warning("Deficit data has gaps for: ",
          paste(deficit_gaps$country, collapse = ", "))
}
if (nrow(debt_gaps) > 0) {
  warning("Debt data has gaps for: ",
          paste(debt_gaps$country, collapse = ", "))
}

# --- Save ---------------------------------------------------------------------
readr::write_csv(debt_filtered,
                 file.path(path_data, "debt_raw.csv"))
saveRDS(debt_filtered,
        file.path(path_data, "debt_raw.rds"))

readr::write_csv(deficit_filtered,
                 file.path(path_data, "deficit_raw.csv"))
saveRDS(deficit_filtered,
        file.path(path_data, "deficit_raw.rds"))

message("Script 02 complete: ",
        nrow(debt_filtered), " debt rows, ",
        nrow(deficit_filtered), " deficit rows saved.")
message("Countries with debt data: ",
        paste(sort(unique(debt_filtered$country)), collapse = ", "))
message("Countries with deficit data: ",
        paste(sort(unique(deficit_filtered$country)), collapse = ", "))
