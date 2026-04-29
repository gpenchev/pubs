# =============================================================================
# 01_download_defence.R
# Download defence spending data from WDI (SIPRI-sourced)
# Indicator: MS.MIL.XPND.GD.ZS — Military expenditure (% of GDP)
# Coverage: all 24 countries, 1995-2023, complete, zero gaps confirmed
#
# Previous source: Eurostat gov_10a_exp (COFOG GF02)
# Reason for switch: Eurostat COFOG GF02 missing FR, DE, GB entirely;
# NO values systematically underreported (partial COFOG classification,
# 0.4-0.9% vs actual 1.4-2.3%). WDI/SIPRI provides consistent
# single-source coverage for all 24 countries.
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Retry helper -------------------------------------------------------------
wdi_with_retry <- function(country, indicator, start, end,
                           max_attempts = 5, wait_base = 10) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      WDI::WDI(
        country   = country,
        indicator = indicator,
        start     = start,
        end       = end
      ),
      error = function(e) {
        message("WDI download attempt ", attempt, " of ", max_attempts,
                " failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) return(result)
    if (attempt < max_attempts) {
      wait_secs <- wait_base * attempt
      message("Waiting ", wait_secs, " seconds before retry...")
      Sys.sleep(wait_secs)
    }
  }
  NULL
}

# --- Download -----------------------------------------------------------------
cache_path <- file.path(path_data, "defence_raw.rds")

wdi_raw <- wdi_with_retry(
  country   = nato_eu_robustness,
  indicator = "MS.MIL.XPND.GD.ZS",
  start     = year_start,
  end       = year_end
)

if (is.null(wdi_raw)) {
  if (file.exists(cache_path)) {
    warning("All WDI download attempts failed. ",
            "Loading cached defence_raw.rds from previous successful run. ",
            "Data may be outdated.")
    defence_filtered <- readRDS(cache_path)
    message("Script 01 complete (from cache): ",
            nrow(defence_filtered), " rows loaded.")
  } else {
    stop("All WDI download attempts failed and no cached file exists at: ",
         cache_path, ". Check your internet connection and the World Bank API.")
  }
} else {

  # --- Rename and filter ------------------------------------------------------
  defence_filtered <- wdi_raw %>%
    dplyr::select(
      country     = iso2c,
      year        = year,
      defence_gdp = MS.MIL.XPND.GD.ZS
    ) %>%
    dplyr::filter(country %in% nato_eu_robustness) %>%
    dplyr::distinct(country, year, .keep_all = TRUE) %>%
    dplyr::arrange(country, year)

  # --- Coverage check ---------------------------------------------------------
  coverage <- defence_filtered %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      n_nonmissing = sum(!is.na(defence_gdp)),
      n_missing    = sum(is.na(defence_gdp)),
      .groups      = "drop"
    )

  missing_countries <- coverage %>% dplyr::filter(n_missing > 0)
  if (nrow(missing_countries) > 0) {
    warning("Defence data has gaps for: ",
            paste(missing_countries$country, collapse = ", "))
  }

  message("Countries with defence data: ",
          paste(sort(unique(defence_filtered$country)), collapse = ", "))

  # --- Save -------------------------------------------------------------------
  readr::write_csv(defence_filtered,
                   file.path(path_data, "defence_raw.csv"))
  saveRDS(defence_filtered,
          file.path(path_data, "defence_raw.rds"))

  message("Script 01 complete: ", nrow(defence_filtered), " rows saved.")
}
