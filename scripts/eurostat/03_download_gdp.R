# =============================================================================
# 03_download_gdp.R
# Download GDP data
# Primary source: Eurostat nama_10_pc / nama_10_gdp (all EU countries)
#   GDP per capita: CP_EUR_HAB (current prices, EUR per inhabitant)
#   GDP growth:     CLV_PCH_PRE (chain-linked volumes, % change on previous year)
# Supplementary source: WDI (GB only — Eurostat has no GB data post-Brexit)
#   GDP per capita: NY.GDP.PCAP.CD (current USD, converted to EUR via ECB rate)
#   GDP growth:     NY.GDP.MKTP.KD.ZG (constant prices, % change)
#
# Currency note for GB GDP per capita:
#   Eurostat reports in EUR. WDI reports in USD. GB values are converted to
#   EUR using the ECB annual average EUR/USD exchange rate (Eurostat ert_bil_eur_a)
#   to maintain currency consistency with the rest of the panel.
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Download GDP per capita (Eurostat) ---------------------------------------
raw_gdp_pc <- eurostat::get_eurostat(
  id          = "nama_10_pc",
  time_format = "num",
  keepFlags   = TRUE
)

raw_gdp_pc <- raw_gdp_pc %>%
  dplyr::mutate(
    TIME_PERIOD = as.numeric(.data$TIME_PERIOD),
    geo         = dplyr::recode(geo, "EL" = "GR")
  )

gdp_pc_eurostat <- raw_gdp_pc %>%
  dplyr::filter(
    freq        == "A",
    na_item     == "B1GQ",
    unit        == "CP_EUR_HAB",
    geo         %in% nato_eu_robustness,
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end
  ) %>%
  dplyr::select(
    country = geo,
    year    = TIME_PERIOD,
    gdp_pc  = values
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::arrange(country, year)

# --- Download GDP growth rate (Eurostat) --------------------------------------
raw_gdp_growth <- eurostat::get_eurostat(
  id          = "nama_10_gdp",
  time_format = "num",
  keepFlags   = TRUE
)

raw_gdp_growth <- raw_gdp_growth %>%
  dplyr::mutate(
    TIME_PERIOD = as.numeric(.data$TIME_PERIOD),
    geo         = dplyr::recode(geo, "EL" = "GR")
  )

gdp_growth_eurostat <- raw_gdp_growth %>%
  dplyr::filter(
    freq        == "A",
    na_item     == "B1GQ",
    unit        == "CLV_PCH_PRE",
    geo         %in% nato_eu_robustness,
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end
  ) %>%
  dplyr::select(
    country    = geo,
    year       = TIME_PERIOD,
    gdp_growth = values
  ) %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::arrange(country, year)

# --- Download ECB EUR/USD exchange rate (Eurostat ert_bil_eur_a) --------------
raw_fx <- eurostat::get_eurostat(
  id          = "ert_bil_eur_a",
  time_format = "num"
)

fx_usd <- raw_fx %>%
  dplyr::mutate(TIME_PERIOD = as.numeric(.data$TIME_PERIOD)) %>%
  dplyr::filter(
    currency    == "USD",
    statinfo    == "AVG",
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end
  ) %>%
  dplyr::select(
    year         = TIME_PERIOD,
    eur_usd_rate = values
  ) %>%
  dplyr::distinct(year, .keep_all = TRUE) %>%
  dplyr::arrange(year)

# --- Download WDI GDP data for GB ---------------------------------------------
wdi_gdp_gb <- WDI::WDI(
  country   = "GB",
  indicator = c(
    "NY.GDP.PCAP.CD",
    "NY.GDP.MKTP.KD.ZG"
  ),
  start = year_start,
  end   = year_end
)

wdi_gdp_gb <- wdi_gdp_gb %>%
  dplyr::rename(
    gdp_pc_usd = NY.GDP.PCAP.CD,
    gdp_growth = NY.GDP.MKTP.KD.ZG
  ) %>%
  dplyr::select(year, gdp_pc_usd, gdp_growth) %>%
  dplyr::left_join(fx_usd, by = "year") %>%
  dplyr::mutate(
    gdp_pc  = gdp_pc_usd / eur_usd_rate,
    country = "GB"
  ) %>%
  dplyr::select(country, year, gdp_pc, gdp_growth) %>%
  dplyr::arrange(year)

# --- Merge: Eurostat primary, WDI supplement for GB only ---------------------
gdp_pc_filtered <- gdp_pc_eurostat %>%
  dplyr::bind_rows(
    wdi_gdp_gb %>%
      dplyr::select(country, year, gdp_pc) %>%
      dplyr::anti_join(
        gdp_pc_eurostat %>% dplyr::select(country, year),
        by = c("country", "year")
      )
  ) %>%
  dplyr::arrange(country, year)

gdp_growth_filtered <- gdp_growth_eurostat %>%
  dplyr::bind_rows(
    wdi_gdp_gb %>%
      dplyr::select(country, year, gdp_growth) %>%
      dplyr::anti_join(
        gdp_growth_eurostat %>% dplyr::select(country, year),
        by = c("country", "year")
      )
  ) %>%
  dplyr::arrange(country, year)

# --- Coverage check -----------------------------------------------------------
gdp_pc_gaps <- gdp_pc_filtered %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(n_missing = sum(is.na(gdp_pc)), .groups = "drop") %>%
  dplyr::filter(n_missing > 0)

if (nrow(gdp_pc_gaps) > 0) {
  warning("GDP per capita gaps for: ",
          paste(gdp_pc_gaps$country, collapse = ", "))
}

message("Countries with GDP per capita data: ",
        paste(sort(unique(gdp_pc_filtered$country)), collapse = ", "))
message("Countries with GDP growth data: ",
        paste(sort(unique(gdp_growth_filtered$country)), collapse = ", "))

# --- Save ---------------------------------------------------------------------
readr::write_csv(gdp_pc_filtered,
                 file.path(path_data, "gdp_pc_raw.csv"))
saveRDS(gdp_pc_filtered,
        file.path(path_data, "gdp_pc_raw.rds"))

readr::write_csv(gdp_growth_filtered,
                 file.path(path_data, "gdp_growth_raw.csv"))
saveRDS(gdp_growth_filtered,
        file.path(path_data, "gdp_growth_raw.rds"))

message("Script 03 complete: ",
        nrow(gdp_pc_filtered), " GDP per capita rows, ",
        nrow(gdp_growth_filtered), " GDP growth rows saved.")
