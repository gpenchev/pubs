# =============================================================================
# 05_merge_eurostat.R
# Merge all sources into a single country-year panel
#
# Sources by variable:
#   defence_gdp      — WDI/SIPRI (all 24 countries, single source)
#   debt_gdp         — IMF WEO (all 24 countries, single source)
#   deficit_gdp      — IMF WEO (all 24 countries, single source)
#   gdp_pc           — Eurostat primary, WDI supplement for GB only
#   gdp_growth       — Eurostat primary, WDI supplement for GB only
#   immigration_rate — Eurostat only (GB is NA for all years)
#
# GB immigration_rate is NA for all years by design, not data failure.
# GB is a structural outlier in the threat-defence space: as an island
# nation, the land-border threat measure systematically underestimates
# its threat environment, and its defence commitments reflect global
# power projection rather than European territorial defence. GB is
# excluded from primary regression models (which include immigration_rate)
# and enters only the robustness specification without immigration_rate.
# See 00_setup.R and 09_revision_checks.R Check F and G for full
# documentation.
#
# Source tracking columns added to panel:
#   defence_source — "wdi" for all countries
#   fiscal_source  — "imf_weo" for all countries
#   gdp_source     — "eurostat" for EU countries, "wdi" for GB
#
# Join key: country ISO2 + year
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load cleaned sources -----------------------------------------------------
defence <- readRDS(file.path(path_data, "defence_raw.rds")) %>%
  dplyr::select(country, year, defence_gdp)

debt <- readRDS(file.path(path_data, "debt_raw.rds")) %>%
  dplyr::select(country, year, debt_gdp)

deficit <- readRDS(file.path(path_data, "deficit_raw.rds")) %>%
  dplyr::select(country, year, deficit_gdp)

gdp_pc <- readRDS(file.path(path_data, "gdp_pc_raw.rds")) %>%
  dplyr::select(country, year, gdp_pc)

gdp_growth <- readRDS(file.path(path_data, "gdp_growth_raw.rds")) %>%
  dplyr::select(country, year, gdp_growth)

migration <- readRDS(file.path(path_data, "migration_raw.rds")) %>%
  dplyr::select(country, year, immigration_rate)

# --- Build panel skeleton -----------------------------------------------------
panel_skeleton <- tidyr::expand_grid(
  country = nato_eu_robustness,
  year    = seq(year_start, year_end, by = 1)
)

# --- Sequential left joins ----------------------------------------------------
panel <- panel_skeleton %>%
  dplyr::left_join(defence,    by = c("country", "year")) %>%
  dplyr::left_join(debt,       by = c("country", "year")) %>%
  dplyr::left_join(deficit,    by = c("country", "year")) %>%
  dplyr::left_join(gdp_pc,     by = c("country", "year")) %>%
  dplyr::left_join(gdp_growth, by = c("country", "year")) %>%
  dplyr::left_join(migration,  by = c("country", "year"))

# --- Add source tracking columns ----------------------------------------------
panel <- panel %>%
  dplyr::mutate(
    defence_source = "wdi",
    fiscal_source  = "imf_weo",
    gdp_source     = dplyr::if_else(country == "GB", "wdi", "eurostat")
  )

# --- Add regime indicator -----------------------------------------------------
panel <- panel %>%
  dplyr::mutate(
    regime = dplyr::case_when(
      year >= regimes$regime_1[1] & year <= regimes$regime_1[2] ~ 1L,
      year >= regimes$regime_2[1] & year <= regimes$regime_2[2] ~ 2L,
      year >= regimes$regime_3[1] & year <= regimes$regime_3[2] ~ 3L,
      year >= regimes$regime_4[1] & year <= regimes$regime_4[2] ~ 4L,
      TRUE ~ NA_integer_
    )
  )

# --- Verify all years have a regime assigned ----------------------------------
n_missing_regime <- sum(is.na(panel$regime))
if (n_missing_regime > 0) {
  stop("Regime coding gap detected: ", n_missing_regime,
       " rows have NA regime. Check regimes list in 00_setup.R.")
}
message("Regime coding verified: all ", nrow(panel), " rows assigned a regime.")

# --- Add country name ---------------------------------------------------------
panel <- panel %>%
  dplyr::mutate(
    country_name = countrycode::countrycode(
      country,
      origin      = "iso2c",
      destination = "country.name"
    )
  ) %>%
  dplyr::select(
    country, country_name, year, regime,
    defence_source, fiscal_source, gdp_source,
    defence_gdp, debt_gdp, deficit_gdp,
    gdp_pc, gdp_growth, immigration_rate
  )

# --- Coverage summary ---------------------------------------------------------
coverage_summary <- panel %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    dplyr::across(
      c(defence_gdp, debt_gdp, deficit_gdp, gdp_pc, gdp_growth, immigration_rate),
      ~ sum(!is.na(.x)),
      .names = "n_{.col}"
    ),
    .groups = "drop"
  )

message("Coverage summary:")
for (i in seq_len(nrow(coverage_summary))) {
  message(
    coverage_summary$country[i], ": ",
    "defence=", coverage_summary$n_defence_gdp[i], " ",
    "debt=",    coverage_summary$n_debt_gdp[i], " ",
    "deficit=", coverage_summary$n_deficit_gdp[i], " ",
    "gdp_pc=",  coverage_summary$n_gdp_pc[i], " ",
    "growth=",  coverage_summary$n_gdp_growth[i], " ",
    "immig=",   coverage_summary$n_immigration_rate[i]
  )
}

gb_immig <- sum(!is.na(panel$immigration_rate[panel$country == "GB"]))
if (gb_immig == 0) {
  message("Note: GB immigration_rate is NA for all years by design. ",
          "GB is a structural outlier in the threat-defence space and is ",
          "excluded from primary regression models. See 00_setup.R and ",
          "09_revision_checks.R Check F and G for full documentation.")
}

# --- Save ---------------------------------------------------------------------
readr::write_csv(panel, file.path(path_data, "panel_eurostat.csv"))
saveRDS(panel,          file.path(path_data, "panel_eurostat.rds"))

message("Script 05 complete: panel dimensions ",
        nrow(panel), " rows x ", ncol(panel), " columns.")
