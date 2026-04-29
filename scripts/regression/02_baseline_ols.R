# =============================================================================
# 02_baseline_ols.R
# Baseline pooled OLS and within (FE) panel regression
# No spatial component — establishes baseline coefficients
# Output: baseline_ols_results.rds
# =============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load panel ---------------------------------------------------------------
panel <- readRDS(file.path(path_data, "panel_full.rds"))

# --- Regression sample --------------------------------------------------------
# Exclude Luxembourg (structural outlier — defence/GDP < 0.2%).
# GB: immigration_rate is NA for all years — GB rows will be dropped only
#     in models that include immigration_rate as a regressor.
#     GB contributes observations for all other variables.
# NO: immigration_rate has 3 missing years (1995-1997) — NO rows for those
#     years will be dropped from models including immigration_rate.
regression_countries <- nato_eu_core[nato_eu_core != "LU"]

reg_data <- panel %>%
  dplyr::filter(country %in% regression_countries) %>%
  dplyr::select(
    country,
    year,
    regime,
    defence_gdp,
    threat_land_log,
    threat_score_log,
    debt_gdp,
    deficit_gdp,
    gdp_growth,
    immigration_rate,
    gov_left_right,
    gov_eu_position,
    election_year
  ) %>%
  dplyr::filter(
    !is.na(defence_gdp),
    !is.na(threat_land_log),
    !is.na(debt_gdp),
    !is.na(deficit_gdp),
    !is.na(gdp_growth),
    !is.na(immigration_rate),
    !is.na(gov_left_right)
  ) %>%
  dplyr::mutate(
    regime  = factor(as.character(regime), levels = c("1", "2", "3", "4")),
    country = as.factor(country),
    year    = as.integer(year)
  )

message("Regression sample: ", nrow(reg_data), " observations")
message("Countries: ", nlevels(reg_data$country))
message("Years: ", min(reg_data$year), "-", max(reg_data$year))
message("Note: immigration_rate is NA for 1995-1999 (Eurostat coverage from 2000).")
message("      GB immigration_rate is NA for all years (no Eurostat coverage).")
message("      GB rows dropped from models including immigration_rate.")
message("      Effective Regime 1 sample is 2000-2004 only.")

message("Year distribution in regression sample:")
print(table(reg_data$year))
message("Regime distribution:")
print(table(reg_data$regime))
message("Country distribution:")
print(table(reg_data$country))

# --- Panel data object --------------------------------------------------------
pdata <- plm::pdata.frame(reg_data,
                           index = c("country", "year"))

# --- Model 1: Pooled OLS ------------------------------------------------------
m1_pooled <- plm::plm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data  = pdata,
  model = "pooling"
)

# --- Model 2: Country FE (within) ---------------------------------------------
m2_fe_country <- plm::plm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data   = pdata,
  model  = "within",
  effect = "individual"
)

# --- Model 3: Two-way FE (country + year) -------------------------------------
m3_fe_twoway <- plm::plm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data   = pdata,
  model  = "within",
  effect = "twoways"
)

# --- Model 4: Two-way FE with regime interactions -----------------------------
m4_fe_regime <- plm::plm(
  defence_gdp ~ threat_land_log * regime + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data   = pdata,
  model  = "within",
  effect = "twoways"
)

# --- Hausman test: FE vs RE ---------------------------------------------------
m2_re <- plm::plm(
  defence_gdp ~ threat_land_log + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year,
  data   = pdata,
  model  = "random"
)

hausman <- plm::phtest(m2_fe_country, m2_re)
message("Hausman test p-value: ", round(hausman$p.value, 4))
message("FE preferred by Hausman: ", hausman$p.value < 0.05)
message("Note: proceeding with FE regardless — standard in defence panel literature")

# --- Wald test for regime interactions ----------------------------------------
reg_data_lm <- reg_data %>%
  dplyr::mutate(
    country_f = as.factor(country),
    year_f    = as.factor(year)
  )

lm_regime <- lm(
  defence_gdp ~ threat_land_log * regime + debt_gdp + deficit_gdp +
    gdp_growth + immigration_rate + gov_left_right +
    gov_eu_position + election_year + country_f + year_f,
  data = reg_data_lm
)

regime_terms <- grep("threat_land_log:regime",
                     names(coef(lm_regime)),
                     value = TRUE)

wald_regime <- NULL
if (length(regime_terms) > 0) {
  wald_regime <- tryCatch(
    car::linearHypothesis(lm_regime, regime_terms),
    error = function(e) {
      message("Wald test failed: ", e$message)
      NULL
    }
  )
  if (!is.null(wald_regime)) {
    message("Wald test for regime interactions:")
    print(wald_regime)
  }
} else {
  message("No regime interaction terms found.")
}

# --- Robust standard errors (Driscoll-Kraay) ----------------------------------
se_m3 <- lmtest::coeftest(
  m3_fe_twoway,
  vcov = function(x) plm::vcovSCC(x, type = "HC3")
)

se_m4 <- lmtest::coeftest(
  m4_fe_regime,
  vcov = function(x) plm::vcovSCC(x, type = "HC3")
)

# --- Summary ------------------------------------------------------------------
message("\nModel 1: Pooled OLS")
print(summary(m1_pooled))

message("\nModel 2: Country FE")
print(summary(m2_fe_country))

message("\nModel 3: Two-way FE")
print(summary(m3_fe_twoway))

message("\nModel 4: Two-way FE + Regime interactions")
print(summary(m4_fe_regime))

message("\nModel 3 with Driscoll-Kraay robust SE:")
print(se_m3)

message("\nModel 4 with Driscoll-Kraay robust SE:")
print(se_m4)

# --- Save ---------------------------------------------------------------------
baseline_results <- list(
  reg_data      = reg_data,
  m1_pooled     = m1_pooled,
  m2_fe_country = m2_fe_country,
  m3_fe_twoway  = m3_fe_twoway,
  m4_fe_regime  = m4_fe_regime,
  m2_re         = m2_re,
  hausman       = hausman,
  wald_regime   = wald_regime,
  se_m3         = se_m3,
  se_m4         = se_m4
)

saveRDS(baseline_results,
        file.path(path_data, "baseline_ols_results.rds"))

message("Script 02_baseline_ols complete.")
