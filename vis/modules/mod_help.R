mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        offset = 1,
        br(),
        h2("EU Defence Spending Panel — Help"),
        p(
          "This application visualises the data and results from the study of",
          "NATO-EU defence spending determinants, 1995-2023.",
          "Full methodology is available at",
          a("pub.e-dnrs.org", href = "https://pub.e-dnrs.org", target = "_blank"),
          "."
        ),
        hr(),

        bslib::accordion(
          open = FALSE,
          multiple = TRUE,

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Overview",
            icon  = bsicons::bs_icon("info-circle"),
            p(
              "The panel covers 24 NATO-EU member states plus Norway and",
              "Great Britain over the period 1995-2023 (29 years, 696 country-year",
              "observations). The primary regression sample contains 530 observations",
              "across 22 countries after applying sample restrictions."
            ),
            p(
              "The central research question is: what drives defence spending",
              "among NATO-EU member states in the post-Cold War period?",
              "The study constructs a novel threat proximity measure from",
              "georeferenced conflict event data (UCDP GED 25.1) and estimates",
              "its effect using spatial autoregressive (SAR) panel models."
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Data Sources and Variables",
            icon  = bsicons::bs_icon("database"),
            p("Variables available in this application:"),
            tags$table(
              class = "table table-sm table-striped",
              tags$thead(
                tags$tr(
                  tags$th("Variable"),
                  tags$th("Description"),
                  tags$th("Source"),
                  tags$th("Coverage")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Defence Spending (% GDP)"),
                  tags$td("Military expenditure as % of GDP"),
                  tags$td("World Bank WDI / SIPRI"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Government Debt (% GDP)"),
                  tags$td("General government gross debt"),
                  tags$td("IMF World Economic Outlook"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Fiscal Deficit (% GDP)"),
                  tags$td("Net lending/borrowing (negative = deficit)"),
                  tags$td("IMF World Economic Outlook"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("GDP per Capita (EUR)"),
                  tags$td("GDP per capita, current prices"),
                  tags$td("Eurostat / WDI (GB)"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("GDP Growth Rate (%)"),
                  tags$td("Real GDP growth, % change on previous year"),
                  tags$td("Eurostat / WDI (GB)"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Immigration Rate (per 1000)"),
                  tags$td("Annual immigration per 1000 population"),
                  tags$td("Eurostat migr_imm1ctz"),
                  tags$td("23 countries, 2000-2023")
                ),
                tags$tr(
                  tags$td("Threat Score (log)"),
                  tags$td("All state-based conflict, spatially decayed"),
                  tags$td("UCDP GED 25.1"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Threat Score Land (log)"),
                  tags$td("Land-contiguous conflict only, spatially decayed"),
                  tags$td("UCDP GED 25.1"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Government Left-Right Position"),
                  tags$td("Seat-weighted cabinet left-right (0=left, 10=right)"),
                  tags$td("ParlGov"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Government EU Position"),
                  tags$td("Seat-weighted cabinet EU integration (0=anti, 10=pro)"),
                  tags$td("ParlGov"),
                  tags$td("24 countries, 1995-2023")
                ),
                tags$tr(
                  tags$td("Election Year"),
                  tags$td("1 if parliamentary election occurred, 0 otherwise"),
                  tags$td("ParlGov"),
                  tags$td("24 countries, 1995-2023")
                )
              )
            ),
            br(),
            p(
              tags$strong("Note on Great Britain:"),
              "GB is included in the panel but excluded from primary regression",
              "models that include immigration_rate, because Eurostat carries no",
              "immigration data for GB. This is a deliberate modelling decision.",
              "As an island nation, the land-border threat measure systematically",
              "underestimates GB's threat environment. GB's mean threat score is",
              "40% below the sample mean while its mean defence spending is 47%",
              "above — the opposite of the theory-predicted direction."
            ),
            p(
              tags$strong("Note on immigration_rate:"),
              "Eurostat immigration data (migr_imm1ctz) is available from 2000",
              "onwards. Years 1995-1999 have NA immigration_rate for all countries.",
              "Regime 1 (1995-2004) is therefore estimated on 2000-2004 only in",
              "models that include immigration_rate."
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Tab: Time Series",
            icon  = bsicons::bs_icon("graph-up"),
            p(
              "The Time Series tab shows the evolution of any panel variable",
              "over time for selected countries."
            ),
            tags$ul(
              tags$li(
                tags$strong("Variable:"),
                "Select any variable from the dropdown. All 11 panel variables",
                "are available."
              ),
              tags$li(
                tags$strong("Countries:"),
                "Select one or more countries. Deselect all to show all countries."
              ),
              tags$li(
                tags$strong("Year range:"),
                "Drag the slider to restrict the time window."
              ),
              tags$li(
                tags$strong("Regime bands:"),
                "Coloured background bands mark the four analytical regimes:",
                tags$ul(
                  tags$li(tags$span(style = "color:#4E79A7", "\u25A0"), " Regime 1 (1995-2004): Post-Cold War consolidation"),
                  tags$li(tags$span(style = "color:#F28E2B", "\u25A0"), " Regime 2 (2005-2013): Pre-crisis stability"),
                  tags$li(tags$span(style = "color:#E15759", "\u25A0"), " Regime 3 (2014-2021): Post-Crimea rearmament"),
                  tags$li(tags$span(style = "color:#76B7B2", "\u25A0"), " Regime 4 (2022-2023): Post-Ukraine invasion surge")
                )
              ),
              tags$li(
                tags$strong("Source overlay:"),
                "Only available for Defence Spending. Colours lines by data",
                "source (WDI, Eurostat, IMF WEO) to show where each country's",
                "data comes from."
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Tab: Map",
            icon  = bsicons::bs_icon("map"),
            p(
              "The Map tab shows a choropleth of any variable for a selected year."
            ),
            tags$ul(
              tags$li(
                tags$strong("Variable and Year:"),
                "Select any variable and drag the year slider."
              ),
              tags$li(
                tags$strong("Colour palette:"),
                "Sequential (Blues) is appropriate for variables that are always",
                "positive (defence spending, debt). Diverging (RdBu) is",
                "appropriate for variables that can be positive or negative",
                "(fiscal deficit, GDP growth)."
              ),
              tags$li(
                tags$strong("Conflict events overlay:"),
                "When enabled, circles show UCDP GED state-based conflict events",
                "for the selected year. Circle size is proportional to",
                "log(fatalities + 1). Red circles are land-contiguous events",
                "(used in the primary threat measure); grey circles are",
                "sea-separated events (excluded from the primary measure but",
                "included in the robustness variant)."
              ),
              tags$li(
                tags$strong("Countries shown:"),
                "The map shows all EU member states plus GB for geographic",
                "context. Countries not in the regression sample (Luxembourg,",
                "Austria, Cyprus, Ireland, Malta, Sweden) appear on the map",
                "but will show NA for regression-derived variables."
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Tab: Scatter",
            icon  = bsicons::bs_icon("scatter-chart"),
            p(
              "The Scatter tab shows a cross-sectional scatter plot for a",
              "selected year."
            ),
            tags$ul(
              tags$li(
                tags$strong("X and Y variables:"),
                "Select any two panel variables. The scatter shows one point",
                "per country for the selected year."
              ),
              tags$li(
                tags$strong("Colour by:"),
                "Colour points by analytical regime, country ISO2 code, or",
                "data source."
              ),
              tags$li(
                tags$strong("Fit line:"),
                "Adds an OLS regression line through the cross-section.",
                "This is a simple bivariate fit for visual reference only,",
                "not the panel regression results."
              ),
              tags$li(
                tags$strong("Labels:"),
                "Toggle country ISO2 labels on or off."
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Tab: Outliers",
            icon  = bsicons::bs_icon("exclamation-triangle"),
            p(
              "The Outliers tab flags unusual observations using two methods."
            ),
            tags$ul(
              tags$li(
                tags$strong("IQR method:"),
                "A value is flagged if it falls below Q1 - 3*IQR or above",
                "Q3 + 3*IQR, where IQR is computed globally across all",
                "countries and years. This detects extreme values relative",
                "to the full distribution. The IQR multiplier can be adjusted",
                "— higher values are more lenient."
              ),
              tags$li(
                tags$strong("Year-on-year jump method:"),
                "For each country, the mean and standard deviation of absolute",
                "year-on-year changes are computed. A value is flagged if its",
                "absolute change from the previous year exceeds",
                "mean + 3*SD. This detects sudden jumps that may indicate",
                "data revisions or source switches."
              ),
              tags$li(
                tags$strong("Both:"),
                "A value is flagged if it is flagged by either method."
              ),
              tags$li(
                tags$strong("Interpretation:"),
                "Flagged observations are not automatically excluded from the",
                "regression. Cook's distance influence diagnostics (script 07)",
                "are used to assess whether flagged observations drive results.",
                "Bulgaria 2019 and Greece 2022 are the highest-leverage",
                "observations in the primary regression."
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Tab: Unit Root",
            icon  = bsicons::bs_icon("activity"),
            h4("What is a unit root?"),
            p(
              "A time series has a unit root if shocks to it are permanent",
              "rather than temporary. A series with a unit root is called",
              tags$strong("non-stationary"),
              "— it has no fixed mean and its variance grows over time.",
              "Regressing non-stationary variables on each other produces",
              tags$strong("spurious regression:"),
              "high R-squared and significant coefficients even when the",
              "variables are unrelated. For panel data, this matters because",
              "if defence spending or threat scores are non-stationary,",
              "standard OLS inference is invalid unless the variables are",
              "cointegrated."
            ),
            hr(),
            h4("Tests performed"),
            tags$dl(
              tags$dt("ADF — Augmented Dickey-Fuller test (per country)"),
              tags$dd(
                "Tests the null hypothesis that a series has a unit root",
                "(is non-stationary) against the alternative that it is",
                "stationary. Applied separately to each country's time series",
                "for each variable.",
                tags$ul(
                  tags$li("Null H0: unit root present (non-stationary)"),
                  tags$li("Alternative H1: stationary"),
                  tags$li("p < 0.05: reject H0 — series is stationary"),
                  tags$li("p >= 0.05: fail to reject H0 — unit root not ruled out")
                )
              ),
              tags$dt("KPSS — Kwiatkowski-Phillips-Schmidt-Shin test (per country)"),
              tags$dd(
                "Tests the opposite null: that the series is stationary.",
                "Complements ADF because ADF has low power in short series",
                "(T = 29 years per country here).",
                tags$ul(
                  tags$li("Null H0: stationary"),
                  tags$li("Alternative H1: unit root present"),
                  tags$li("p < 0.05: reject H0 — series is non-stationary"),
                  tags$li("p >= 0.05: fail to reject H0 — stationarity not ruled out")
                )
              ),
              tags$dt("IPS — Im-Pesaran-Shin panel unit root test (per variable)"),
              tags$dd(
                "Pools ADF statistics across all countries for a given variable.",
                "More powerful than country-by-country ADF because it uses the",
                "full panel. Allows heterogeneous dynamics across countries.",
                tags$ul(
                  tags$li("Null H0: all panels have a unit root"),
                  tags$li("Alternative H1: some panels are stationary"),
                  tags$li("p < 0.05: reject H0 — at least some countries have stationary series")
                )
              )
            ),
            hr(),
            h4("How to read the heatmap"),
            p(
              "Each cell shows the result for one country-variable combination."
            ),
            tags$table(
              class = "table table-sm table-bordered",
              style = "max-width: 500px;",
              tags$thead(
                tags$tr(
                  tags$th("Colour"),
                  tags$th("Label"),
                  tags$th("Meaning")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$span(style = "color:#59A14F; font-size:1.4em;", "\u25A0")),
                  tags$td("S"),
                  tags$td("Stationary — ADF rejects unit root AND KPSS does not reject stationarity")
                ),
                tags$tr(
                  tags$td(tags$span(style = "color:#F28E2B; font-size:1.4em;", "\u25A0")),
                  tags$td("M"),
                  tags$td("Mixed — ADF and KPSS give conflicting results")
                ),
                tags$tr(
                  tags$td(tags$span(style = "color:#E15759; font-size:1.4em;", "\u25A0")),
                  tags$td("U"),
                  tags$td("Unit root — ADF does not reject unit root AND KPSS rejects stationarity")
                ),
                tags$tr(
                  tags$td(tags$span(style = "color:#d3d3d3; font-size:1.4em;", "\u25A0")),
                  tags$td("?"),
                  tags$td("Missing — fewer than 10 observations, test not run")
                )
              )
            ),
            p(
              tags$strong("Both mode (recommended):"),
              "A cell is green only if both ADF and KPSS agree the series is",
              "stationary. This is the most conservative and reliable classification."
            ),
            hr(),
            h4("What the results mean for the regression"),
            tags$dl(
              tags$dt("defence_gdp"),
              tags$dd(
                "If non-stationary, the two-way fixed effects model absorbs",
                "much of the trend via year fixed effects. A first-difference",
                "SAR (FD SAR) is estimated as a robustness check and reported",
                "in the paper."
              ),
              tags$dt("threat_land_log"),
              tags$dd(
                "Many country series are zero for long stretches (no nearby",
                "conflict), which makes unit root tests unreliable for this",
                "variable. The IPS panel test is more informative than",
                "country-by-country ADF here."
              ),
              tags$dt("debt_gdp"),
              tags$dd(
                "Typically I(1) (non-stationary) in most countries. The",
                "regression controls for this via country fixed effects which",
                "absorb country-specific trends."
              ),
              tags$dt("Mixed results"),
              tags$dd(
                "Common in short panels (T = 29 years). The regression",
                "proceeds with levels and two-way fixed effects as the primary",
                "specification, with first-difference SAR as robustness.",
                "The persistence vs diffusion decomposition (Check A in the",
                "revision checks) provides additional evidence on whether",
                "the spatial lag reflects genuine diffusion or spending inertia."
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Regression Sample",
            icon  = bsicons::bs_icon("people"),
            p("The primary regression sample contains 22 countries and 530 observations."),
            tags$table(
              class = "table table-sm table-striped",
              style = "max-width: 600px;",
              tags$thead(
                tags$tr(
                  tags$th("Country"),
                  tags$th("ISO2"),
                  tags$th("Status")
                )
              ),
              tags$tbody(
                tags$tr(tags$td("Belgium"),        tags$td("BE"), tags$td("In sample")),
                tags$tr(tags$td("Bulgaria"),       tags$td("BG"), tags$td("In sample")),
                tags$tr(tags$td("Croatia"),        tags$td("HR"), tags$td("In sample")),
                tags$tr(tags$td("Czech Republic"), tags$td("CZ"), tags$td("In sample")),
                tags$tr(tags$td("Denmark"),        tags$td("DK"), tags$td("In sample")),
                tags$tr(tags$td("Estonia"),        tags$td("EE"), tags$td("In sample")),
                tags$tr(tags$td("Finland"),        tags$td("FI"), tags$td("In sample")),
                tags$tr(tags$td("France"),         tags$td("FR"), tags$td("In sample")),
                tags$tr(tags$td("Germany"),        tags$td("DE"), tags$td("In sample")),
                tags$tr(tags$td("Greece"),         tags$td("GR"), tags$td("In sample")),
                tags$tr(tags$td("Hungary"),        tags$td("HU"), tags$td("In sample")),
                tags$tr(tags$td("Italy"),          tags$td("IT"), tags$td("In sample")),
                tags$tr(tags$td("Latvia"),         tags$td("LV"), tags$td("In sample")),
                tags$tr(tags$td("Lithuania"),      tags$td("LT"), tags$td("In sample")),
                tags$tr(tags$td("Netherlands"),    tags$td("NL"), tags$td("In sample")),
                tags$tr(tags$td("Norway"),         tags$td("NO"), tags$td("In sample")),
                tags$tr(tags$td("Poland"),         tags$td("PL"), tags$td("In sample")),
                tags$tr(tags$td("Portugal"),       tags$td("PT"), tags$td("In sample")),
                tags$tr(tags$td("Romania"),        tags$td("RO"), tags$td("In sample")),
                tags$tr(tags$td("Slovakia"),       tags$td("SK"), tags$td("In sample")),
                tags$tr(tags$td("Slovenia"),       tags$td("SI"), tags$td("In sample")),
                tags$tr(tags$td("Spain"),          tags$td("ES"), tags$td("In sample")),
                tags$tr(tags$td("Great Britain"),  tags$td("GB"), tags$td("In panel, excluded from primary models (structural outlier)")),
                tags$tr(tags$td("Luxembourg"),     tags$td("LU"), tags$td("In panel, excluded (defence/GDP < 0.2% throughout)"))
              )
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Threat Measure",
            icon  = bsicons::bs_icon("bullseye"),
            p(
              "The threat proximity score for country c in year t is:"
            ),
            withMathJax(
              helpText(
                "$$\\text{threat}(c, t) = \\sum_{e} \\left[ \\log(\\text{fatalities}_e + 1) \\cdot \\exp\\left(-\\frac{d(c,e)}{500}\\right) \\right]$$"
              )
            ),
            tags$ul(
              tags$li(
                "e indexes state-based conflict events (UCDP GED type_of_violence = 1)",
                "in year t that pass the land-contiguity filter"
              ),
              tags$li(
                "fatalities_e is the best estimate of battle deaths from UCDP GED"
              ),
              tags$li(
                "d(c, e) is the distance in km from the nearest point on country c's",
                "border polygon to event e, computed in ETRS89-LAEA projection (EPSG:3035)"
              ),
              tags$li(
                "The 500 km bandwidth means an event at 500 km contributes",
                "exp(-1) \u2248 37% of its log-fatality weight; at 1000 km approximately 14%"
              )
            ),
            p(
              tags$strong("Land-contiguity filter:"),
              "An event is classified as land-contiguous if the straight-line path",
              "from the event to the nearest point on the EU external land border",
              "crosses no more than 50 km of open sea. This excludes clearly",
              "sea-separated conflicts (e.g. North Africa across the Mediterranean,",
              "minimum ~150 km sea crossing) while accommodating narrow straits",
              "(e.g. Danish straits, ~8 km)."
            ),
            p(
              tags$strong("Primary variable:"),
              "threat_land_log = log(threat_land + 1), land-contiguous events only.",
              tags$br(),
              tags$strong("Robustness variable:"),
              "threat_score_log = log(threat_score + 1), all state-based events."
            )
          ),

          # ------------------------------------------------------------------
          bslib::accordion_panel(
            title = "Citation",
            icon  = bsicons::bs_icon("journal-text"),
            p("If you use this application or the underlying data, please cite:"),
            tags$blockquote(
              style = "border-left: 4px solid #dee2e6; padding-left: 1em; color: #6c757d;",
              p("Threat Proximity and Defence Spending in NATO-EU Member States, 1995-2023."),
              p(a("pub.e-dnrs.org", href = "https://pub.e-dnrs.org", target = "_blank"))
            ),
            p("Data sources:"),
            tags$ul(
              tags$li(
                "UCDP GED 25.1: Davies, S. et al. (2025). UCDP Georeferenced Event Dataset.",
                a("ucdp.uu.se", href = "https://ucdp.uu.se", target = "_blank")
              ),
              tags$li(
                "ParlGov: Döring, H. and Manow, P. (2024). Parliaments and Governments Database.",
                a("parlgov.org", href = "http://www.parlgov.org", target = "_blank")
              ),
              tags$li(
                "Defence spending: World Bank WDI / SIPRI Military Expenditure Database."
              ),
              tags$li(
                "Fiscal data: IMF World Economic Outlook."
              ),
              tags$li(
                "GDP and migration: Eurostat."
              )
            )
          )
        ),
        br()
      )
    )
  )
}

mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
