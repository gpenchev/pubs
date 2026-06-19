# Abbreviations

## Data Sources

| Abbreviation | Full Term | Context |
|----|----|----|
| WDI | World Development Indicators | World Bank data portal |
| SIPRI | Stockholm International Peace Research Institute | Defence spending data source |
| IMF WEO | International Monetary Fund World Economic Outlook | Fiscal data source |
| UCDP GED | Uppsala Conflict Data Program Georeferenced Event Dataset (v26.1) | Conflict event data — primary threat source |
| GPR | Geopolitical Risk Index (Caldara & Iacoviello 2022) | Media-perception threat measure — used in M13 |
| ParlGov | Parliaments and Governments Database | Political variables source |
| ECB | European Central Bank | EUR/USD exchange rate source |
| COFOG | Classification of the Functions of Government | Eurostat expenditure classification |

------------------------------------------------------------------------

## Country Codes (ISO 3166-1 alpha-2)

Countries are listed in three groups: (1) primary regression sample (22 countries), (2) panel download only, and (3) excluded by design.

### Regression sample (N = 22)

| Code | Country        | Notes |
|------|----------------|-------|
| BE   | Belgium        | |
| BG   | Bulgaria       | 2019 procurement spike (Cook's D = 0.099) — see diagnostics |
| CZ   | Czech Republic | |
| DE   | Germany        | |
| DK   | Denmark        | |
| EE   | Estonia        | |
| ES   | Spain          | |
| FI   | Finland        | Distance-based W fallback (no land border with sample countries) |
| FR   | France         | |
| GR   | Greece         | Eurostat code EL; recoded to GR in all scripts |
| HR   | Croatia        | |
| HU   | Hungary        | |
| IT   | Italy          | |
| LT   | Lithuania      | |
| LV   | Latvia         | |
| NL   | Netherlands    | |
| PL   | Poland         | |
| PT   | Portugal       | |
| RO   | Romania        | |
| SI   | Slovenia       | |
| SK   | Slovakia       | |
| GB   | Great Britain  | In panel download; excluded from regressions (island misfit — threat 39% below mean, spending 47% above) |

### Panel download only (not in regressions)

| Code | Country | Reason excluded from regressions |
|------|---------|----------------------------------|
| NO   | Norway  | Insufficient Eurostat fiscal coverage for regression controls |
| LU   | Luxembourg | Structural outlier (defence/GDP below 0.2% throughout) |

### Excluded by design (not in panel)

| Code | Country | Reason |
|------|---------|--------|
| AT   | Austria  | EU member, NATO non-member (permanent neutrality) |
| CY   | Cyprus   | EU member, NATO non-member (political dispute with Turkey) |
| IE   | Ireland  | EU member, NATO non-member (permanent neutrality) |
| MT   | Malta    | EU member, NATO non-member (permanent neutrality) |
| SE   | Sweden   | NATO accession March 2024 — outside study period (1995–2023) |

------------------------------------------------------------------------

## Model Names

| Abbreviation | Full Term | Context |
|----|----|----|
| OLS | Ordinary Least Squares | Baseline regression |
| FE | Fixed Effects | Panel estimator |
| SAR | Spatial Autoregressive Model | Spatial lag model |
| FD SAR | First-Difference Spatial Autoregressive Model | Persistence vs diffusion decomposition |
| SEM | Spatial Error Model | Spatial error model |
| LR | Likelihood Ratio | Model comparison test |
| AIC | Akaike Information Criterion | Model selection |
| BIC | Bayesian Information Criterion | Model selection |
| VIF | Variance Inflation Factor | Multicollinearity diagnostic |
| DK | Driscoll-Kraay standard errors | Robust to cross-sectional dependence and heteroskedasticity |
| HC3 | Heteroskedasticity-Consistent (type 3) | Robust standard errors — used with DK in M1–M4 |
| IPS | Im-Pesaran-Shin test | Panel unit root test |
| ADF | Augmented Dickey-Fuller test | Unit root test |
| KPSS | Kwiatkowski-Phillips-Schmidt-Shin test | Stationarity test |
| RS test | Robust Score test | Spatial model selection |
| supF | Supremum F-statistic | Structural break test |

------------------------------------------------------------------------

## Variables

| Variable | Full Term | Unit |
|----|----|----|
| defence_gdp | Military expenditure | % of GDP |
| threat_land_log | Land-contiguous threat score (log) | Dimensionless |
| threat_score_log | All-conflict threat score (log, no sea filter) | Dimensionless |
| gpr | Country-specific Geopolitical Risk Index annual mean | Dimensionless (used in M13) |
| debt_gdp | General government gross debt | % of GDP |
| deficit_gdp | Net lending / net borrowing | % of GDP |
| gdp_pc | GDP per capita | EUR, current prices |
| gdp_growth | Real GDP growth rate | % change on previous year |
| immigration_rate | Annual immigration | Per 1000 population |
| gov_left_right | Government left-right position | 0 (far left) to 10 (far right) |
| gov_eu_position | Government EU integration position | 0 (anti-EU) to 10 (pro-EU) |
| election_year | Parliamentary election year flag | 0 or 1 |
| regime | Geopolitical regime factor (1–4) | 1=1995–2004, 2=2005–2013, 3=2014–2021, 4=2022–2023 |
| rho | Spatial autoregressive parameter (ρ) | Dimensionless |
| lambda | Spatial error parameter (λ) | Dimensionless |

------------------------------------------------------------------------

## Spatial Terms

| Abbreviation | Full Term | Context |
|----|----|----|
| ETRS89-LAEA | European Terrestrial Reference System 1989, Lambert Azimuthal Equal Area | Metric projection for Europe |
| EPSG:3035 | European Petroleum Survey Group code for ETRS89-LAEA | Projection identifier |
| W_queen | Queen contiguity weight matrix | Primary spatial weights |
| W_inv_dist | Inverse distance weight matrix | Robustness spatial weights |
| W_dist_band | Distance band weight matrix (1000 km) | Robustness spatial weights |
| IQR | Interquartile Range | Outlier detection |
