# Abbreviations

## Data Sources

| Abbreviation | Full Term | Context |
|----|----|----|
| WDI | World Development Indicators | World Bank data portal |
| SIPRI | Stockholm International Peace Research Institute | Defence spending data source |
| IMF WEO | International Monetary Fund World Economic Outlook | Fiscal data source |
| UCDP GED | Uppsala Conflict Data Program Georeferenced Event Dataset | Conflict event data |
| ParlGov | Parliaments and Governments Database | Political variables source |
| ECB | European Central Bank | EUR/USD exchange rate source |
| COFOG | Classification of the Functions of Government | Eurostat expenditure classification |

------------------------------------------------------------------------

## Country Codes (ISO 3166-1 alpha-2)

| Code | Country        |
|------|----------------|
| BE   | Belgium        |
| BG   | Bulgaria       |
| CZ   | Czech Republic |
| DE   | Germany        |
| DK   | Denmark        |
| EE   | Estonia        |
| ES   | Spain          |
| FI   | Finland        |
| FR   | France         |
| GB   | Great Britain  |
| GR   | Greece         |
| HR   | Croatia        |
| HU   | Hungary        |
| IT   | Italy          |
| LT   | Lithuania      |
| LU   | Luxembourg     |
| LV   | Latvia         |
| NL   | Netherlands    |
| NO   | Norway         |
| PL   | Poland         |
| PT   | Portugal       |
| RO   | Romania        |
| SI   | Slovenia       |
| SK   | Slovakia       |

------------------------------------------------------------------------

## Model Names

| Abbreviation | Full Term | Context |
|----|----|----|
| OLS | Ordinary Least Squares | Baseline regression |
| FE | Fixed Effects | Panel estimator |
| SAR | Spatial Autoregressive Model | Spatial lag model |
| SEM | Spatial Error Model | Spatial error model |
| LR | Likelihood Ratio | Model comparison test |
| AIC | Akaike Information Criterion | Model selection |
| BIC | Bayesian Information Criterion | Model selection |
| VIF | Variance Inflation Factor | Multicollinearity diagnostic |
| HC3 | Heteroskedasticity-Consistent (type 3) | Robust standard errors |
| IPS | Im-Pesaran-Shin test | Panel unit root test |
| ADF | Augmented Dickey-Fuller test | Unit root test |
| KPSS | Kwiatkowski-Phillips-Schmidt-Shin test | Stationarity test |
| RS test | Robust Score test | Spatial model selection |
| supF | Supremum F-statistic | Structural break test |

------------------------------------------------------------------------

## Variables

| Variable | Full Term | Unit |
|----|----|----|
| defence_gdp | Military expenditure | \% of GDP |
| threat_land_log | Land-contiguous threat score (log) | Dimensionless |
| threat_score_log | All-conflict threat score (log) | Dimensionless |
| debt_gdp | General government gross debt | \% of GDP |
| deficit_gdp | Net lending / net borrowing | \% of GDP |
| gdp_pc | GDP per capita | EUR, current prices |
| gdp_growth | Real GDP growth rate | \% change on previous year |
| immigration_rate | Annual immigration | Per 1000 population |
| gov_left_right | Government left-right position | 0 (far left) to 10 (far right) |
| gov_eu_position | Government EU integration position | 0 (anti-EU) to 10 (pro-EU) |
| election_year | Parliamentary election year flag | 0 or 1 |
| rho | Spatial autoregressive parameter | Dimensionless |
| lambda | Spatial error parameter | Dimensionless |

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
