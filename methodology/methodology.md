# Methodology

## 1. Research Question and Thesis Statement

This study examines the determinants of defence spending among NATO-EU member states over the period 1995-2023. The central hypothesis is that threat proximity — measured as a spatially-decayed, fatality-weighted index of nearby conflict events — is a significant positive predictor of defence spending, conditional on fiscal constraints and government political orientation.

A secondary hypothesis is that the threat-defence relationship strengthened after 2014 (Crimea annexation) and again after 2022 (Ukraine invasion), reflecting the shift from post-Cold War consolidation to active rearmament.

The study situates itself within the NATO burden-sharing literature, which frames collective defence as a public goods problem in which member states have incentives to free-ride on the security provided by larger allies.[^methodology-1]

[^methodology-1]: Olson, M. and Zeckhauser, R. (1966). An economic theory of alliances. *Review of Economics and Statistics*, 48(3), 266-279.

------------------------------------------------------------------------

## 2. Sample and Data

### 2.1 Country Sample

The primary sample consists of 24 countries: EU member states that are also NATO members as of the study period, plus Norway (NATO non-EU) and Great Britain (NATO, left EU 2020 but included for the full 1995-2023 period).

Countries intentionally excluded:

| Country    | Reason                                                     |
|------------|------------------------------------------------------------|
| Austria    | EU member, NATO non-member (permanent neutrality)          |
| Cyprus     | EU member, NATO non-member (political dispute with Turkey) |
| Ireland    | EU member, NATO non-member (permanent neutrality)          |
| Malta      | EU member, NATO non-member (permanent neutrality)          |
| Sweden     | NATO member only from March 2024, outside study period     |
| Luxembourg | Structural outlier: defence/GDP below 0.2% throughout      |

Greece is coded as GR throughout. Eurostat uses EL internally; all download scripts recode EL to GR before filtering.

Great Britain is included in the panel but excluded from primary regression models that include immigration_rate, because immigration_rate is NA for all GB years. This is a deliberate modelling decision, not a data gap. As an island nation, the land-border threat measure systematically underestimates GB's threat environment, while GB's defence commitments reflect global power projection rather than European territorial defence. Empirically, GB's mean threat score is 40% below the sample mean while its mean defence spending is 47% above the sample mean — the opposite of the theory-predicted direction.

### 2.2 Dependent Variable

Military expenditure as a percentage of GDP (defence_gdp). Source: World Bank WDI indicator MS.MIL.XPND.GD.ZS, which draws on SIPRI data. Eurostat COFOG GF02 was considered but rejected because it is missing France, Germany, and Great Britain entirely, and Norway values are systematically underreported.

### 2.3 Independent Variables

| Variable | Description | Source |
|----|----|----|
| threat_land_log | Land-contiguous threat score (log) | UCDP GED 25.1 |
| debt_gdp | General government gross debt (% GDP) | IMF WEO GGXWDG_NGDP |
| deficit_gdp | Net lending/borrowing (% GDP, negative = deficit) | IMF WEO GGXCNL_NGDP |
| gdp_growth | Real GDP growth (%) | Eurostat CLV_PCH_PRE |
| gdp_pc | GDP per capita (EUR) | Eurostat CP_EUR_HAB |
| immigration_rate | Annual immigration per 1000 population | Eurostat migr_imm1ctz |
| gov_left_right | Seat-weighted cabinet left-right position (0-10) | ParlGov |
| gov_eu_position | Seat-weighted cabinet EU integration position (0-10) | ParlGov |
| election_year | Binary flag for parliamentary election years | ParlGov |

------------------------------------------------------------------------

## 3. Threat Measure Construction

### 3.1 Conceptual Motivation

Standard threat measures in the literature use alliance-level aggregates or binary conflict indicators. This study constructs a continuous, country-specific, time-varying threat proximity score that captures both the intensity of nearby conflict (fatalities) and the geographic decay of threat perception with distance.

### 3.2 Data Source

UCDP Georeferenced Event Dataset (GED) version 25.1, filtered to state-based conflict only (type_of_violence = 1). Non-state conflict and one-sided violence are excluded because they do not represent the interstate or civil war threat environment that drives NATO defence spending decisions. The geographic filter covers the European theatre bounding box (latitude 30N-72N, longitude 25W-45E).

### 3.3 Land-Contiguity Filter

An event is classified as land-contiguous if the straight-line path from the event location to the nearest point on the EU external land border crosses no more than 50 km of open sea. The 50 km threshold accommodates narrow straits (e.g. the Danish straits, approximately 8 km) while excluding clearly sea-separated theatres (e.g. North Africa across the Mediterranean, minimum approximately 150 km sea crossing).

Sea crossing is measured by intersecting the straight-line path with an ocean mask derived from Natural Earth land polygons. Events that fail the land-contiguity filter are retained in the robustness variant (threat_score_log) but excluded from the primary measure (threat_land_log).

### 3.4 Threat Formula

The threat score for country c in year t is:

$$\text{threat}(c, t) = \sum_{e} \left[ \log(\text{fatalities}_e + 1) \cdot \exp\left(-\frac{d(c,e)}{500}\right) \right]$$

Where:

- e indexes conflict events in year t that pass the land-contiguity filter
- fatalities_e is the best estimate of battle deaths from UCDP GED
- d(c, e) is the distance in km from the nearest point on country c's border polygon to event e
- The spatial decay bandwidth of 500 km means an event at 500 km contributes exp(-1) approximately 37% of its log-fatality weight; at 1000 km approximately 14%
- log(fatalities + 1) compresses the fatality distribution and handles zero-fatality events

### 3.5 Distance Computation

All distances are computed after projecting to ETRS89-LAEA (EPSG:3035), a metric equal-area projection for Europe. Border-polygon distances are used rather than centroid distances, which are more accurate for large or non-convex countries such as France and Norway.

### 3.6 Normalisation

The primary variable threat_land_log = log(threat_land + 1). A robustness variant threat_score_log uses the same formula without the land-contiguity filter. Both variants are standardised to z-scores for sensitivity analysis.

------------------------------------------------------------------------

## 4. Analytical Regimes

Four analytical regimes are defined based on structural breaks in the international security environment:

| Regime | Years     | Label                       |
|--------|-----------|-----------------------------|
| 1      | 1995-2004 | Post-Cold War consolidation |
| 2      | 2005-2013 | Pre-crisis stability        |
| 3      | 2014-2021 | Post-Crimea rearmament      |
| 4      | 2022-2023 | Post-Ukraine invasion surge |

Regime boundaries are validated empirically using Chow tests at 2003, 2014, and 2022, the Bai-Perron supF test, and AIC/BIC comparison across regime specifications. The four-regime specification is preferred over data-driven alternatives on both theoretical and statistical grounds (AIC = 658.5 vs 664.8 for the no-regime baseline).

------------------------------------------------------------------------

## 5. Spatial Weight Matrices

Three weight matrices are constructed for the regression sample (23 countries, Luxembourg excluded):

### 5.1 Queen Contiguity (primary)

Two countries are neighbours if their border polygons share at least one point. Row-standardised so each row sums to 1. Finland has no land border with other sample countries and receives distance-based neighbours within 2000 km as a fallback. Sensitivity to this decision is tested in model M10a (SAR excluding Finland).

### 5.2 Inverse Distance (robustness)

Weight between countries i and j: 1/d(i,j) if d(i,j) \<= 2000 km, else 0. Row-standardised. Distances computed between country centroids in ETRS89-LAEA projection.

### 5.3 Distance Band (robustness)

Binary: 1 if 0 \< d(i,j) \<= 1000 km, else 0. Row-standardised. Used for Moran's I robustness check alongside queen W.

### 5.4 Block-Diagonal Construction

For spatial panel models, a block-diagonal W is constructed by stacking per-year weight matrices. This applies the spatial structure within each year while maintaining the panel dimension. Isolated observations (zero row-sum after subsetting) are removed rather than self-weighted.

------------------------------------------------------------------------

## 6. Model Specifications

### 6.1 Baseline OLS

Four baseline models are estimated without spatial components:

M1 — Pooled OLS (no fixed effects)

M2 — Country fixed effects (within estimator)

M3 — Two-way fixed effects (country and year), primary baseline:

$$\text{defence}_{ct} = \alpha_c + \delta_t + \beta_1 \text{threat}_{ct} + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

M4 — Two-way fixed effects with regime interactions:

$$\text{defence}_{ct} = \alpha_c + \delta_t + \sum_{r=1}^{4} \beta_r (\text{threat}_{ct} \times \mathbb{1}[\text{regime}_t = r]) + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

Where alpha_c are country fixed effects, delta_t are year fixed effects, and X_ct is the vector of controls: debt, deficit, GDP growth, immigration rate, government left-right, government EU position, and election year.

Standard errors use Driscoll-Kraay (HC3), robust to cross-sectional dependence and heteroskedasticity. HC3 is preferred over HC1 for small cluster counts (N = 22 countries).

### 6.2 Spatial Autocorrelation Tests

Before estimating spatial models, spatial autocorrelation in two-way FE residuals is tested using Moran's I (computed on M3 residuals, not pooled OLS residuals, to avoid conflating panel heterogeneity with spatial signal) and Robust Score (RS) tests per year.[^methodology-2]

[^methodology-2]: Anselin, L., Bera, A., Florax, R. and Yoon, M. (1996). Simple diagnostic tests for spatial dependence. *Regional Science and Urban Economics*, 26(1), 77-104.

RS test decision rule: if adjRSlag \> adjRSerr, prefer the SAR model.

Results: Moran's I mean = -0.344 (queen W), 0% of years significant. RS tests favour SAR. LR test SAR vs SEM: stat = 3.44, p = 0.064, SAR preferred.

### 6.3 Spatial Lag Model — SAR (M5, primary spatial)

$$\text{defence}_{ct} = \alpha_c + \delta_t + \rho \mathbf{W} \text{defence}_{ct} + \beta_1 \text{threat}_{ct} + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

Where rho is the spatial autoregressive parameter capturing cross-country defence spending diffusion, and W is the block-diagonal queen contiguity weight matrix. Estimated via maximum likelihood using spatialreg::lagsarlm.

Primary results: rho = 0.177 (SE = 0.039, p \< 0.001), threat_land_log = 0.090 (SE = 0.023, p \< 0.001), deficit_gdp = -0.023 (SE = 0.005, p \< 0.001).

### 6.4 Spatial Error Model — SEM (M6)

$$\mathbf{u}_{ct} = \lambda \mathbf{W} \mathbf{u}_{ct} + \varepsilon_{ct}$$

Where lambda is the spatial error parameter capturing spatially correlated unobservables. SAR preferred over SEM by LR test.

### 6.5 Robustness Specifications

| Model | Variation                                          |
|-------|----------------------------------------------------|
| M7    | SAR with regime x threat interactions              |
| M8    | threat_score_log instead of threat_land_log        |
| M9    | Inverse distance W instead of queen W              |
| M10a  | Exclude Finland (sensitivity to distance fallback) |
| M10b  | Post-2014 subsample                                |
| M10c  | Pre-2014 subsample                                 |
| M12   | SAR with lagged dependent variable                 |

------------------------------------------------------------------------

## 7. Multicollinearity and Coefficient Stability

VIF analysis on the two-way FE OLS reveals severe collinearity for threat_land_log (VIF = 13.18) and debt_gdp (VIF = 11.08). All other variables have VIF below 5.

An orthogonalisation check regresses threat_land_log on debt_gdp and uses the residuals as an orthogonalised threat measure. Fiscal coefficients are stable across original and orthogonalised specifications, confirming that the threat coefficient is not an artefact of collinearity with debt.

------------------------------------------------------------------------

## 8. Structural Break Analysis

Primary evidence for break year selection:

| Test | Break year | Statistic | p-value | Conclusion    |
|------|------------|-----------|---------|---------------|
| Chow | 2003       | F = 2.32  | 0.014   | Break present |
| Chow | 2014       | F = 1.76  | 0.074   | No break      |
| Chow | 2022       | F = 2.59  | 0.006   | Break present |

The four-regime specification (breaks at 2004, 2013, 2021) is preferred over the no-regime baseline on AIC (658.5 vs 664.8) and BIC (726.8 vs 707.5).

Pre/post-2014 spatial lag asymmetry is tested by comparing rho from M10b (post-2014) and M10c (pre-2014) using a z-test. The gov_eu_position sign reversal pre/post-2014 is formally tested and documented.

------------------------------------------------------------------------

## 9. Persistence vs Diffusion Decomposition

A key diagnostic question is whether the spatial lag rho reflects genuine cross-country defence spending diffusion or is an artefact of temporal persistence:

| Specification      | rho    | SE    | p-value  | Interpretation                    |
|--------------------|--------|-------|----------|-----------------------------------|
| M5: Levels SAR     | 0.177  | 0.039 | \< 0.001 | Baseline spatial lag              |
| M12: Lagged DV SAR | 0.060  | 0.034 | 0.080    | After controlling for persistence |
| FD SAR             | -0.104 | 0.042 | 0.014    | After first-differencing          |

The spatial lag largely disappears after controlling for persistence (M12) and reverses sign in first differences (FD SAR), confirming that the spatial pattern reflects spending inertia rather than genuine cross-country diffusion.

------------------------------------------------------------------------

## 10. Great Britain as Structural Outlier

| Group               | N   | Mean threat | Mean defence (% GDP) |
|---------------------|-----|-------------|----------------------|
| All other countries | 667 | 0.986       | 1.60                 |
| Great Britain       | 29  | 0.595       | 2.35                 |

GB's mean threat score is 40% below the rest-of-sample mean while its mean defence spending is 47% above. This is the opposite of the theory-predicted direction, confirming structural incompatibility. Exclusion from primary regression models is theoretically grounded, not a data availability limitation.

------------------------------------------------------------------------

## References

- Olson, M. and Zeckhauser, R. (1966). An economic theory of alliances. *Review of Economics and Statistics*, 48(3), 266-279.
- Anselin, L., Bera, A., Florax, R. and Yoon, M. (1996). Simple diagnostic tests for spatial dependence. *Regional Science and Urban Economics*, 26(1), 77-104.
- Döring, H. and Manow, P. (2024). Parliaments and governments database (ParlGov): Information on parties, elections and cabinets in established democracies. [parlgov.org](http://www.parlgov.org)
- Davies, S. et al. (2025). UCDP Georeferenced Event Dataset (GED) Global version 25.1. Uppsala Conflict Data Program. [ucdp.uu.se](https://ucdp.uu.se)
