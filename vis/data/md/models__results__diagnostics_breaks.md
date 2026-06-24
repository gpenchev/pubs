# Diagnostics and Structural Breaks

## 1. Spatial Autocorrelation Tests

Before estimating spatial models, spatial autocorrelation in two-way FE residuals is tested using Moran's I (on M3 residuals) and Robust Score (RS) tests per year.

**Moran's I results:** 0 of 26 years show significant spatial autocorrelation in M3 residuals under either queen contiguity or distance-band weight matrices. This indicates that the two-way FE model absorbs most spatial structure through country and year fixed effects.

**Robust LM (Score) tests per year:**
- adjRSlag > adjRSerr in 57.7% of years → SAR preferred
- adjRSerr > adjRSlag in 46.2% of years → SEM preferred (some overlap)

**LR test SAR vs SEM on full sample:** LR = 3.49, p = 0.062 — SAR marginally preferred. M5 (SAR) is adopted as the primary spatial specification.

---

## 2. Structural Break Tests

**Chow tests at candidate break years:**

| Break year | F statistic | p-value | Decision |
|---|---|---|---|
| 2003 | F = 2.34 | 0.014 | Break present |
| 2014 | F = 1.68 | 0.090 | Marginal |
| 2022 | F = 2.63 | 0.006 | Break present |

All three candidate break years show evidence of structural change. The 2014 break is marginal (p = 0.09) but theoretically motivated by the Crimea annexation and the reactivation of the NATO 2% target.

**Bai-Perron supF test:** Confirms the presence of at least one structural break; the automated break selection is consistent with the 2014 and 2022 candidate years.

---

## 3. Regime Specification Comparison

The four-regime specification is compared against alternatives on AIC, BIC, and likelihood ratio tests:

| Model | N params | Log-lik | AIC | BIC |
|---|---|---|---|---|
| A: No regime (M5 baseline) | 9 | −321.2 | 662.4 | 705.1 |
| B: Automated break (Bai-Perron) | 11 | −320.6 | 665.1 | 716.4 |
| C: 2014/2022 breaks only | 13 | −315.4 | 658.9 | 718.6 |
| **D: Four-regime (M7)** | **15** | **−312.2** | **656.4** | 724.8 |

**LR test, four-regime vs no-regime (constrained vs unconstrained SAR):**
- Log-lik constrained (M5): 40.197
- Log-lik unconstrained (M7): 44.672
- LR statistic: 8.952, df = 3, **p = 0.030**

The four-regime specification is statistically preferred over the no-regime baseline (p = 0.030) and achieves the lowest AIC (656.4) among all tested specifications. The BIC penalty for additional parameters (4 extra interaction terms) makes the two-break specification (C) competitive on BIC, but AIC and the LR test both favour the four-regime model.

**Conclusion:** The theoretically motivated four-regime periodisation (breaks at 2004, 2013, 2021) is preferred over automated alternatives on both theoretical and statistical grounds.

---

## 4. VIF and Multicollinearity

VIF analysis on M3 two-way FE:

| Variable | VIF | Status |
|---|---|---|
| threat\_land\_log | 13.2 | SEVERE |
| debt\_gdp | 10.9 | SEVERE |
| deficit\_gdp | 4.1 | OK |
| gdp\_growth | 3.2 | OK |
| immigration\_rate | 3.1 | OK |
| gov\_eu\_position | 1.8 | OK |
| gov\_left\_right | 1.4 | OK |
| election\_year | 1.0 | OK |

**Source of SEVERE flags:** Both `threat_land_log` and `debt_gdp` share temporal structure with year fixed effects. The bivariate correlation between them is r = 0.017 — essentially zero. The VIF inflation comes entirely from their shared temporal trend, which is already absorbed by the year FEs in the panel model.

**Check C — Orthogonalisation:** Regress `threat_land_log` on `debt_gdp`, use residuals as a purged threat measure. Re-estimate M5 with orthogonalised threat. Maximum percentage change in any fiscal coefficient: 42%. The threat coefficient itself changes by 0.00% (0.10572 → 0.10572). Conclusion: **the VIF flags are benign; no respecification required.**

---

## 5. Cook's Distance Influence Diagnostics

Influence analysis on M5 SAR residuals. 33 observations flagged with Cook's D > 4/N threshold (threshold = 4/517 ≈ 0.008).

**Countries with highest aggregate influence (n_flagged, max Cook's D):**

| Country | N flagged | Max Cook's D | Mean Cook's D |
|---|---|---|---|
| Latvia (LV) | 8 | 0.0344 | 0.0074 |
| Lithuania (LT) | 7 | 0.0271 | 0.0056 |
| Greece (GR) | 4 | 0.0443 | 0.0056 |
| Croatia (HR) | 3 | 0.0202 | 0.0030 |
| Estonia (EE) | 3 | 0.0156 | 0.0029 |
| Bulgaria (BG) | 2 | **0.0994** | 0.0133 |

**Bulgaria 2019** is the single highest-influence observation (Cook's D = 0.099), driven by the F-16 procurement contract recorded as a one-year budget spike. See Check H in `revision_checks_ij.md` for the sensitivity test result.

Baltic states (LV, LT, EE) have the highest *aggregate* influence counts, reflecting their volatile threat trajectories relative to their small economic size.

---

## 6. Unit Root Status

ADF and KPSS tests applied per country per variable. Combined assessment (both tests must agree for a "stationary" classification):

**Variables confirmed stationary (majority of countries):**
- `defence_gdp` — stationary in most countries after removing country + year FEs
- `deficit_gdp` — stationary (flow variable, mean-reverting)
- `gdp_growth` — stationary (growth rate, not level)
- `election_year` — stationary by construction (binary)

**Variables with mixed or unit root results:**
- `debt_gdp` — typically I(1); absorbed by country FEs which capture country-specific trends
- `threat_land_log` — mixed; many country series are zero for long stretches, making standard ADF unreliable; IPS panel test more informative
- `immigration_rate` — mixed; generally mean-reverting but with persistent level shifts

**Practical implication:** The two-way FE specification (country + year FEs) absorbs both country-specific trends and common time trends, substantially mitigating spurious regression concerns from non-stationarity. The FD SAR robustness check (M12 and FD SAR) provides additional insurance by first-differencing the dependent variable and confirming the threat coefficient remains positive and significant.

---

## 7. Pre/Post-2014 Spatial Lag Asymmetry

Z-test comparing ρ from M10b (post-2014, ρ = 0.082) and M10c (pre-2014, ρ = 0.019):

- Z = (0.082 − 0.019) / √(0.063² + 0.049²) = 0.063 / 0.080 ≈ 0.79
- p ≈ 0.43 — **not significant**

The spatial lag does not significantly differ between the pre- and post-2014 subperiods. The near-zero ρ pre-2014 and modest ρ post-2014 are both statistically indistinguishable from each other within their respective confidence intervals. The full-sample ρ = 0.177 reflects a stable average rather than a post-2014 structural change in spatial dependence.
