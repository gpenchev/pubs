# Model Specifications

## Baseline OLS (M1–M4)

Four baseline models are estimated without spatial components, serving as benchmarks and for VIF diagnostics.

**M1 — Pooled OLS (no fixed effects)**
Simple pooled regression; ignores all panel structure. Included only for comparison; not interpretable as causal.

**M2 — Country fixed effects (within estimator)**
Removes cross-country variation. Identifies the effect of within-country changes in threat on within-country changes in defence spending.

**M3 — Two-way fixed effects (country + year) — primary OLS baseline:**

$$\text{defence}_{ct} = \alpha_c + \delta_t + \beta_1 \text{threat}_{ct} + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

Where α_c are country fixed effects, δ_t are year fixed effects, and **X**_ct is the vector of controls: debt, deficit, GDP growth, immigration rate, government left–right, government EU position, election year. Standard errors: Driscoll-Kraay (DK-HC3), robust to cross-sectional dependence and heteroskedasticity.

**M4 — Two-way fixed effects with regime × threat interactions:**

$$\text{defence}_{ct} = \alpha_c + \delta_t + \sum_{r=1}^{4} \beta_r (\text{threat}_{ct} \times \mathbb{1}[\text{regime}_t = r]) + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

Allows the threat elasticity to differ across the four analytical regimes. Regime 1 (1995–2004) is the baseline.

## Spatial Lag Model — SAR (M5, primary spatial)

$$\text{defence}_{ct} = \alpha_c + \delta_t + \rho \mathbf{W} \text{defence}_{ct} + \beta_1 \text{threat}_{ct} + \mathbf{X}_{ct}\boldsymbol{\gamma} + \varepsilon_{ct}$$

Where ρ is the spatial autoregressive parameter capturing cross-country defence spending diffusion, and **W** is the block-diagonal queen contiguity weight matrix (stacked by year). Estimated via maximum likelihood using `spatialreg::lagsarlm`.

**Primary results:** ρ = 0.177 (SE = 0.039, p < 0.001), β₁ (threat) = 0.088 (SE = 0.023, p < 0.001), β (deficit) = −0.023 (SE = 0.005, p < 0.001). N = 517 (12 observations dropped due to isolated nodes in the block-diagonal W).

## Spatial Error Model — SEM (M6)

$$\mathbf{u}_{ct} = \lambda \mathbf{W} \mathbf{u}_{ct} + \varepsilon_{ct}$$

Where λ is the spatial error parameter capturing spatially correlated unobservables. SAR is preferred over SEM by LR test (LR = 3.49, p = 0.062).

## Robustness Specifications (M7–M13)

| Model | Variation | Key finding |
|---|---|---|
| M7 | SAR with regime × threat interactions | Four-regime specification preferred by LR test (p = 0.030) |
| M8 | `threat_score_log` (all events, no land filter) | Threat coefficient stable: β = 0.082 |
| M9 | Inverse distance W matrix | ρ = 0.346; spatial signal larger with distance-based weights |
| M10a | Exclude Finland (no land-border fallback) | ρ = 0.221; Finland exclusion has minor effect |
| M10b | Post-2014 subsample | Threat non-significant (absorbed by year FE after 2022 universal shock) |
| M10c | Pre-2014 subsample | Threat β = 0.106 (p < 0.001) |
| M12 | SAR with lagged dependent variable | ρ drops to 0.061 (p = 0.077) — persistence decomposition |
| M13 | SAR with GPR instead of UCDP threat | ΔAIC = +17.6 vs M5; ρ reverses sign to −0.210 |

## Spatial Weight Matrices

Three **W** matrices are constructed for the spatial weight matrix sample (23 countries):

1. **Queen contiguity (primary):** Two countries are neighbours if border polygons share at least one point. Row-standardised. Finland has no land border — assigned distance-based neighbours within 2000 km as a fallback.
2. **Inverse distance (robustness):** Weight = 1/d(i,j) if d ≤ 2000 km, else 0. Row-standardised.
3. **Distance band (robustness):** Binary 1 if 0 < d ≤ 1000 km, else 0. Row-standardised. Used for Moran's I robustness checks.

For spatial panel models, a **block-diagonal W** is constructed by stacking per-year matrices. Isolated observations (zero row-sum) are removed rather than self-weighted — this accounts for the 12 dropped observations in M5.

## Analytical Regimes

| Regime | Years | Label | Rationale |
|---|---|---|---|
| 1 | 1995–2004 | Balkans Wars | Peak conflict events in sample |
| 2 | 2005–2013 | Austerity decade | EU fiscal consolidation constrains spending |
| 3 | 2014–2021 | Post-Crimea rearmament | NATO 2% target re-activated |
| 4 | 2022–2023 | Ukraine invasion surge | Universal simultaneous shock |

Regime boundaries validated by Chow tests (breaks at 2003, 2014, 2022), Bai-Perron supF test, and AIC/BIC comparison. The four-regime specification is preferred over no-regime baseline (ΔAIC = 6.0, LR = 18.0, p = 0.006).
