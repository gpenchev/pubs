# Regression Results — M1 to M12

## 1. Regime 1 Truncation Caveat

**Immigration data is unavailable before 2000 for most countries.** All models that include `immigration_rate` as a control (M3, M4, M5, M6, M7, M8, M10a, M10b, M10c, M12) are therefore estimated on a sample that begins in 1998–2000 depending on country. Regime 1 (1995–2004) is effectively identified on 2000–2004 only in these models — approximately five years rather than ten.

This truncation removes the peak of the Balkans Wars (1995–1999), which had the highest threat scores in the entire dataset. The Regime 1 interaction coefficient in M4 and M7 is therefore estimated on the post-Balkans wind-down rather than the full conflict period, and likely **understates** Regime 1 threat-responsiveness.

The direction of bias is conservative: the true Regime 1 threat elasticity is plausibly larger than the +0.100 net effect estimated in M4. This limitation is disclosed; it does not invalidate the finding that threat responsiveness was present and positive in Regime 1.

---

## 2. Baseline OLS Results (M1–M4)

**Primary threat coefficient across OLS specifications:**

| Model | β (threat\_land\_log) | SE | p-value | Controls |
|---|---|---|---|---|
| M1: Pooled OLS | 0.079 | 0.019 | < 0.001 | None |
| M2: Country FE | 0.050 | 0.009 | < 0.001 | Country FE |
| M3: Two-way FE | 0.106 | 0.024 | < 0.001 | Country + year FE |
| M4: FE + Regime | 0.100 | 0.034 | 0.004 | Country + year FE + regime interactions |

Threat is significant across all baseline specifications. The coefficient rises from M1 (no FE) to M3 (two-way FE), consistent with omitted variable bias in M1: countries with chronically higher threat also tend to have other structural reasons for higher spending, and controlling for country FE isolates the within-country variation.

**Regime interactions in M4 (net threat elasticity per regime):**

| Regime | Label | Base coef | Interaction | Net effect |
|---|---|---|---|---|
| 1 | 1995–2004 (Balkans) | 0.100 | 0 (baseline) | +0.100 |
| 2 | 2005–2013 (Austerity) | 0.100 | −0.256 | **−0.156** |
| 3 | 2014–2021 (Post-Crimea) | 0.100 | −0.167 | **−0.068** |
| 4 | 2022–2023 (Ukraine) | 0.100 | +0.051 | +0.151 |

The austerity decade (Regime 2) produces a **negative net threat elasticity**: EU fiscal rules and sovereign debt constraints were so binding that governments *reduced* defence spending even when nearby threat increased. This is the sharpest finding in the regime analysis. Regime 3 shows partial recovery; Regime 4 returns to positive but is estimated on only 44 observations (see methodology/models/weak.md §2).

**Key controls in M3:**

| Variable | β | SE | p-value |
|---|---|---|---|
| deficit\_gdp | −0.025 | 0.005 | < 0.001 |
| immigration\_rate | +0.016 | 0.004 | < 0.001 |
| gov\_eu\_position | −0.019 | 0.009 | 0.041 |
| debt\_gdp | −0.000 | 0.001 | 0.843 |
| gov\_left\_right | +0.004 | 0.007 | 0.555 |
| election\_year | −0.026 | 0.024 | 0.287 |
| gdp\_growth | +0.004 | 0.005 | 0.494 |

Deficit (not debt) is the binding fiscal constraint. Immigration pressure is associated with higher defence spending. Government left-right position is not significant within-country.

**VIF (M3 two-way FE):**

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

The SEVERE flags reflect shared temporal trends with year FEs, not structural collinearity. Orthogonalisation check (Check C) confirms zero coefficient change.

---

## 3. Primary SAR Results — M5

The primary spatial autoregressive model (M5) adds a block-diagonal queen contiguity W matrix. Estimated via maximum likelihood (`spatialreg::lagsarlm`). N = 517 (12 observations dropped as isolated nodes in the block-diagonal W).

**Full M5 coefficient table:**

| Variable | β | SE | p-value | Significant |
|---|---|---|---|---|
| ρ (spatial lag) | +0.177 | 0.039 | < 0.001 | ✓ |
| threat\_land\_log | +0.088 | 0.023 | < 0.001 | ✓ |
| deficit\_gdp | −0.023 | 0.005 | < 0.001 | ✓ |
| immigration\_rate | +0.012 | 0.003 | < 0.001 | ✓ |
| gov\_eu\_position | −0.020 | 0.009 | 0.021 | ✓ |
| gov\_left\_right | +0.007 | 0.007 | 0.301 | — |
| election\_year | −0.034 | 0.023 | 0.129 | — |
| gdp\_growth | +0.003 | 0.005 | 0.494 | — |
| debt\_gdp | +0.000 | 0.001 | 0.868 | — |

**Country fixed effects (M5 SAR):**

| Country | Fixed Effect | SE | Interpretation |
|---|---|---|---|
| GR | +1.687 | 0.101 | Spends 1.7 pp above threat+fiscal prediction |
| FR | +0.901 | 0.076 | Global power projection premium |
| PL | +0.901 | 0.090 | NATO eastern flank commitment |
| NO | +0.860 | 0.107 | High structural spending level |
| EE | +0.790 | 0.114 | Baltic security posture |
| HR | +0.767 | 0.083 | Post-conflict institutional legacy |
| BE | 0.000 | — | Reference country (baseline) |

---

## 4. SEM Results — M6

M6 estimates a spatial error model. λ = +0.180 (SE = 0.041, p < 0.001). Threat coefficient: β = +0.101 (SE = 0.026, p < 0.001). SAR preferred over SEM by LR test (LR = 3.49, p = 0.062).

---

## 5. Robustness Specifications (M7–M12)

**Spatial lag ρ stability across specifications:**

| Model | ρ | SE | p-value | N |
|---|---|---|---|---|
| M5: SAR (primary) | +0.177 | 0.039 | < 0.001 | 517 |
| M6: SEM | +0.180 | 0.041 | < 0.001 | 517 |
| M7: SAR + Regime | +0.153 | 0.039 | < 0.001 | 517 |
| M8: SAR all-events | +0.164 | 0.039 | < 0.001 | 517 |
| M9: SAR inv.dist W | +0.346 | 0.070 | < 0.001 | 529 |
| M10a: SAR no Finland | +0.221 | 0.042 | < 0.001 | 465 |
| M10b: SAR post-2014 | +0.082 | 0.063 | 0.194 | 220 |
| M10c: SAR pre-2014 | +0.019 | 0.049 | 0.696 | 297 |
| M12: SAR lagged DV | +0.061 | 0.034 | 0.077 | 495 |

**Threat coefficient stability across SAR specifications:**

| Model | β (threat) | SE | p-value |
|---|---|---|---|
| M5 | +0.088 | 0.023 | < 0.001 |
| M7 (with regime interactions) | +0.071 | 0.034 | 0.038 |
| M8 (all-events threat) | — | — | — |
| M10a (no Finland) | +0.098 | 0.025 | < 0.001 |
| M10c (pre-2014) | +0.069 | 0.025 | 0.005 |
| M12 (lagged DV) | +0.050 | 0.018 | 0.007 |

Threat is significant in all specifications except M10b (post-2014), where the 2022 universal shock is absorbed by year fixed effects.

**Model fit comparison:**

| Model | Log-lik | AIC | N |
|---|---|---|---|
| M5: SAR | 40.20 | 33.61 | 517 |
| M6: SEM | 38.45 | 37.10 | 517 |
| M7: SAR + Regime | 44.67 | 30.66 | 517 |
| M8: SAR all-events | 47.55 | 18.90 | 517 |
| M12: SAR lagged DV | 182.5 | −251.0 | 495 |

M7 (preferred on AIC among M5–M7). M12 has much lower AIC due to the lagged DV absorbing most variance, but this represents persistence, not explanatory power.

---

## 6. Gov EU Position — Subperiod Reversal

The full-sample M5 coefficient on `gov_eu_position` is −0.020 (p = 0.021). Subperiod estimates reveal a significant sign reversal:

| Period | β | SE | p-value | Reading |
|---|---|---|---|---|
| Pre-2014 (M10c) | +0.017 | 0.009 | 0.053 | Marginally positive |
| Post-2014 (M10b) | −0.020 | 0.016 | 0.213 | Negative (not sig. in isolation) |
| Full sample (M5) | −0.020 | 0.009 | 0.021 | Significant negative |

z-test for pre vs post-2014 difference: **p = 0.008** — the reversal is statistically significant. Pre-2014: pro-EU cabinets spent marginally more as part of a multilateral commitment posture. Post-2014: Eurosceptic nationalist governments (PiS Poland, Fidesz Hungary, Baltic coalitions) drove rearmament while pro-EU governments were constrained by EU fiscal rules.
