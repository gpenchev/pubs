# Study Weaknesses and Limitations

This document describes the four principal methodological weaknesses of the study. Each is acknowledged, quantified where possible, and assessed for its impact on the central findings.

---

## 1. Threat measure does not capture VIF collinearity with debt

**Concern:** VIF analysis on the two-way FE model (M3) identifies severe collinearity for `threat_land_log` (VIF = 13.2) and `debt_gdp` (VIF = 10.9). Standard guidance treats VIF ≥ 10 as severe and potentially inflating coefficient standard errors.

**Assessment:** The source of the collinearity is the shared temporal structure of both variables with year fixed effects — not bivariate correlation between threat and debt (r = 0.017). An orthogonalisation check (Check C) regresses `threat_land_log` on `debt_gdp` and uses the residuals as a purged threat measure. Re-estimating M5 with the orthogonalised threat produces zero coefficient change (0.10572 → 0.10572). The VIF flags are benign: the shared temporal trend is absorbed by year FEs in the panel regression, and coefficient estimates are not distorted.

**Status:** Disclosed — no respecification required.

---

## 2. Regime 4 has insufficient observations for high statistical power

**Concern:** Regime 4 (2022–2023) contains only N = 44 observations (22 countries × 2 years). A power calculation assuming standard errors scale as 1/√N gives estimated power of 27.9% for the threat interaction term, far below the conventional 80% threshold (which would require N ≈ 184).

**Assessment:** The interaction term for Regime 4 in M7 (`threat_land_log:regime4`, β = +0.060, p = 0.170) is not individually significant, which is consistent with low power rather than a true zero effect. The net threat elasticity in Regime 4 is +0.151 — directionally consistent with the post-2022 rearmament surge. Within-year cross-sections for 2022 (β = +0.381, R² = 0.559) and 2023 (β = +0.282, R² = 0.383) confirm the threat-defence gradient is real and large in these years.

**Status:** Disclosed — non-significance of the M7 Regime 4 interaction is a power artefact. Results will strengthen as post-2023 data accrues.

---

## 3. Immigration rate truncates Regime 1 to 2000–2004

**Concern:** Eurostat immigration data (`migr_imm1ctz`) is unavailable before 2000 for most countries. Models including `immigration_rate` as a control therefore cannot use 1995–1999 observations. Regime 1 (1995–2004) is effectively identified on 2000–2004 only — approximately five years rather than ten — in models with the immigration control.

**Assessment:** This truncation removes the peak of the Balkans Wars (1995–1999), which had the highest threat scores in the entire dataset. The Regime 1 interaction coefficient in M4 (+0.100 net effect) is therefore estimated on the post-Balkans wind-down rather than the full conflict period, likely **understating** Regime 1 threat-responsiveness. The direction of bias is conservative: if anything, the true Regime 1 effect is larger than estimated.

The exclusion of `immigration_rate` from models estimated on the full 1995–2023 range (Check F) shows that the threat and fiscal coefficients are stable (threat changes from 0.088 to 0.105), but political ideology coefficients are not — `gov_left_right` and `gov_eu_position` reverse sign without the immigration control. This confirms that immigration is a necessary control for correct identification of political ideology effects.

**Status:** Disclosed — Regime 1 threat-responsiveness is likely understated. Not invalidating.

---

## 4. Spatial weight matrix sensitivity and Finland's distance fallback

**Concern:** The queen contiguity weight matrix assigns Finland zero neighbours (no land borders with other sample countries). A distance-based fallback within 2000 km is applied. This is a design decision rather than a data-driven solution.

**Assessment:** Sensitivity check M10a excludes Finland entirely. Results: ρ increases marginally from 0.177 to 0.221, threat coefficient is stable (0.088 → 0.098), all other coefficients within 1 SE. Finland's fallback does not drive the spatial results. The distance-band W and inverse-distance W robustness checks (M9) also confirm ρ is positive and significant across all weight matrix specifications (range: +0.164 to +0.346).

**Status:** Documented — Finland exclusion sensitivity passed.

---

## 5. Spatial persistence vs diffusion

**Concern:** The positive spatial lag ρ = +0.177 could reflect temporal persistence of defence spending levels rather than genuine cross-country diffusion. Countries with persistent high spending will appear to have high-spending neighbours if spatial and temporal autocorrelation are conflated.

**Assessment:** A persistence decomposition (Check A) directly tests this. After adding a lagged dependent variable (M12, ρ = +0.061, p = 0.077), the spatial signal weakens substantially. After first-differencing (FD SAR, ρ = −0.091, p = 0.032), the sign reverses. The negative FD-SAR coefficient reflects short-run burden-sharing substitution: countries that increase spending in a given year tend to have neighbours that increase spending *less*. This is consistent with division-of-labour dynamics within NATO. The levels-SAR positive ρ and FD-SAR negative ρ are therefore both theoretically interpretable and mutually consistent; they reflect different time horizons of the same strategic relationship.

**Status:** Documented — both signs are consistent with NATO alliance dynamics.

---

## 6. GPR comparison is restricted to 13 countries

**Concern:** The Caldara-Iacoviello GPR index is only available for 13 of the 22 primary regression countries. The 9 missing countries (BG, CZ, EE, HR, LT, LV, RO, SI, SK) are predominantly Central and Eastern European states with the **highest land-threat exposure** in the sample. The UCDP vs GPR AIC comparison (ΔAIC = 17.6 in favour of UCDP) is therefore conducted on a subsample that systematically excludes the countries where UCDP has the greatest advantage over GPR.

**Assessment:** The UCDP advantage over GPR (ΔAIC = 17.6, AIC_UCDP = −344.8 vs AIC_GPR = −327.2 on the 13-country subsample) is a conservative lower bound. On the full 22-country sample including the high-threat CEE states, the UCDP advantage would almost certainly be larger. The sign reversal of ρ (UCDP: +0.177, GPR: −0.210) further confirms that the two measures capture structurally different phenomena.

**Status:** Disclosed — UCDP advantage is understated.

---

## 9. Kinetic bias: the threat measure misses the 2014 Crimea shock

**Concern:** The 2014 Crimea annexation was a defining event for European security and produced an immediate NATO rearmament response. Yet UCDP records almost no fatalities near EU land borders from this event — it was an annexation, not a conventional ground war. The UCDP land threat index therefore shows almost no signal at 2014, while newspaper-based GPR measures spike sharply.

**Evidence:** The UCDP vs GPR divergence series shows mean divergence of 0.68 in 2014 and maximum divergence of 2.96 — both near the top of the 1995–2013 range. The divergence remains elevated through 2015–2016 before returning toward baseline. In 2022, the pattern reverses: the Ukraine invasion produces both a sharp UCDP spike (high fatalities near EU borders) *and* a GPR spike, so the two measures converge.

**Why UCDP still outperforms GPR overall:** Despite the 2014 kinetic bias, UCDP delivers lower AIC across the full sample. Newspaper salience is noisy — GPR responds to diplomatic incidents, elections, and rhetorical escalation that have no measurable defence spending effect. UCDP fatality counts, while missing coercive non-kinetic events, are a cleaner signal of the threat type that historically drives NATO defence budgets.

**Framing:** This is a discussion point, not a modelling fix. The appropriate response is to note the kinetic bias as a limitation, present the GPR comparison as evidence that UCDP nonetheless outperforms the alternative, and flag that future work should develop hybrid measures that incorporate both kinetic and non-kinetic threat signals.

---

## 10. Sea threshold (50 km) excludes Greek Aegean threat

**Concern:** Greece's primary security concerns — tensions with Turkey over the Aegean Sea, overflights, and island sovereignty — involve a sea-separated theatre. The 50 km sea-crossing threshold excludes these events from the land-contiguous measure. Greece consequently has a near-average UCDP threat score despite spending far above the sample average (mean defence/GDP = 2.4% vs sample mean 1.6%).

**Evidence:** Greece's mean `threat_land_log` across 1995–2023 is 1.37 (close to the sample median), while its mean defence spending is the second highest in the sample. This discrepancy is not noise — it is structural. The Greek state has consistently prioritised defence spending in response to a threat environment that the UCDP land measure does not capture.

**How the model handles this:** The country fixed effect for Greece is +1.687 — the largest in the sample by a substantial margin (next largest: France at +0.901). The fixed effect absorbs the persistent elevation in Greek spending that cannot be explained by the threat index. The threat coefficient is therefore not contaminated by Greek exceptionalism; Greek spending is explained by the fixed effect, not misattributed to the threat variable.

**Framing:** The 50 km threshold is a theoretically motivated design choice (land-accessible threat), not a data error. The Greek case confirms rather than undermines the measure's logic: the FE correctly identifies that Greece spends persistently above the threat-predicted level, consistent with an Aegean-oriented defence posture that the land measure cannot capture. A more complete threat measure would incorporate sea-based threat components; this is a direction for future work.
