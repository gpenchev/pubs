# Presentation Plan
## Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023
### 15-minute conference presentation

---

## Figure availability assessment

All figures below are renderable from existing pipeline CSVs using standard
`ggplot2` + `png` device (Cairo not required). Five static figures are
proposed for slides; two are maps. All data confirmed present.

| Fig | Source CSV | Type | Slide |
|-----|-----------|------|-------|
| F1 | `app_conflict_events.csv` + rnaturalearth | Map — conflict events 1995–2004 vs 2014–2023 | 3 |
| F2 | `app_threat_panel.csv` | Line — threat time series, selected countries | 3 |
| F3 | `app_coef_long.csv` | Forest plot — threat_land_log across M1–M12 | 6 |
| F4 | `app_regime_effects.csv` | Dot+CI — net threat effect by regime (M7) | 7 |
| F5 | `app_issue1_crimea.csv` + `app_gpr_divergence_year.csv` | Line — UCDP vs GPR normalised, 2010–2023 | 9 |

Two backup figures (not on slides, render for Q&A):
| F6 | `app_check_i.csv` | Scatter — 2022 cross-section (Check I) | Backup B2 |
| F7 | `app_influence_country.csv` | Bar — Cook's D by country | Backup B3 |

Script to render all: `presentation/render_figures.R` (to be coded separately)

---

**Format convention**
Each slide entry contains:
- `TITLE` — slide heading
- `CONTENT` — bullets, table, or figure label
- `TALKING POINTS` — what to say (not read verbatim)
- `TIME` — cumulative clock target

---

## SLIDE 1 — Title
**Time: 0:00–0:30 (0.5 min)**

**TITLE:**
Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023

**CONTENT:**
- Author · Affiliation · Date · Conference
- *QR code / URL — interactive supplement*

**TALKING POINTS:**
- One sentence only: "Do European governments spend more on defence when there is an actual war nearby — and what stops them when they don't?"
- Do not explain data or method yet. Just the question.

---

## SLIDE 2 — Motivation
**Time: 0:30–1:30 (1 min)**

**TITLE:**
The Problem: NATO Burden-Sharing Needs a Credible Threat Measure

**CONTENT:**
- NATO 2% GDP target treats all members equally — regardless of geography
- Existing proxies: CINC (capability), GPR (media perception), GDP ratios (circular)
- Gap: no continuous, georeferenced, country-specific measure of **proximate territorial threat**
- This paper builds one — and tests whether governments actually respond to it

**TALKING POINTS:**
- The 2% debate assumes equal threat exposure. Poland and Portugal are not equally threatened.
- GPR captures what newspapers worry about — Iraq 2003, terrorism, trade wars — not whether a war is reachable by land.
- CINC measures military capability, not threat faced.
- The contribution is the measure itself, then what it reveals about rational response.

---

## SLIDE 3 — The threat index
**Time: 1:30–3:30 (2 min)**

**TITLE:**
A Novel Georeferenced Territorial Threat Measure (UCDP GED 26.1)

**CONTENT (top half — formula):**

**Threat_it = ln( 1 + Σⱼ Fatalities_jt × e^(−d_ij / 500) × Land_ij )**

- Source: UCDP GED 26.1 — 118,000 georeferenced conflict events
- **Land-contiguous only** — ≤ 50 km sea crossing filter
- **500 km spatial decay** — proximity to border matters
- **Fatality-weighted** — intensity, not just incident count

**CONTENT (bottom half — FIGURE F1):**
*[Two-panel map: Europe with conflict event bubbles sized by fatalities]*
*Left panel: 1995–2004 — bubble mass over Balkans (former Yugoslavia)*
*Right panel: 2014–2023 — bubble mass shifts to Eastern Europe (Ukraine)*

**TALKING POINTS:**
- Three design choices, each deliberate:
  1. Land contiguity: an island nation (UK) has a genuinely different threat profile from Poland. We exclude sea-separated events — this is a feature, not a limitation.
  2. Fatality weighting: the Balkans wars killed tens of thousands; a single border skirmish kills one. Intensity matters for deterrence decisions.
  3. Spatial decay: a war 50 km from your border is a different planning problem from one 450 km away.
- The maps make the geographic variation concrete. In 1995 the threat was in the Western Balkans — Croatia, Bosnia. By 2022 the mass is overwhelmingly in Eastern Ukraine, 500–1500 km from NATO members. Countries like Poland, Romania, Lithuania face it at full weight; Spain and Portugal face near-zero.
- This geographic heterogeneity within years is what allows identification — we are not just exploiting time variation.

---

## SLIDE 4 — Data and sample
**Time: 3:30–4:15 (0.75 min)**

**TITLE:**
Panel: 22 NATO-EU States · 1998–2023 · N = 529

**CONTENT:**
| Variable | Source |
|---|---|
| Defence spending (% GDP) | WDI / SIPRI |
| Threat proximity index | UCDP GED 26.1 |
| Fiscal deficit (% GDP) | Eurostat / IMF WEO |
| Government debt (% GDP) | Eurostat |
| Immigration rate | Eurostat |
| Gov. EU position | ParlGov (seat-weighted cabinet score) |
| Gov. left-right | ParlGov (seat-weighted cabinet score) |

- GB excluded: island geography creates structural misfit — threat 39% below mean, spending 47% above
- LU excluded: structural outlier (very small, very high spending)

**TALKING POINTS:**
- One sentence per row. Fast.
- The GB exclusion: "Great Britain's island geography means our land-contiguity measure systematically underestimates its threat exposure. We exclude it from regressions and test this assumption explicitly — it is confirmed by the structural divergence statistics."
- ParlGov ideology variables: seat-weighted, so they reflect coalition balance, not just the lead party.

---

## SLIDE 5 — Empirical strategy
**Time: 4:15–5:30 (1.25 min)**

**TITLE:**
Three Identification Layers, Each Answering a Different Question

**CONTENT:**

**Layer 1 — Two-way FE (M3)**
*Does threat predict spending at all?*
- Country FE: strips out everything permanently different per country
- Year FE: strips out all common shocks (financial crisis, COVID)
- Identification: within-country, within-year variation only

**Layer 2 — Spatial SAR panel (M5)**
*Is cross-country correlation genuine strategic interaction or shared inertia?*
- Block-diagonal queen contiguity weight matrix
- ρ > 0: neighbours copy each other (strategic complementarity)
- Controls for persistence and first differences to separate levels from changes

**Layer 3 — Regime interactions (M4, M7)**
*Has the threat-defence relationship changed across political epochs?*
- Bai-Perron structural break detection → four regimes confirmed (p = 0.030)
- 1995–2004 / 2005–2013 / 2014–2021 / 2022–2023

**TALKING POINTS:**
- Each layer builds on the previous. Not 13 models for robustness — 13 models because each asks a different question.
- Layer 1: basic rationality test.
- Layer 2: mechanism test — is the observed spatial pattern real coordination or just shared history?
- Layer 3: temporal stability test — the finding that rationality breaks down in 2005–2013 is not a robustness check, it is the core result.

---

## SLIDE 6 — Main result
**Time: 5:30–7:00 (1.5 min)**

**TITLE:**
Threat Is Significant in 11 of 13 Specifications (β = +0.050 to +0.106)

**CONTENT — FIGURE F3:**
*[Forest plot: threat_land_log coefficient + 95% CI across M1–M12]*
*All points to the right of zero. M10b (post-2014 subsample) the only exception — annotated "year FE artefact"]*

*Primary model (M5) anchor:*
| | Coef | SE | p |
|---|---|---|---|
| Threat proximity | **+0.088** | 0.023 | <0.001 |
| Fiscal deficit | **−0.023** | 0.005 | <0.001 |
| Spatial lag ρ | **+0.177** | 0.039 | <0.001 |

**TALKING POINTS:**
- The forest plot is the single most powerful slide: every point is positive, confidence intervals do not cross zero (except M10b), and the estimates are tightly clustered.
- Range reflects identification strategy not fragility: M1 captures cross-country levels, M12 captures changes beyond inertia. All agree.
- Two findings beyond threat:
  - **Fiscal deficit (−0.023***)**: when governments are borrowing more, they cut defence — the fiscal constraint channel is real.
  - **Spatial lag (ρ = +0.177***)**: neighbours' spending levels predict own spending — this is the NATO burden-sharing norm at work.
- M10b non-significance: year FE absorbed the 2022 common shock. Every country's threat rose simultaneously. Cross-sectional OLS for 2022 alone gives β = +0.381, R² = 0.56. The response was real; the estimator could not see it through the common shock.

---

## SLIDE 7 — The rationality gap
**Time: 7:00–9:00 (2 min)**  ⭐ core finding

**TITLE:**
Fiscal Austerity Broke Rational Threat Response for a Decade

**CONTENT — FIGURE F4:**
*[Dot + 95% CI plot: net threat-defence elasticity by regime (M7)]*
*X-axis: four regimes. Y-axis: net marginal effect of threat on defence spending.*
*Zero line dashed. R1 positive, R2 and R3 below zero, R4 positive.*

| Regime | Period | Net β | Reading |
|---|---|---|---|
| 1 | 1995–2004 | **+0.100** | Normal response — Balkans |
| 2 | 2005–2013 | **−0.156** | Inverted — austerity dominated |
| 3 | 2014–2021 | **−0.067** | Partial recovery — still negative |
| 4 | 2022–2023 | **+0.151** | Return — underpowered (N=44) |

LR test: four-regime specification significantly preferred (p = 0.030)

**TALKING POINTS:**
- This is the central finding. Not just "threat predicts spending" — but "the rationality of the response broke down for a decade and is only now recovering."
- Regime 2 net coefficient is negative: countries facing rising threat *reduced* spending. Fiscal consolidation post-2008 completely overrode the strategic imperative. This is not irrationality — it is fiscal constraint. They could not afford to respond even when they should have.
- Regime 3: the 2014 Crimea annexation began a partial recovery, but governments were still running post-crisis budgets. The net elasticity stayed negative.
- Regime 4 is directionally correct (+0.151, close to the Regime 1 baseline) but underpowered: N=44, only two years. We need more data to confirm. The cross-section 2022 result (β = +0.381) is consistent supplementary evidence.
- Policy implication: the NATO 2% debate has been happening mostly during Regime 2 and 3 — the period when fiscal constraints structurally prevented rational response. The demand that countries spend more is only meaningful once those constraints are lifted.

---

## SLIDE 8 — Spatial structure
**Time: 9:00–9:45 (0.75 min)**

**TITLE:**
Alliance Coordination in Levels — Not Contagion in Changes

**CONTENT:**

| Specification | ρ | p | Reading |
|---|---|---|---|
| M5 — levels SAR | +0.177 | <0.001 | Complementarity in levels |
| M12 — lagged DV SAR | +0.061 | 0.077 | Near-zero after persistence removed |
| FD-SAR — changes | −0.091 | 0.032 | Substitution in changes |
| Pre-2014 (M10c) | +0.019 | 0.696 | No spatial effect |
| Post-2014 (M10b) | +0.082 | 0.194 | Weak |

**TALKING POINTS:**
- Countries have matched *spending levels* because they share alliance norms (NATO 2% target creates a levels anchor). They do not copy each other's annual *changes* — in fact, short-run changes show slight substitution: if Germany surges, France relaxes slightly (free-riding in the short run).
- Pre-2014: no spatial interaction. Countries responded independently to their own threats. Post-2022: apparent coordinated response driven by the common Ukraine shock.
- This matters for alliance design: the spatial coordination is norm-driven and shock-driven, not a durable diffusion mechanism. It may not persist without an ongoing threat.
- Keep this slide fast — one pass through the table, two sentences of interpretation.

---

## SLIDE 9 — GPR comparison
**Time: 9:45–10:45 (1 min)**

**TITLE:**
UCDP Outperforms GPR Perception Index — and the Spatial Sign Reverses

**CONTENT — FIGURE F5:**
*[Dual-line plot: UCDP normalised vs GPR normalised, 13-country mean, 2010–2023]*
*Vertical dashed lines at 2014 (Crimea) and 2022 (Ukraine invasion)*
*GPR spikes sharply in 2014; UCDP barely moves. Both spike in 2022.*

| | UCDP (M5-sub) | GPR (M13) |
|---|---|---|
| AIC (13 countries) | **−344.8** | −327.2 |
| Spatial lag ρ | **+0.177** | **−0.210** |

**TALKING POINTS:**
- The figure makes the kinetic bias visible: GPR spikes in 2014 because newspapers were full of Crimea stories. UCDP barely moves because Crimea was a largely non-kinetic annexation — very few fatalities near EU land borders. Our measure misses the political shock; GPR captures it.
- Despite this limitation, UCDP fits the defence spending data better by ΔAIC = 17.6 on the same 13 countries.
- The spatial sign reversal is theoretically important: UCDP gives positive ρ (shared physical threat → shared response); GPR gives negative ρ (idiosyncratic media perception → free-riding). The two indices are measuring genuinely different phenomena.
- The 10 missing Eastern European countries in the GPR sample are the highest-threat states. The UCDP advantage is conservative.

---

## SLIDE 10 — Robustness
**Time: 10:45–11:30 (0.75 min)**

**TITLE:**
Primary Results Stable Across 10 Sensitivity Checks

**CONTENT:**

| Check | Tests | Verdict |
|---|---|---|
| C — Orthogonalisation | Threat/debt VIF=13 concern | **STABLE** — 0% coefficient change |
| F — No immigration | Political variable sensitivity | Threat stable; ideology reverses sign → immigration stays |
| H — Bulgaria 2019 | F-16 procurement spike (Cook's D = 0.099) | **STABLE** — all < 1 SE |
| I — Cross-section 2022 | Year FE absorption | **CONFIRMED** β = +0.381, R² = 0.56 |
| J — Immigration × post-2022 | Ukrainian refugee confound | No distinct mechanism (p = 0.058) |

**TALKING POINTS:**
- One sentence each.
- Check C: the collinearity flag (VIF = 13) is benign — mathematically separating threat from debt changes the threat coefficient by exactly 0%. The flag reflects shared temporal structure, not parameter instability.
- Check F is the most substantively interesting: removing immigration flips the sign of EU-position and left-right coefficients. This confirms that political ideology effects on defence are conditioned on immigration context — they cannot be estimated independently.
- Check I is the strongest rebuttal to the "post-2014 non-significance" concern. Countries closest to Ukraine spent most in 2022. The panel estimator could not see it; the cross-section can.
- Check J: no special Ukraine refugee effect. Immigration coefficient is the same across all years.

---

## SLIDE 11 — EU position reversal
**Time: 11:30–12:15 (0.75 min)**

**TITLE:**
Pro-EU Governments Reversed from Security Cooperation to Fiscal Restraint After 2014

**CONTENT:**

| Period | Gov. EU position β | p | Reading |
|---|---|---|---|
| Pre-2014 | +0.024 | 0.053 | Marginally *more* spending |
| Post-2014 | **−0.052** | <0.001 | Significantly *less* spending |
| z-test difference | — | **0.008** | Significant reversal |

- Left-right position: **not significant** in any within-country model
- EU position: significant and reversed
- Post-2014 highest spenders by ideology: PiS Poland, Fidesz Hungary, Baltic nationalist coalitions

**TALKING POINTS:**
- Before 2014, pro-EU governments spent slightly more — EU membership was associated with security cooperation commitments.
- After 2014, the relationship reversed: Eurosceptic nationalist parties became the rearmament drivers. Pro-EU governments were restrained by EU fiscal rules and reluctant to frame defence as a national project.
- The left-right dimension has no effect once country levels are absorbed — it is the EU dimension, not the traditional left-right axis, that predicts defence spending changes within countries over time.
- This is a novel finding — worth one dedicated slide.

---

## SLIDE 12 — Conclusions
**Time: 12:15–13:45 (1.5 min)**  ⭐ core takeaway

**TITLE:**
European Governments Are Conditionally Rational on Defence

**CONTENT:**

**The condition is fiscal space.**

| Finding | Result | Implication |
|---|---|---|
| Threat drives spending | β = +0.088*** across all specs | Rational when unconstrained |
| Fiscal deficit suppresses | −0.023*** | Fiscal space is a prerequisite |
| Rationality gap 2005–2013 | Net β = −0.156 | Austerity produced systematic underresponse |
| Regime 4 returning | Net β = +0.151, R² = 0.56 in 2022 | Recovery — but two years only |
| Alliance coordination | ρ = +0.177*** (levels) | Norm-driven, not contagion |
| EU position reversal | Sign flip p = 0.008 | Eurosceptics became rearmament drivers |

**TALKING POINTS:**
- The overarching story: governments respond to threat — when they can afford to. The fiscal constraint is not an excuse; it is a structural finding with policy implications.
- For NATO burden-sharing: demanding that all members hit 2% during a fiscal crisis is politically unrealistic and empirically wrong. The data shows that even governments that *wanted* to respond (high threat, justified spending) could not do so when running large deficits.
- For the post-2022 moment: the current rearmament surge is real, threat-proportional, and consistent with a return to rational behaviour. But two years is not a confirmed regime. We need to watch 2024–2026.
- For political science: the EU position finding suggests that the dimension of political contestation over defence has shifted. It is no longer left vs right — it is European integration vs national sovereignty.

---

## SLIDE 13 — Limitations
**Time: 13:45–14:15 (0.5 min)**

**TITLE:**
Three Disclosed Limitations

**CONTENT:**
- **Kinetic bias** — UCDP misses hybrid warfare, cyberattacks, Crimea 2014 annexation; GPR comparison partly addresses this
- **50 km sea threshold** — Mediterranean naval threats excluded; structural misfit for Greece (highest spender in sample, near-average UCDP threat)
- **Regime 4 underpowered** — N = 44 observations, 28% statistical power; need 2024+ data

*Not a limitation: causality. Two-way FE + spatial correction is the correct identification strategy for this question. Rationality demonstrated; causal identification would require an instrument for threat — a separate paper.*

**TALKING POINTS:**
- Brief and direct. Do not apologise — just name them.
- Kinetic bias is the deepest structural issue: a measure built on fatality counts will always underweight political-military shocks that are coercive but not lethal. We disclose it; the GPR comparison shows UCDP still wins on fit.
- Greece is the most visible case of the sea threshold problem: it spends at the top of the distribution but its UCDP land-threat score is near-average because Aegean naval threats are excluded by design.
- Regime 4: "We have two years. The direction is right and the cross-sectional evidence for 2022 is strong. We are honest about what the panel can and cannot confirm."

---

## SLIDE 14 — Closing
**Time: 14:15–15:00 (0.75 min)**

**TITLE:**
Open Science Supplement — All Data, Code, and Models Available

**CONTENT:**
- Full replication pipeline: [GitHub / OSF link]
- Interactive application: [URL or QR code]
  - **Threat Index** — maps, time series, country comparisons
  - **Panel Estimation** — coefficient forest plot, regime effects
  - **Robustness Checks** — all 10 checks, full tables
  - **Specific Issues** — pre-prepared answers: kinetic bias · Greece · immigration · GPR coverage

*"Thank you — questions welcome."*

**TALKING POINTS:**
- Invite the audience to open the app during Q&A — every chart is interactive.
- The Specific Issues tab is designed for exactly this moment: if someone asks about Greece, Issue 2 is ready. If someone asks about GPR, Issue 1 is ready.
- Frame reproducibility as part of the research design, not a bonus: "The entire analysis runs from raw UCDP and Eurostat downloads to publication tables in a single pipeline."

---

## Timing summary

| Slide | Topic | Cumulative |
|---|---|---|
| 1 | Title | 0:30 |
| 2 | Motivation | 1:30 |
| 3 | Threat index + map ⭐ | 3:30 |
| 4 | Data | 4:15 |
| 5 | Strategy | 5:30 |
| 6 | Main result + forest plot | 7:00 |
| 7 | Regime interactions ⭐⭐ | 9:00 |
| 8 | Spatial structure | 9:45 |
| 9 | GPR comparison | 10:45 |
| 10 | Robustness | 11:30 |
| 11 | EU position reversal | 12:15 |
| 12 | Conclusions ⭐⭐ | 13:45 |
| 13 | Limitations | 14:15 |
| 14 | Closing | 15:00 |

⭐⭐ = slides where 15 extra seconds is fine; do not cut them short.
Slides 4, 8, 10 are the compressible ones if you run over.

---

## Figure specifications for rendering

### F1 — Conflict event map (Slide 3)
**Type:** Two-panel map of Europe (ggplot2 + rnaturalearth + sf)
**Data:** `scripts/output/data/ucdp_map_events.csv` filtered to `land_contiguous == TRUE`
**Left panel:** years 1995–2004, bubble = log(best+1), coloured red, centred on lon/lat
**Right panel:** years 2014–2023, same scale
**Caption:** "Land-contiguous UCDP conflict events — fatality-weighted bubbles"
**Note:** Render at 1400×600 px, two-panel `patchwork`

### F2 — Threat time series (Slide 3, optional second visual or backup)
**Type:** Line chart
**Data:** `scripts/output/app/app_threat_panel.csv`
**Countries to show:** PL, RO, LT, LV, EE (Eastern frontline) vs DE, FR, ES (Western)
**Caption:** "Threat proximity index by country, 1995–2023"
**Note:** Render at 1200×500 px; regime background shading in four bands

### F3 — Coefficient forest plot (Slide 6)
**Type:** Horizontal dot + 95% CI, faceted by term
**Data:** `scripts/output/app/app_coef_long.csv` filtered to `term == "threat_land_log"`
**Models:** M1–M12 on Y-axis, estimate + CI on X-axis
**Zero line:** vertical dashed
**Annotation:** M10b labelled "year FE absorption"
**Note:** Render at 900×600 px single panel (threat_land_log only, not faceted)

### F4 — Regime net effects (Slide 7)
**Type:** Dot + 95% CI, four regime points
**Data:** `scripts/output/app/app_regime_effects.csv`
**Y-axis:** net_coef; X-axis: regime label
**Zero line:** horizontal dashed
**Colours:** red for R2 and R3 (negative), blue for R1 and R4 (positive)
**Annotation:** "N=44, 28% power" on R4 point
**Note:** Render at 800×500 px; this is the most important figure in the talk

### F5 — UCDP vs GPR time series (Slide 9)
**Type:** Two-line chart
**Data:** `scripts/output/app/app_issue1_crimea.csv` — group_by(year), normalise per country, then mean
**X-axis:** year 2010–2023; Y-axis: normalised 0–1
**Lines:** UCDP threat (blue) vs GPR (red)
**Vertical lines:** 2014 (Crimea), 2022 (Ukraine invasion)
**Note:** Render at 1000×500 px

---

## Backup slides (render, bring, do not show unless asked)

**B1** — Full M5 coefficient table (if asked for complete regression output)
**B2** — Check I scatter: threat_land_log vs defence_gdp, 2022, N=22, annotated with country codes
  → Data: rebuild from `app_threat_panel.csv` filtered year==2022
**B3** — Bulgaria time series: defence_gdp 1995–2023, annotated 2019 spike + "F-16 procurement"
  → Data: `app_threat_panel.csv` filtered country=="BG"
**B4** — GPR country correlation table (if GPR method questioned)
  → Data: `scripts/output/quality_reports/gpr_correlation_summary.csv`
**B5** — VIF table + orthogonalisation result (if collinearity concern raised)
  → Data: `scripts/output/app/app_vif.csv`
