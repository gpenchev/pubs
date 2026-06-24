# Threat Index — Empirical Results

## 1. Country-Level Summary Statistics

The threat proximity index (`threat_land_log`) is computed for all 24 panel countries over 1995–2023 (29 years each). All countries have exactly 5 zero-threat years (17.2% of the period), corresponding to years with no UCDP state-based conflict events within land-accessible distance.

**Top 10 countries by mean land-threat score (1995–2023):**

| Country | Mean threat\_land\_log | SD | Max | Mean score (all events) | UCDP–GPR divergence |
|---|---|---|---|---|---|
| Romania (RO) | 1.406 | 2.236 | 6.946 | 5.203 | 3.797 |
| Bulgaria (BG) | 1.379 | 2.146 | 6.377 | 5.296 | 3.917 |
| Greece (GR) | 1.365 | 2.012 | 5.852 | 5.670 | 4.305 |
| Hungary (HU) | 1.253 | 2.130 | 6.518 | 4.375 | 3.122 |
| Croatia (HR) | 1.245 | 2.092 | 6.770 | 4.474 | 3.229 |
| Poland (PL) | 1.199 | 2.023 | 6.594 | 4.109 | 2.911 |
| Slovakia (SK) | 1.177 | 2.052 | 6.364 | 4.165 | 2.988 |
| Italy (IT) | 1.147 | 1.913 | 6.354 | 5.039 | 3.892 |
| Slovenia (SI) | 1.083 | 1.944 | 6.497 | 4.040 | 2.957 |
| Czech Republic (CZ) | 1.073 | 1.923 | 5.932 | 3.841 | 2.768 |

**Bottom 5 countries (lowest mean threat):**

| Country | Mean threat\_land\_log | Mean defence/GDP |
|---|---|---|
| Portugal (PT) | 0.341 | ~1.3% |
| Great Britain (GB) | 0.595 | ~2.4% |
| Norway (NO) | 0.639 | ~1.7% |
| Spain (ES) | 0.636 | ~1.2% |
| Netherlands (NL) | 0.718 | ~1.4% |

**Key observation — Great Britain:** GB's mean threat score (0.595) is 39% below the sample mean, yet its mean defence spending (≈2.35%) is 47% above the sample mean. This is the opposite of the theory-predicted direction and is the primary justification for GB's exclusion from primary regression models (see methodology/models/weak.md §10 and the Issues tab).

## 2. Temporal Structure

The threat index is highly right-skewed: the median country-year observation has `threat_land_log` near zero, while the upper tail reflects intense conflict periods (Balkans 1995–2004, Ukraine 2022). The time series is strongly regime-dependent:

- **Regime 1 (1995–2004):** Balkans Wars peak. Eastern/South-Eastern European countries (RO, BG, GR, HR) have their maximum threat scores during this period.
- **Regime 2 (2005–2013):** Near-zero threat for most countries. The austerity decade coincides with an almost complete absence of nearby conflict.
- **Regime 3 (2014–2021):** Modest uptick for Baltic and Eastern European countries following Ukraine/Donbas conflict. Threat levels far below the Balkans peak.
- **Regime 4 (2022–2023):** Sharp spike concentrated in Eastern European countries; Ukraine invasion events near the EU's eastern border.

## 3. Land vs All-Events Comparison

The divergence between `threat_land_log` (land-contiguous only) and `threat_score_log` (all state-based events) captures events excluded by the 50 km sea threshold. Countries with large divergence values have significant nearby sea-separated conflict theatres:

| Country | Mean divergence | Interpretation |
|---|---|---|
| Greece (GR) | 4.305 | Highest divergence — Aegean/Eastern Mediterranean excluded |
| Italy (IT) | 3.892 | Mediterranean/Libya excluded |
| Bulgaria (BG) | 3.917 | Black Sea theatre excluded |
| Romania (RO) | 3.797 | Black Sea theatre excluded |
| Spain (ES) | 3.695 | North Africa/Strait of Gibraltar excluded |

Correlation between `threat_land_log` and `threat_score_log` across all country-years: **r = 0.506** (Check D). The two measures share just over 25% of their variance — confirming they capture meaningfully different threat environments.

## 4. GPR Correlation by Country

The Caldara-Iacoviello GPR index is available for 13 of the 22 primary regression countries. Correlation between annual `threat_land_log` and country-specific GPR mean:

| Country | Pearson r | p-value | Interpretation |
|---|---|---|---|
| Finland (FI) | 0.729 | < 0.001 | Strong |
| Hungary (HU) | 0.676 | < 0.001 | Moderate–Strong |
| Norway (NO) | 0.667 | < 0.001 | Moderate–Strong |
| Poland (PL) | 0.587 | < 0.001 | Moderate |
| Denmark (DK) | 0.528 | 0.003 | Moderate |
| Netherlands (NL) | 0.490 | 0.007 | Moderate |
| Italy (IT) | 0.375 | 0.045 | Weak |
| Belgium (BE) | 0.371 | 0.047 | Weak |
| Germany (DE) | 0.428 | 0.021 | Weak |
| Spain (ES) | 0.041 | 0.832 | Negligible |
| France (FR) | 0.201 | 0.297 | Negligible |
| Great Britain (GB) | 0.105 | 0.588 | Negligible |
| Portugal (PT) | 0.097 | 0.618 | Negligible |

**Pooled correlation across all 13-country observations: r = 0.082 (p = 0.110) — not significant.** The two measures are largely orthogonal at the pooled level, reflecting the fact that GPR responds to newspaper salience (diplomatic incidents, elections) while UCDP captures kinetic conflict fatalities. Countries where the two align (Finland, Hungary, Norway, Poland) are those most closely connected to the Eastern European security environment.

## 5. Kinetic Bias — 2014 Crimea Divergence

The UCDP–GPR divergence time series reveals the clearest evidence of kinetic bias. Mean divergence across the 13-country GPR subsample:

- **1995 peak:** mean = 2.68, max = 4.18 (Balkans wars — both measures elevated)
- **2000–2013 baseline:** mean = 0.30–1.07 (stable low divergence)
- **2014:** mean = 0.68, max = 2.96 (GPR spikes at Crimea; UCDP barely moves)
- **2022:** mean = 2.02, max = 3.56 (Ukraine invasion — both measures spike, divergence from GPR saturation)
- **2023:** mean = 2.34, max = 3.86 (continued elevated divergence)

The 2014 spike confirms the kinetic bias: the Crimea annexation was a coercive hybrid operation that produced minimal UCDP-recorded fatalities near EU land borders but saturated newspaper coverage. The 2022 divergence reflects a different dynamic: the Ukraine invasion produced high UCDP fatalities (true kinetic conflict) *and* saturated GPR, creating a different type of signal compression.
