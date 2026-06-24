# Research Framework

## Overview

This study examines the determinants of defence spending among NATO-EU member states over the period 1995–2023. The central question is whether European governments respond to proximate military threat in a rational, proportional way — and what fiscal and political conditions constrain that response.

The study constructs a novel **threat proximity index** from georeferenced conflict event data (UCDP GED 26.1): a spatially-decayed, fatality-weighted score measuring the intensity of state-based conflict events within land-accessible distance of each country's border. This measure is estimated in spatial autoregressive (SAR) panel models with country and year fixed effects, controlling for fiscal constraints (debt, deficit), economic conditions (GDP growth), immigration pressure, and government political orientation.

## Central Findings

**Threat responsiveness is real and statistically robust.** The primary SAR model (M5) estimates a threat coefficient of β = +0.088 (SE = 0.023, p < 0.001): a one-unit increase in log threat proximity is associated with a 0.088 percentage-point increase in defence spending as a share of GDP, conditional on fiscal and political controls.

**Spatial complementarity is present at the alliance level.** The spatial lag parameter ρ = +0.177 (SE = 0.039, p < 0.001) confirms that countries with high-spending neighbours spend more themselves — consistent with long-run strategic burden-sharing equilibria within NATO. However, a persistence decomposition reveals this reflects long-run spending levels rather than short-run imitation: after controlling for temporal persistence (M12, ρ = +0.061) or first-differencing (FD SAR, ρ = −0.091), the spatial signal weakens substantially.

**Fiscal austerity suppressed the threat response during 2005–2013.** Regime-specific estimates (M7) show that the threat elasticity was strongly negative in Regime 2 (net effect −0.156), consistent with EU fiscal consolidation constraining defence budgets even as threat levels remained above the post-Cold War baseline.

**The political cleavage that matters after 2014 is sovereignty vs. integration, not left vs. right.** The government EU position coefficient reverses sign across the 2014 structural break (pre-2014: +0.024; post-2014: −0.052; z-test p = 0.008). Eurosceptic nationalist governments became the primary drivers of post-Crimea rearmament.

## Data and Sample

- **Countries:** 22 NATO-EU member states in primary models (24 in panel download)
- **Period:** 1995–2023 (29 years; regression sample 529 observations)
- **Exclusions:** Luxembourg (structural outlier, defence/GDP < 0.2% throughout); Great Britain (island geography systematically underestimates land-contiguous threat)
- **Sources:** UCDP GED 26.1 · World Bank WDI · IMF WEO · Eurostat · ParlGov · Caldara-Iacoviello GPR
