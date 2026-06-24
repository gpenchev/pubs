# Threat Measure Construction

## Data Source

UCDP Georeferenced Event Dataset (GED) version 26.1, filtered to **state-based conflict only** (type_of_violence = 1). Non-state conflict and one-sided violence are excluded because they do not represent the interstate or civil war threat environment that drives NATO defence spending decisions. The geographic filter covers the European theatre bounding box (latitude 30N–72N, longitude 25W–45E).

## Land-Contiguity Filter

An event is classified as land-contiguous if the straight-line path from the event location to the nearest point on the EU external land border crosses **no more than 50 km of open sea**. This threshold:

- **Accommodates** narrow straits: Danish straits (~8 km), Bosphorus (~1 km)
- **Excludes** clearly sea-separated theatres: North Africa across the Mediterranean (minimum ~150 km sea crossing), Cyprus from the mainland

Sea crossing is measured by intersecting the straight-line path with an ocean mask derived from Natural Earth land polygons. Events that fail the land-contiguity filter are retained in the robustness variant (`threat_score_log`) but excluded from the primary measure (`threat_land_log`).

The primary variable therefore captures **land-accessible military threat** — the subset of conflict events that could credibly expand to threaten EU territory via overland routes.

## Threat Formula

The threat score for country *c* in year *t* is:

$$\text{threat}(c, t) = \sum_{e} \left[ \log(\text{fatalities}_e + 1) \cdot \exp\left(-\frac{d(c,e)}{500}\right) \right]$$

Where:

- *e* indexes state-based conflict events in year *t* that pass the land-contiguity filter
- fatalities*ₑ* is the best estimate of battle deaths from UCDP GED
- *d(c, e)* is the distance in km from the **nearest point on country c's border polygon** to event *e* (not centroid distance)
- The **500 km spatial decay bandwidth** means: an event at 500 km contributes exp(−1) ≈ 37% of its log-fatality weight; at 1000 km ≈ 14%; at 2000 km ≈ 2%
- log(fatalities + 1) compresses the fatality distribution and handles zero-fatality events

## Distance Computation

All distances are computed after projecting to **ETRS89-LAEA (EPSG:3035)**, a metric equal-area projection for Europe. Border-polygon distances are used rather than centroid distances — this is more accurate for large or non-convex countries such as France (which has overseas territories) and Norway (elongated coastline).

## Normalisation

The primary variable `threat_land_log = log(threat_land + 1)`. A robustness variant `threat_score_log` uses the same formula without the land-contiguity filter. Both variants are standardised to z-scores for sensitivity analysis (Check D: correlation between `threat_land_log` and `threat_score_log` = 0.506).

## Geographic Interpretation

The threat measure is intentionally **country-specific and time-varying**. Unlike standard threat proxies in the burden-sharing literature (alliance-level aggregates, binary conflict indicators), this measure captures:

1. **Proximity gradient**: a conflict at 200 km contributes more than one at 800 km
2. **Intensity weighting**: a high-fatality event contributes more than a low-fatality one
3. **Land-accessibility**: sea-separated conflicts are excluded from the primary measure
4. **Annual variation**: the score varies by year as conflict events occur, intensify, or cease

Countries with identical geographic positions can differ substantially in their threat scores if the direction of conflict differs (e.g., Estonia vs. Spain in any given year). Countries close to the Balkans theatre had high threat scores in 1995–2004 and near-zero scores in 2006–2013.
