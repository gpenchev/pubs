# Variables

## Dependent Variable

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `defence_gdp` | Military expenditure as % of GDP | World Bank WDI (MS.MIL.XPND.GD.ZS), drawing on SIPRI data | 24 countries, 1995–2023 |

**Note on source selection:** Eurostat COFOG GF02 was considered and rejected — it is missing France, Germany, and Great Britain entirely, and Norway values are systematically underreported. WDI/SIPRI provides the most complete and internationally comparable series.

## Independent Variables

### Threat

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `threat_land_log` | Land-contiguous threat score (log) — primary measure | UCDP GED 26.1 | 24 countries, 1995–2023 |
| `threat_score_log` | All state-based conflict threat score (log) — robustness | UCDP GED 26.1 | 24 countries, 1995–2023 |

### Fiscal Controls

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `debt_gdp` | General government gross debt (% GDP) | IMF WEO GGXWDG_NGDP | 24 countries, 1995–2023 |
| `deficit_gdp` | Net lending/borrowing (% GDP; negative = deficit) | IMF WEO GGXCNL_NGDP | 24 countries, 1995–2023 |

### Economic Controls

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `gdp_growth` | Real GDP growth (% change on previous year) | Eurostat CLV_PCH_PRE | 24 countries, 1995–2023 |

**Note:** `gdp_pc` (GDP per capita) was considered but is not included in the regression sample CSV as it is not a direct control in any reported model specification.

### Socio-Political Controls

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `immigration_rate` | Annual immigration per 1,000 population | Eurostat migr_imm1ctz | 23 countries, 2000–2023 (GB excluded; data unavailable before 2000) |
| `gov_left_right` | Seat-weighted cabinet left–right position (0 = left, 10 = right) | ParlGov | 24 countries, 1995–2023 |
| `gov_eu_position` | Seat-weighted cabinet EU integration position (0 = anti-EU, 10 = pro-EU) | ParlGov | 24 countries, 1995–2023 |
| `election_year` | Binary flag: 1 if parliamentary election occurred, 0 otherwise | ParlGov | 24 countries, 1995–2023 |

## Country Sample

**Primary regression sample: 22 countries, 529 observations, 1998–2023.**

| Group | Countries | Reason |
|---|---|---|
| In primary models | BE, BG, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IT, LT, LV, NL, NO, PL, PT, RO, SI, SK | Full data availability |
| In panel only | GB | Island geography; structural threat-defence mismatch |
| In panel only | LU | Defence/GDP < 0.2% throughout; structural outlier |
| Excluded from all | AT, CY, IE, MT | EU members, NATO non-members (permanent neutrality) |
| Excluded from all | SE | NATO member from March 2024 only — outside study period |

## Key Data Notes

**Immigration rate:** Eurostat `migr_imm1ctz` is unavailable before 2000 for most countries and not available for Great Britain at any point. Models including `immigration_rate` are therefore estimated on 1998–2023 (or 2000–2023 depending on country) and exclude GB. The 1995–1999 gap truncates the Balkans Wars peak period — see Issues tab, Issue 3.

**Greece coding:** Eurostat uses the internal code EL for Greece; all download scripts recode EL → GR for consistency with ISO 3166-1 alpha-2 standards used throughout the pipeline.

**Political variables (ParlGov):** Seat-weighted averages are computed annually from monthly cabinet composition data. For countries with multiple governments in a single year, the seat-weighted mean reflects the political centre of gravity across the full year.
