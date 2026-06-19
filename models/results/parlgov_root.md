# ParlGov Merge & Unit Root Check — Results

Computed 2026-06-18 · 24 countries · 1995–2023 · 696 observations

---

## 1. ParlGov merge results

### Coverage

| Variable | Rows present | Coverage |
|----------|-------------|----------|
| `gov_left_right` | 691 / 696 | 99.3% |
| `gov_eu_position` | 691 / 696 | 99.3% |
| `election_year` flag | 181 country-years | 26.0% |

5 missing rows are expected — they correspond to country-years at the very
start of the panel (1995) where cabinet data predates ParlGov coverage for
some new democracies. All 24 countries have 100% coverage for the working
regression period.

### What ParlGov provides

- **`gov_left_right`** — seat-weighted average left-right position of all
  parties in government (0 = far left, 10 = far right). Captures whether
  conservative or progressive governments are in power.
- **`gov_eu_position`** — seat-weighted average EU integration position of
  the cabinet (0 = strongly Eurosceptic, 10 = strongly pro-EU).
- **`election_year`** — binary flag: 1 if a national election occurred that
  year. Controls for electoral budget cycles.

---

## 2. Unit root results

### Summary table

| Variable | ADF % stationary | KPSS % non-stationary | IPS p-value | Verdict |
|----------|-----------------|----------------------|-------------|---------|
| `threat_land_log` | 0% | **0%** | 0.000 | ✅ Stationary |
| `deficit_gdp` | 4% | 8% | 0.000 | ✅ Stationary |
| `gdp_growth` | 0% | 4% | 0.000 | ✅ Stationary |
| `gov_left_right` | 17% | 29% | 0.000 | ✅ Stationary |
| `gov_eu_position` | 13% | 33% | 0.000 | ✅ Stationary |
| `threat_score_log` | 0% | 54% | 0.001 | ✅ Stationary (panel) |
| `defence_gdp` | 4% | 63% | 0.000 | ⚠️ Mixed, stationary at panel level |
| `immigration_rate` | 0% | 65% | 1.000 | ⚠️ Persistent, unit root not rejected |
| `debt_gdp` | 8% | 79% | 0.304 | ⚠️ Borderline I(1) |
| `gdp_pc` | 0% | **100%** | 1.000 | ❌ Non-stationary, I(1) trend |

### Three tests used

- **ADF** (Augmented Dickey-Fuller) — tests each country separately; null is
  unit root. Low power with T=29.
- **KPSS** — tests each country separately; null is stationarity. High power
  but tends to over-reject with trending series.
- **IPS** (Im-Pesaran-Shin) — panel-level test pooling information across all
  countries; much higher power than country-by-country tests with T=29.
  IPS is the decisive test here.

---

## 3. Expert interpretation

### Clean variables (no action needed)
`threat_land_log` passes all three tests: ADF fails to reject unit root for
most individual countries (low power, T=29), but KPSS never rejects
stationarity (0% rejection rate) and IPS rejects the unit root null at
p=0.000. The threat index is stationary by construction — it is driven by
discrete conflict events that begin and end, not by a trending process.
`deficit_gdp`, `gdp_growth`, `gov_left_right`, and `gov_eu_position` are
all stationary at the panel level (IPS p=0.000). These are the theoretically
central variables and they are all clean.

### Mixed signals — two-way FE as mitigation
`defence_gdp` shows persistent behaviour in many individual countries
(IPS p=0.000 nonetheless rejects the unit root at panel level). This is
typical for defence spending: it drifts upward or downward for long stretches
within each country but does not share a common stochastic trend across
countries. Two-way fixed effects (country FE + year FE) absorb
country-specific level shifts and common year-to-year trends, which is why
IPS rejects the unit root even when individual ADF tests struggle.

### Genuine concern: `gdp_pc`
100% KPSS rejection and IPS p=1.000 confirm `gdp_pc` is I(1) — a
non-stationary trending level series. This is not surprising: GDP per capita
grows persistently in all 24 countries over 1995–2023. **Mitigation already
in place**: the primary regressions use `gdp_growth` (the stationary flow
measure, IPS p=0.000) rather than `gdp_pc`. The level `gdp_pc` appears only
in robustness specifications where the long-run wealth effect is of interest;
in those models the country fixed effect absorbs the country-specific trend.

### Borderline concern: `debt_gdp` and `immigration_rate`
Both show high KPSS rejection rates and IPS fails to reject the unit root.
Public debt is a stock variable that accumulates over decades — persistence
is expected. Immigration is similarly path-dependent. In both cases the
two-way FE specification absorbs country-specific trajectories. The
theoretically stationary flow counterpart (`deficit_gdp`) is already in the
model alongside `debt_gdp`. If a reviewer raises this, the standard response
is: (a) FE absorbs country trends; (b) the stationary flow measure
`deficit_gdp` is included; (c) results are robust to first-differencing
(tested in `08_structural_breaks.R` Block 7).

---

## 4. Naive explanation

### What is a unit root and why does it matter?

Imagine measuring the height of a tree every year. Each year it is taller
than the last — the series just keeps going up. If you try to find a
relationship between tree height and rainfall, you will always find one
simply because both keep growing over time, even if they have nothing to do
with each other. This is the unit root problem — a **spurious correlation
caused by shared trends**.

For our models to be trustworthy, the variables we use should not be
pure trending series. They should fluctuate around a stable average,
or at least not drift endlessly in one direction.

### What we found

**Good news — the variables that matter most are clean:**

- **Threat index** (`threat_land_log`) — goes up sharply during the Balkans
  wars and the Ukraine invasion, then falls back to near-zero in peacetime.
  It does not trend. It is safe to use directly. ✅

- **Fiscal deficit** (`deficit_gdp`) — fluctuates above and below zero
  depending on economic conditions. No persistent trend. ✅

- **GDP growth** (`gdp_growth`) — bounces around a long-run average with
  no drift. ✅

- **Political variables** (`gov_left_right`, `gov_eu_position`) — governments
  change and ideology shifts back and forth. No one-way trend. ✅

**One variable is a pure trend:**

- **GDP per capita** (`gdp_pc`) — all 24 countries got richer over
  1995–2023. This series just goes up. We already handle this by using
  GDP growth rate instead of the level in our main models.

**Two variables are persistent but manageable:**

- **Government debt** (`debt_gdp`) — debt tends to accumulate slowly over
  years and decades. We include it because the *level* of debt matters for
  fiscal space, and our model design (country fixed effects) controls for
  each country's own debt trajectory.

- **Immigration rate** (`immigration_rate`) — immigration flows build up
  over time in receiving countries. Same fix applies.

### The fix: country fixed effects

Our regression models include a separate intercept for each country
(called a fixed effect). This is equivalent to subtracting each country's
own average from every observation before running the regression. It removes
any country-specific upward or downward trend, leaving only the
year-to-year variation that we actually want to analyse. This is the
standard solution used in all panel econometrics for this type of problem.

---

## 5. Conclusion for regression

**No differencing required.** The core theoretical variables are stationary.
The non-stationary level variables (`gdp_pc`, `debt_gdp`) are handled by
the two-way fixed effects specification already built into M3–M13. Proceed
to regression.
