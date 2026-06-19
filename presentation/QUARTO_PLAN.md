# Quarto PPTX Presentation — Technical Plan
## Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023

---

## 1. Technology decisions

### Output format: PPTX via Quarto
- `format: pptx` in `_quarto.yml`
- Reference template PPTX (`reference.pptx`) defines master slide layouts,
  fonts, and colour palette — Quarto inherits everything from it
- Figures rendered to PNG at 300 dpi by `render_figures.R` before Quarto runs
- All figures are **static PNG embeds** — no R code chunks in the slides QMD
  (keeps the PPTX portable and renders fast)

### Why static PNGs, not knitr chunks
- PPTX output does not support interactive plots
- Pre-rendering at exact pixel dimensions gives precise control over
  font size, line weight, whitespace, and aspect ratio
- Figures can be reviewed and tweaked independently of the QMD
- Quarto PPTX rendering is fragile with complex knitr chunk output;
  image embeds are reliable

---

## 2. Directory structure

```
presentation/
├── QUARTO_PLAN.md            ← this file
├── plan.md                   ← slide-by-slide content plan
├── render_figures.R          ← renders all PNGs to figures/
├── slides.qmd                ← main Quarto source
├── reference.pptx            ← PowerPoint master template
├── _quarto.yml               ← project config
└── figures/
    ├── f1_conflict_map.png       slide 3
    ├── f2_threat_timeseries.png  slide 3 (second visual, same slide)
    ├── f3_forest_plot.png        slide 6
    ├── f4_regime_effects.png     slide 7
    ├── f5_gpr_comparison.png     slide 9
    ├── f6_check_i_scatter.png    backup B2
    └── f7_bulgaria_series.png    backup B3
```

---

## 3. Colour palette and visual design

### Palette — "dark academic defence"
Used consistently across ALL figures and the reference template:

| Role | Hex | Usage |
|------|-----|-------|
| Background | `#1C2333` | Slide background (dark navy) |
| Surface | `#243044` | Card / plot background |
| Grid lines | `#2E3D55` | Minor grid, axis lines |
| Text primary | `#E8EDF5` | Title, body text |
| Text secondary | `#9AABB8` | Subtitles, axis labels, captions |
| Accent blue | `#4A90D9` | Primary highlight, positive coefficients |
| Accent orange | `#E8863A` | Secondary highlight, R2/R3 negative regimes |
| Accent red | `#D64545` | Danger / negative / significant negative |
| Accent green | `#52B788` | Confirmation / significant positive |
| Accent grey | `#6B7F94` | Neutral / not significant |
| White line | `#FFFFFF` | Zero reference lines, map borders |

### Typography
- **Slide titles:** PT Sans Bold 32 pt (available on system)
- **Body text:** PT Sans Regular 20 pt
- **Figure labels/axes:** PT Sans Regular — loaded via `showtext` in render_figures.R
- **Captions:** PT Sans Narrow Italic 14 pt
- Minimum readable axis text: **13 pt** (projected at 1920×1080)

### Figure dimensions (all PNG, 300 dpi)
| Figure | Width px | Height px | Aspect | Notes |
|--------|----------|-----------|--------|-------|
| F1 conflict map | 2400 | 1100 | 2.18:1 | Two-panel patchwork, full-width slide |
| F2 threat timeseries | 2000 | 900 | 2.22:1 | Full-width slide bottom half |
| F3 forest plot | 1800 | 1100 | 1.64:1 | Horizontal, needs height for 12 rows |
| F4 regime effects | 1600 | 1000 | 1.6:1 | 4 points, generous whitespace |
| F5 GPR comparison | 2000 | 900 | 2.22:1 | Two lines + annotations |
| F6 Check-I scatter | 1400 | 1000 | 1.4:1 | Backup |
| F7 Bulgaria series | 1600 | 800 | 2:1 | Backup |

### ggplot2 base theme (used by ALL figures)
```r
theme_pres <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "PT Sans") +
  ggplot2::theme(
    # Background
    plot.background    = ggplot2::element_rect(fill = "#1C2333", colour = NA),
    panel.background   = ggplot2::element_rect(fill = "#243044", colour = NA),
    panel.grid.major   = ggplot2::element_line(colour = "#2E3D55", linewidth = 0.4),
    panel.grid.minor   = ggplot2::element_blank(),
    # Text
    plot.title         = ggplot2::element_text(colour = "#E8EDF5", face = "bold",
                                               size = rel(1.25), hjust = 0,
                                               margin = ggplot2::margin(b = 6)),
    plot.subtitle      = ggplot2::element_text(colour = "#9AABB8", size = rel(0.95),
                                               hjust = 0,
                                               margin = ggplot2::margin(b = 14)),
    plot.caption       = ggplot2::element_text(colour = "#6B7F94", size = rel(0.7),
                                               hjust = 1),
    axis.title         = ggplot2::element_text(colour = "#9AABB8", size = rel(0.9)),
    axis.text          = ggplot2::element_text(colour = "#9AABB8", size = rel(0.85)),
    axis.line          = ggplot2::element_line(colour = "#2E3D55"),
    # Legend
    legend.background  = ggplot2::element_rect(fill = "#1C2333", colour = NA),
    legend.text        = ggplot2::element_text(colour = "#9AABB8", size = rel(0.85)),
    legend.title       = ggplot2::element_text(colour = "#E8EDF5", size = rel(0.9)),
    legend.key         = ggplot2::element_rect(fill = NA, colour = NA),
    # Facets
    strip.background   = ggplot2::element_rect(fill = "#2E3D55", colour = NA),
    strip.text         = ggplot2::element_text(colour = "#E8EDF5", face = "bold",
                                               size = rel(0.9)),
    # Margin
    plot.margin        = ggplot2::margin(14, 14, 10, 14)
  )
}
```

---

## 4. Figure-by-figure specification

### F1 — Conflict event map (Slide 3)
**Purpose:** Show geographic shift of threat from Balkans (1995–2004) to Eastern Europe (2014–2023)

**Data:**
- `scripts/output/data/ucdp_map_events.csv` — filter `land_contiguous == TRUE`
- `rnaturalearth::ne_countries(scale="medium", returnclass="sf")` — EU+neighbours extent

**Construction:**
- Two `ggplot` maps built separately, combined with `patchwork::plot_layout(ncol=2)`
- Map extent: `xlim=c(-12, 42)`, `ylim=c(34, 72)` — full Europe
- Country polygons: fill `#2E3D55`, border `#4A6080` size 0.3
- Conflict bubbles: `geom_point(aes(x=lon, y=lat, size=log(best+1), alpha=0.6))`,
  colour `#D64545` (left) and `#E8863A` (right)
- Size scale: `scale_size_continuous(range=c(1, 14), guide="none")`
- Annotation boxes (top-left each panel): period label in white
- Vertical divider strip between panels via patchwork

**Key visibility decisions:**
- Dark map background makes red/orange conflict bubbles pop
- No graticule lines — too much noise
- Country labels only for key states: PL, RO, LT, LV, HR, BA, RS
  placed with `ggrepel::geom_label_repel` in dark `#1C2333` fill, white text
- Caption: "Land-contiguous events only (≤50km sea crossing). Bubble size = log(fatalities)"

---

### F2 — Threat time series (Slide 3, second visual on same slide)
**Purpose:** Show within-country temporal variation — Eastern vs Western EU

**Data:** `scripts/output/app/app_threat_panel.csv`

**Construction:**
- 8 countries: PL, RO, LT, LV, EE (Eastern, high-threat, solid lines, blues/greens)
  vs DE, FR, ES (Western, low-threat, dashed lines, greys)
- `geom_line(linewidth=1.2)` + `geom_point(size=2, shape=21)` on last year only
  (avoid overplotting, label endpoint)
- Country labels via `ggrepel::geom_text_repel` at rightmost point
- Background regime bands: `annotate("rect", ...)` in 4 colours at 5% alpha
  labelled R1/R2/R3/R4 at the top
- Vertical lines at 2008 (financial crisis) and 2022 (Ukraine) — white dashed

**Key visibility decisions:**
- Eastern EU countries in warm blues/teals (they are the "story")
- Western EU in muted grey dashed (context only)
- Regime bands give the temporal structure at a glance
- Y-axis: "Threat proximity index (log-scale)" — no raw numbers on the axis
  that audience won't know

---

### F3 — Coefficient forest plot (Slide 6)
**Purpose:** Show threat_land_log coefficient is positive and stable across all specs

**Data:** `scripts/output/app/app_coef_long.csv` filtered to `term == "threat_land_log"`

**Construction:**
- 12 rows (one per model), sorted by estimate descending
- `geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi), height=0.25, linewidth=0.8)`
- `geom_point(aes(colour=sig_colour), size=4)`
- Colour coding:
  - `#52B788` (green) — significant positive (p < 0.05)
  - `#6B7F94` (grey) — not significant (M10b)
- `geom_vline(xintercept=0, colour="#FFFFFF", linetype="dashed", linewidth=0.7)`
- Right annotation strip: model name + p-value label in `#9AABB8`
- M10b row: add annotation "year FE absorbs common shock" in `#E8863A` italic
- M5 row: horizontal highlight band (`annotate("rect")`) in `#4A90D9` at 8% alpha,
  labelled "Primary model"

**Key visibility decisions:**
- All 12 rows visible at once with comfortable row height
- Green/grey colour coding makes the "11 of 12 significant" claim visible instantly
- The M5 highlight band draws the eye to the anchor estimate
- X-axis range: `-0.05` to `+0.16` — no model estimate falls outside this

---

### F4 — Regime net effects (Slide 7) ⭐ most important figure
**Purpose:** Show the rationality gap — threat response inverted in 2005–2013

**Data:** `scripts/output/app/app_regime_effects.csv`

**Construction:**
- 4 large points (size = 8) + thick error bars
  (se_net is NA in the CSV — compute from the M7 coefs: use ±0.034 for R1,
  propagated SE for R2/R3/R4 from app_coef_long regime interaction terms)
- Colour by regime:
  - R1: `#52B788` (green — positive, rational response)
  - R2: `#D64545` (red — negative, austerity broke rationality)
  - R3: `#E8863A` (orange — still negative, partial recovery)
  - R4: `#4A90D9` (blue — positive return, underpowered)
- `geom_hline(yintercept=0, colour="#FFFFFF", linetype="dashed", linewidth=1)`
- `geom_segment` connecting the 4 points (like a dot plot with connecting line)
  in `#6B7F94` linewidth=1
- Shaded bands: above zero = green tint 4%, below zero = red tint 4%
- Annotation on R4: "N=44 · 28% power · direction confirmed" in small italic
- Annotation on R2: "Fiscal austerity dominated" in small italic
- X-axis labels: full regime labels with year ranges and emoji context:
  "R1 1995–2004\nBalkans" / "R2 2005–2013\nAusterity" /
  "R3 2014–2021\nPost-Crimea" / "R4 2022–2023\nUkraine"
- Large point labels: net coefficient value printed above/below each point

**Key visibility decisions:**
- Large points (size=8) are readable projected at 10m distance
- The colour switch from red/orange to green at R4 tells the story without words
- The horizontal zero line is white and thick — the visual anchor
- Y-axis label: "Net threat-defence elasticity (pp defence / log-threat)"
- No legend — colours are self-explained by annotations

---

### F5 — UCDP vs GPR comparison (Slide 9)
**Purpose:** Show kinetic bias (GPR spikes at Crimea 2014; UCDP doesn't)
             and confirm UCDP spikes appropriately at Ukraine 2022

**Data:** `scripts/output/app/app_issue1_crimea.csv`
- Group by year, normalise each country 0–1, take mean across countries

**Construction:**
- Two lines: UCDP (`#4A90D9` blue, linewidth=1.8) and GPR (`#D64545` red, linewidth=1.8)
- `geom_point(size=3, shape=21, fill=colour)` — filled circles on each year
- X-axis: 2010–2023 only (where both series are meaningful)
- `geom_vline` at 2014: `#E8863A` dashed, labelled "Crimea annexation"
- `geom_vline` at 2022: `#FFFFFF` solid, labelled "Ukraine invasion"
- Annotation arrow from 2014 to GPR spike: "GPR +2.1σ · UCDP flat"
  in `#E8863A`
- Annotation at 2022: "Both indices spike — UCDP land-reachable threat highest ever"
  in `#52B788`
- Legend: inside plot, top-left, dark background, two rows

**Key visibility decisions:**
- Limited to 2010–2023: the divergence story is entirely here
- Bold colours on dark background — highly readable at distance
- The two annotations do the interpretation work so you don't need to explain
  while pointing at the slide

---

### F6 — Check I scatter 2022 (Backup B2)
**Purpose:** Show within-year threat gradient in 2022 (N=22 countries)

**Data:** Rebuild from `app_threat_panel.csv` filtered year==2022

**Construction:**
- `geom_point(size=5)` coloured by `#4A90D9`
- `geom_smooth(method="lm", colour="#52B788", se=TRUE, fill="#52B788", alpha=0.15)`
- Country code labels via `ggrepel::geom_text_repel` in white
- Annotate: β = +0.381, p = 0.001, R² = 0.56 — in top-left box

---

### F7 — Bulgaria time series (Backup B3)
**Purpose:** Explain Bulgaria 2019 spike as procurement artefact

**Data:** `app_threat_panel.csv` filtered country=="BG"

**Construction:**
- Line: defence_gdp (blue) and threat_land_log (orange, secondary Y-axis)
- `geom_vline` at 2019 in red
- Annotation: "F-16 procurement contract" at 2019

---

## 5. `_quarto.yml` specification

```yaml
project:
  type: default
  output-dir: _output

format:
  pptx:
    reference-doc: reference.pptx
    slide-level: 2
    slide-number: true
```

**No `_quarto.yml` needed for a single-file project** — all YAML goes in the
`slides.qmd` front matter. Use `slides.qmd` YAML header directly.

---

## 6. `slides.qmd` front matter

```yaml
---
title: "Threat Proximity and Defence Spending in NATO-EU Member States, 1995–2023"
author: "[Author Name]"
date: today
format:
  pptx:
    reference-doc: reference.pptx
    slide-level: 2
    toc: false
execute:
  echo: false
  warning: false
---
```

---

## 7. `reference.pptx` — what to set in the template

The reference PPTX must be created once in PowerPoint/LibreOffice Impress
and saved as `presentation/reference.pptx`.

**Slide master settings:**
| Element | Value |
|---------|-------|
| Slide size | Widescreen 33.87 × 19.05 cm (1920×1080 px equivalent) |
| Background fill | Solid `#1C2333` |
| Title font | PT Sans Bold 36pt, colour `#E8EDF5` |
| Title position | Top bar, 1.5 cm from top, full width |
| Content area | Below title, `#243044` fill rounded rect optional |
| Body text | PT Sans Regular 20pt, colour `#E8EDF5` |
| Bullet level 1 | `#E8EDF5`, 20pt, no indent decoration |
| Bullet level 2 | `#9AABB8`, 17pt, em-dash prefix |
| Footer / slide number | PT Sans 11pt, `#6B7F94`, bottom-right |
| Accent colour 1 | `#4A90D9` |
| Accent colour 2 | `#52B788` |
| Accent colour 3 | `#E8863A` |
| Accent colour 4 | `#D64545` |

**Layouts needed (Quarto PPTX uses named layouts):**
1. `Title Slide` — large centred title, subtitle, author
2. `Title and Content` — title bar + large content area (most slides)
3. `Two Content` — title bar + two equal columns (used for slide 3: map + text)
4. `Blank` — full-bleed image slides (used for figure-only slides)
5. `Section Header` — for the four section dividers (optional)

**Workaround:** If you cannot create the PPTX template, Quarto uses its own
default dark theme by setting `theme: dark` in revealjs — for PPTX you MUST
provide the reference doc for dark slides. Minimum viable reference.pptx:
open PowerPoint, set background to `#1C2333`, title colour to white, save.

---

## 8. `slides.qmd` slide structure

Quarto PPTX uses `##` for slide breaks (`slide-level: 2`).
A single `---` creates a blank slide / section break.
Images: `![]()` with `fig-align: center` and `width: 100%`.

### Key layout patterns:

**Figure-only slide (most impactful):**
```markdown
## Threat Is Significant in 11 of 13 Specifications

![ ](figures/f3_forest_plot.png){fig-align="center" width="95%"}
```
No text on the slide — figure fills the content area.
Speaker notes go in the Notes pane: `::: notes ... :::`

**Half-figure + table slide:**
```markdown
## Regime Interactions (M4 / M7)

:::: {.columns}
::: {.column width="60%"}
![ ](figures/f4_regime_effects.png){width="100%"}
:::
::: {.column width="40%"}
| Regime | Net β |
|---|---|
| R1 1995–2004 | **+0.100** |
| R2 2005–2013 | **−0.156** |
| R3 2014–2021 | **−0.067** |
| R4 2022–2023 | **+0.151** |
:::
::::
```

**Bullet slide:**
Standard `##` heading + bullet list.

---

## 9. Full slide list with layout type

| # | Title | Layout | Figure |
|---|-------|---------|--------|
| 1 | Title | Title Slide | None |
| 2 | The Problem: NATO Burden-Sharing Needs a Threat Measure | Title and Content | None — bullets |
| 3 | A Novel Georeferenced Territorial Threat Measure | Two Content | F1 (map) left, formula + F2 bullets right |
| 4 | Panel: 22 NATO-EU States · 1998–2023 · N=529 | Title and Content | None — table |
| 5 | Three Identification Layers | Title and Content | None — 3 blocks |
| 6 | Threat Significant in 11 of 13 Specifications | Blank | F3 full-bleed |
| 7 | Fiscal Austerity Broke Rational Response for a Decade | Two Content | F4 (left 60%) + table right |
| 8 | Alliance Coordination in Levels — Not Contagion | Title and Content | None — table |
| 9 | UCDP Outperforms GPR (ΔAIC = 17.6) | Blank | F5 full-bleed |
| 10 | Primary Results Stable Across 10 Checks | Title and Content | None — compact table |
| 11 | Pro-EU Governments Reversed After 2014 | Title and Content | None — 2-col table |
| 12 | European Governments Are Conditionally Rational | Title and Content | None — summary table |
| 13 | Three Disclosed Limitations | Title and Content | None — bullets |
| 14 | Open Science Supplement | Title Slide style | QR code image if available |
| B1 | Full M5 coefficients (backup) | Title and Content | None |
| B2 | Check I: 2022 cross-section (backup) | Blank | F6 full-bleed |
| B3 | Bulgaria 2019 (backup) | Blank | F7 full-bleed |

---

## 10. `render_figures.R` — full specification

```
# Structure
1. Setup: library loads, showtext_auto(), font_add("PT Sans", ...), theme_pres definition
2. Output directory: presentation/figures/
3. Helper: save_fig(plot, name, width, height) — wraps ggsave with png + 300dpi
4. ── F1: Conflict map ──────────────────────────────────────────────────────
   - Load ucdp_map_events.csv, filter land_contiguous==TRUE
   - Load EU geometry via rnaturalearth
   - Build p_balkans (1995–2004) and p_east (2014–2023)
   - Combine: patchwork::wrap_plots(p_balkans, p_east) + plot_annotation
   - save_fig(f1, "f1_conflict_map", 2400, 1100)
5. ── F2: Threat time series ─────────────────────────────────────────────────
   - Load app_threat_panel.csv
   - Countries: eastern = c("PL","RO","LT","LV","EE"), western = c("DE","FR","ES")
   - Regime bands as annotate("rect", ...)
   - save_fig(f2, "f2_threat_timeseries", 2000, 900)
6. ── F3: Forest plot ─────────────────────────────────────────────────────────
   - Load app_coef_long.csv, filter term=="threat_land_log", drop duplicate M4 row
   - Compute ci_lo = estimate - 1.96*std_error, ci_hi = estimate + 1.96*std_error
   - Colour: significant p<0.05 = "#52B788", else "#6B7F94"
   - Highlight M5 row band, annotate M10b
   - save_fig(f3, "f3_forest_plot", 1800, 1100)
7. ── F4: Regime effects ──────────────────────────────────────────────────────
   - Load app_regime_effects.csv
   - SE for error bars: compute from app_coef_long interaction SEs
     (R1: base_se=0.034; R2: sqrt(0.034^2+0.081^2)≈0.087;
      R3: sqrt(0.034^2+0.060^2)≈0.069; R4: sqrt(0.034^2+0.065^2)≈0.074)
   - Regime colours: c("1"="#52B788","2"="#D64545","3"="#E8863A","4"="#4A90D9")
   - Shaded zones above/below zero
   - save_fig(f4, "f4_regime_effects", 1600, 1000)
8. ── F5: GPR comparison ──────────────────────────────────────────────────────
   - Load app_issue1_crimea.csv (2010–2023 subset)
   - Normalise per country, average across countries
   - Two lines + vertical event lines
   - save_fig(f5, "f5_gpr_comparison", 2000, 900)
9. ── F6: Check I scatter (backup) ───────────────────────────────────────────
   - Load app_threat_panel.csv, filter year==2022
   - save_fig(f6, "f6_check_i_scatter", 1400, 1000)
10. ── F7: Bulgaria series (backup) ──────────────────────────────────────────
    - Load app_threat_panel.csv, filter country=="BG"
    - save_fig(f7, "f7_bulgaria_series", 1600, 800)
```

---

## 11. Build sequence

```bash
# Step 1 — render all figures
cd /home/other/projects/pubs
Rscript presentation/render_figures.R

# Step 2 — create reference.pptx
# (Manual step in LibreOffice Impress or PowerPoint)
# Minimum: background #1C2333, title white PT Sans Bold

# Step 3 — render Quarto PPTX
cd presentation
quarto render slides.qmd

# Output: presentation/_output/slides.pptx
# or with output-dir in YAML: presentation/slides.pptx
```

---

## 12. Risk log and mitigations

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `se_net` is NA in regime_effects.csv | Confirmed | Compute from coef_long interaction SEs in render_figures.R |
| `rnaturalearth` network fetch fails | Low | Use `rnaturalearthdata` package (already installed) which has cached data |
| PT Sans not found by showtext | Low | Fallback to "DejaVu Sans" — same family, also installed |
| PPTX layout names don't match reference.pptx | Medium | Use only "Title Slide" and "Title and Content" — always present in OOTB PPTX |
| Figure too large for PPTX content area | Low | All images use `width="95%"` in QMD — Quarto scales to fit |
| Dark background not rendering in PPTX | Medium | Must be set in reference.pptx — cannot be set in QMD YAML alone for PPTX format |

---

## 13. Files to create (in order)

1. `presentation/render_figures.R` — renders F1–F7 to `figures/`
2. `presentation/reference.pptx` — PowerPoint master (manual)
3. `presentation/slides.qmd` — full QMD with all 14 slides + 3 backup slides
