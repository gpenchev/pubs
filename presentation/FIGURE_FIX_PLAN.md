# Figure Fix Plan — Font Size and Dimensions

---

## 1. The actual problem (measured)

### Slide canvas
- Slide: **10.00 × 7.50 inches**
- Content area (below title bar): **10.00 × 5.50 inches** (approx)
- Full-width content placeholder: **~9.0 × 4.9 inches**
- Two-Content left/right each: **~4.4 × 4.9 inches**

### Root cause A — base_size too large
`theme_pres()` default is `base_size = 14`. This means:

| Element | Current | Effective on slide | Readable max |
|---|---|---|---|
| `plot.title` | 14 × 1.15 = **16.1 pt** | 16 pt | 14 pt |
| `axis.title` | 14 × 0.85 = **11.9 pt** | 12 pt | 10 pt |
| `axis.text` | 14 × 0.80 = **11.2 pt** | 11 pt | 9 pt |
| `legend.text`| 14 × 0.80 = **11.2 pt** | 11 pt | 9 pt |

For a chart rendered at ~9×5 inches and displayed at ~9×5 inches on a 10×7.5 slide, the correct `base_size` is **11** — matching standard chart-in-slides conventions where axis text reads at ~9 pt.

### Root cause B — F1 map being shrunk 76%
F1 chunk is `fig.width=5.8` but the right column in the Two Content layout is only **4.42 inches wide**. Quarto shrinks the image to fit → 76% scale → everything too small. The map labels (`size=2.8`) end up at **6.1 pt** (too small to read). Fix: set chunk dimensions to match the column width.

### Root cause C — F3 forest `base_size` uncalled
F3 uses `theme_pres()` (no argument) = `base_size=14`. At `fig.width=9, fig.height=5.2` displayed at 8.56×4.94, the scale is 0.95 — effectively still 13.3 pt titles. Fix: `theme_pres(10)`.

### Root cause D — F4 regime `theme_pres(13)` 
At `fig.width=5.2` displayed at 4.42 (scale 0.85): effective base_size = 11.1 pt. Acceptable, but the `geom_text` coefficient labels at `size=4.2` come out at **10.2 pt** — slightly large. Reduce to `size=3.6`.

---

## 2. Correct chunk dimensions (measured from PPTX)

### Principle
Set `fig.width` and `fig.height` **equal to the actual PPTX placeholder dimensions** so scale factor = 1.0 and `base_size` directly controls point sizes.

| Figure | Chunk | Current dims | Correct dims | Scale factor |
|---|---|---|---|---|
| F1 map | `f1-map` | 5.8 × 4.2 | **4.4 × 3.8** | 1.00 |
| F3 forest | `f3-forest` | 9.0 × 5.2 | **8.5 × 5.0** | ~1.00 |
| F4 regime | `f4-regime` | 5.2 × 4.2 | **4.4 × 4.0** | ~1.00 |
| F5 GPR | `f5-gpr` | 9.0 × 4.2 | **8.5 × 4.2** | 1.00 |
| F6 Check I | `f6-checki` | 8.5 × 4.8 | **8.5 × 4.8** | 1.00 (already good) |
| F7 Bulgaria | `f7-bulgaria` | 9.0 × 4.0 | **8.5 × 4.0** | 1.00 |

**Note on F1:** Right column (58% of ~9in = 5.2in wide, but actual placeholder = 4.4in). Use `fig.width=4.4, fig.height=3.8`.

---

## 3. Correct base_size per figure

`base_size` sets the reference font size in points. After correcting dimensions to scale=1.0, these values produce the target rendered sizes:

| Figure | base_size | Axis text | Title | Rationale |
|---|---|---|---|---|
| F1 map | **10** | 8 pt | 11.5 pt | Small column, map labels tiny anyway |
| F3 forest | **10** | 8 pt | 11.5 pt | 12 rows — needs breathing room |
| F4 regime | **12** | 9.6 pt | 13.8 pt | Only 4 rows — more space per element |
| F5 GPR | **11** | 8.8 pt | 12.7 pt | Standard line chart |
| F6 Check I | **11** | 8.8 pt | 12.7 pt | Scatter — country labels drive sizing |
| F7 Bulgaria | **11** | 8.8 pt | 12.7 pt | Standard line chart |

---

## 4. Inline annotation size corrections

ggplot `size=` in `geom_text`/`annotate` is in **mm** (1 mm ≈ 2.845 pt at render).
Target: annotations at ~9 pt on slide. Required `size` = 9 / 2.845 = **3.2**.

| Location | Current `size=` | Current effective pt | Corrected `size=` |
|---|---|---|---|
| F1 map country labels | 2.8 | 6.1 pt (shrunk) | **3.2** (at scale=1.0 → 9.1 pt) |
| F3 coefficient labels | 3.5 | 9.5 pt | **3.0** → 8.5 pt |
| F3 M10b annotation | 3.0 | 8.1 pt | **2.8** → 8.0 pt |
| F4 coefficient values | 4.2 | 10.2 pt | **3.5** → 10.0 pt (large is OK here — 4 points only) |
| F4 regime annotations | 3.5 | 8.5 pt | **3.0** → 8.5 pt |
| F5 event annotations | 3.5 | 9.9 pt | **3.0** → 8.5 pt |
| F6 stat annotation label | 4.5 | 12.8 pt | **3.5** → 10 pt |
| F7 procurement annotation | 3.5 | 9.9 pt | **3.0** → 8.5 pt |

---

## 5. F1 map — additional fixes needed

Because F1 is in the right column (4.4 in wide), the two-panel patchwork
must be reconsidered:

**Option A (current):** Two-panel patchwork in right column only  
→ Each panel = 2.2 in wide — very cramped. Map labels unreadable.

**Option B (recommended):** Give F1 the FULL slide as a standalone slide  
→ Move S3 to: left col = text-only (formula + 3 bullets), right col = F1 at full right-column width  
→ OR restructure S3 as: top = formula + bullets, bottom = F1 spanning full width

**Option C — simplest fix:** Keep two-column layout but make F1 a SINGLE-panel map  
(2014–2023 only — the Ukraine era which is most relevant) at `fig.width=4.4, fig.height=4.0`.  
Then add a brief sentence: "Balkans 1995–2004 threat was Balkan-centred; by 2014–2023 shifted east."  
This avoids the cramped two-panel issue entirely.

**Recommendation: Option A with corrected dimensions.**
Keep two-panel but reduce both panels' font to `base_size=9` since they render at 2.2 in each.
Country labels `size=2.5`. The map is read as a visual gestalt (bubble positions), not text.

---

## 6. theme_figures.R change

Change the default in `theme_pres()`:
```r
# FROM:
theme_pres <- function(base_size = 14) {
# TO:
theme_pres <- function(base_size = 11) {
```

This fixes F5, F6, F7 automatically (they call `theme_pres()` without argument).

---

## 7. Full change list — files and lines

### `presentation/theme_figures.R`
- Line: `theme_pres <- function(base_size = 14)` → **`base_size = 11`**

### `presentation/slides.qmd` — chunk headers and theme calls

| Chunk | Change |
|---|---|
| `f1-map` | `fig.width=5.8, fig.height=4.2` → **`fig.width=4.4, fig.height=3.8`**; `theme_pres(12)` → **`theme_pres(9)`**; country label `size=2.8` → **`size=2.5`** |
| `f3-forest` | `fig.width=9, fig.height=5.2` → **`fig.width=8.5, fig.height=5.0`**; `theme_pres()` → **`theme_pres(10)`**; coef label `size=3.5` → **`size=3.0`**; M10b annotation `size=3.0` → **`size=2.8`** |
| `f4-regime` | `fig.width=5.2, fig.height=4.2` → **`fig.width=4.4, fig.height=4.0`**; `theme_pres(13)` → **`theme_pres(12)`**; coef value `size=4.2` → **`size=3.5`**; regime annotations `size=3.5` → **`size=3.0`** |
| `f5-gpr` | `fig.width=9, fig.height=4.2` → **`fig.width=8.5, fig.height=4.2`**; `theme_pres()` → stays as default (now 11); event annotations `size=3.5` → **`size=3.0`** |
| `f6-checki` | dimensions already good (8.5×4.8); `theme_pres()` → stays default; stat label `size=4.5` → **`size=3.5`** |
| `f7-bulgaria` | `fig.width=9, fig.height=4.0` → **`fig.width=8.5, fig.height=4.0`**; annotations `size=3.5` → **`size=3.0`** |

---

## 8. Build sequence

```bash
cd /home/other/projects/pubs/presentation
quarto render slides.qmd    # all figures re-rendered with new dims
# Verify: 17 slides, inspect figures for font readability
```
