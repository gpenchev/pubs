# Presentation Rebuild Plan
## Why 30 slides instead of 15 — diagnosis and fix

---

## 1. Root cause analysis: why 30 slides

### Problem A — Content overflow splitting (most slides)
Quarto PPTX renders text and images into a fixed content placeholder.
When content overflows that placeholder, **Quarto silently creates a
continuation slide** with the overflow. This produced doubles for:

| Intended slide | Quarto produced | Cause |
|---|---|---|
| S3 — Threat measure (two-column) | **Slides 4 + 5** | Two-column `.columns` div: text column and image column each became separate slides |
| S5 — Data and sample (table) | **Slides 7 + 8** | Table + exclusions bullets overflow one placeholder |
| S8 — Spatial structure (table) | **Slides 12 + 13** | 5-row table + two paragraph bullets overflow |
| S9 — GPR comparison (image + table) | **Slides 14 + 15** | Image + small table both in same slide: table spills |
| S10 — Robustness checks (table) | **Slides 16 + 17** | 5-row table + Check F paragraph spills |
| S11 — EU position (table) | **Slides 18 + 19** | Table + two bullet paragraphs overflow |
| S12 — Conclusions (table) | **Slides 20 + 21** | 6-row table + policy paragraph overflow |
| B1 — M5 coefficients (table) | **Slides 25 + 26** | 9-row table + footer text overflows |
| B2 — Check I (image) | **Slides 27 + 28** | Image + caption text below overflow |
| B3 — Bulgaria (image) | **Slides 29 + 30** | Image + caption text below overflow |

### Problem B — Section header slide
`# Backup Slides` (a level-1 heading) creates a dedicated section header
slide → **Slide 24** — a titled blank slide with no content.

### Problem C — Slides without visible titles
Several slides appear titleless in the deck because their title is
a `## {.title-slide}` with class only (no text → empty title placeholder),
or because overflow continuation slides have no heading.

### Problem D — The `---` horizontal rules
The 20 `---` dividers in the QMD do **not** create slides in PPTX at
`slide-level: 2`. They are ignored (unlike revealjs). They are harmless
but unnecessary clutter in the source.

---

## 2. Why figures as chunks instead of static PNGs

### Current approach (static PNG embeds) — problems
1. **Font rendering at projection resolution** — `showtext` renders at
   the DPI set at save time. When Quarto embeds the PNG into PPTX and
   PowerPoint scales it to fill the slide, fonts can look soft/blurry
   because the rasterisation happened at a different size than displayed.
2. **Dark background mismatch** — the figures have dark backgrounds
   (`#1C2333`) but the PPTX slide background is the default Office Theme
   light grey/white. This creates a visible dark rectangle around every
   figure — looks unprofessional on projector.
3. **No live theming** — if the reference.pptx background changes, the
   figures do not adapt.

### Chunk approach — advantages
1. **White background throughout** — set `bg = "white"` in `theme_pres`
   and `ggsave`. Figures blend seamlessly into white PPTX slides.
2. **Rendered at PPTX display size** — Quarto knitr chunks use `fig.width`
   and `fig.height` in *inches at 96 dpi* (PPTX screen units), so the
   text is sized correctly for the slide layout, not for an arbitrary PNG.
3. **Single source of truth** — no separate render step; `quarto render`
   produces everything.
4. **Annotations scale correctly** — `geom_text` sizes are in relative
   units that scale with the figure dimensions specified in the chunk.

### Chunk approach — the one real risk
Long render time if spatial data (rnaturalearth) or heavy data operations
are in the chunk. Mitigation: cache the geometry load (`cache = TRUE` on
the data chunk) and keep plot code lean.

---

## 3. White background theme changes

### In `render_figures.R` (chunks version — now a helper sourced by the QMD)

Change in `theme_pres()`:
- `plot.background` → `element_rect(fill = "white", colour = NA)`
- `panel.background` → `element_rect(fill = "#F5F7FA", colour = NA)` (very light grey)
- `panel.grid.major` → `element_line(colour = "#E0E4EA")`
- `plot.title` → colour `"#1C2333"` (dark navy — high contrast on white)
- `plot.subtitle` → colour `"#4A5E80"`
- `axis.text`, `axis.title` → colour `"#3A4F6A"`
- `legend.background` → `element_rect(fill = "white")`
- `strip.background` → `element_rect(fill = "#E8EDF5")`
- `strip.text` → colour `"#1C2333"`

### Data colours (keep for contrast on white):
- Accent blue → `"#2B6CB0"` (darker, readable on white)
- Accent green → `"#276749"` (dark green)
- Accent orange → `"#C05621"` (dark orange)
- Accent red → `"#9B2C2C"` (dark red)
- Neutral grey → `"#4A5568"`
- Zero lines → `"#1C2333"` (black-navy)
- Regime bands → same colours at 8% alpha (still readable)

### In `ggsave` / chunk output:
- `bg = "white"` on all saves
- `save_fig(..., bg = "white")` helper updated

---

## 4. Slide count fix — from 30 to 15

### Strategy: ruthless content discipline
Every piece of text that currently overflows a slide must either:
(a) be cut entirely — if it belongs in speaker notes not the slide, or
(b) fit in one slide by reducing rows/bullets, or
(c) be merged with an adjacent slide.

### Target slide map (15 slides + 3 backup = 18 total in QMD)

| # | Title | Layout | Figure chunk | Content rule |
|---|---|---|---|---|
| 1 | *(title)* | Title Slide | none | Title + author + QR |
| 2 | The Problem | Title and Content | none | Max 5 bullets |
| 3 | The Threat Measure | Two Content | **F1 chunk** right col | Formula + 3 bullets left; map right |
| 4 | Data and Sample | Title and Content | none | Table 7 rows only — exclusions IN the table as last row, no separate bullets |
| 5 | Strategy | Title and Content | none | 3 numbered blocks — each max 2 lines |
| 6 | Main Result — Forest Plot | Blank / Title and Content | **F3 chunk** full | Title + figure only |
| 7 | Rationality Gap — Regime Effects | Two Content | **F4 chunk** left | Figure left (60%) + 4-row table right |
| 8 | Spatial Structure | Title and Content | none | Table 5 rows + 1 sentence below — cut the second sentence |
| 9 | GPR Comparison | Title and Content | **F5 chunk** | Figure fills content area; 2-row AIC/rho table goes IN figure as annotation, not below it |
| 10 | Robustness | Title and Content | none | Table 5 rows only — cut Check F paragraph (move to notes) |
| 11 | EU Position Reversal | Title and Content | none | 3-row table only — cut post-2014 sentence (in notes) |
| 12 | Conclusions | Title and Content | none | 5-row table max — cut policy sentence (in notes) |
| 13 | Limitations | Title and Content | none | 3 short named bullets — no sub-paragraphs on slide |
| 14 | Closing / Open Science | Title Slide style | none | 4 bullets max + QR |
| — | *(no section header slide)* | — | — | Remove `# Backup Slides` heading entirely |
| B1 | BACKUP: M5 Coefficients | Title and Content | none | 9-row table — cut footer text (in notes) |
| B2 | BACKUP: Check I 2022 | Title and Content | **F6 chunk** | Figure only — cut caption text (in notes) |
| B3 | BACKUP: Bulgaria 2019 | Title and Content | **F7 chunk** | Figure only — cut caption text (in notes) |

**Total: 14 main + 3 backup = 17 `##` headings = 17 slides in PPTX**

Note: Slide 3 (Two Content) and Slide 7 (Two Content) must NOT have overflow.
The column split handles this: left column is text-only, right column is
figure-only → PPTX maps these to two separate content placeholders correctly.

---

## 5. Specific content cuts per overflowing slide

### S3 — Threat measure (currently splits into 2)
**Problem:** `.columns` div with text left + image right creates two separate
content blocks; PPTX Two Content layout has exactly two placeholders, but
Quarto maps the formula line as a third element before the columns.

**Fix:** Move the formula INSIDE the left column. Structure becomes:
- Left col: formula + 3 numbered bullets + source line
- Right col: F1 chunk

This ensures the slide has: title → [left content] [right content] = 3 elements
matching the "Title and Two Content" layout exactly.

### S5 — Data (currently splits into 2)
**Problem:** Table (7 rows) + bold heading + 2 exclusion bullets = too long.

**Fix:** Merge exclusions into the table as two additional rows with an
"Excluded" category. Table becomes 9 rows total — still fits in one slide.
Delete all bullet text after the table.

### S8 — Spatial (currently splits into 2)
**Problem:** 5-row table + 2 sentence paragraph below.

**Fix:** Keep the table. Cut BOTH sentences — they paraphrase the table header
which already says "levels-complementarity — not diffusion in changes". The
table is self-explanatory. One short italic caption line below is fine.

### S9 — GPR (currently splits into 2)
**Problem:** Image + 2-row table below.

**Fix:** Remove the small AIC/rho table from the slide. The numbers are already
annotated inside the F5 figure (ΔAIC = −17.6 in the subtitle). Keep only the
figure. The AIC comparison table goes in speaker notes.

### S10 — Robustness (currently splits into 2)
**Problem:** 5-row table + long Check F paragraph.

**Fix:** Cut the Check F paragraph. It is speaker notes content, not slide
content. The table row already says "Threat stable · ideology reverses sign →
immigration belongs" — that is sufficient.

### S11 — EU position (currently splits into 2)
**Problem:** 4-row table + 2 lines of bullets.

**Fix:** Cut the 2 bullet lines after the table. The last table row already
conveys the shift. Add one italic sentence max: *The relevant cleavage shifted
from left vs right to European integration vs national sovereignty.*

### S12 — Conclusions (currently splits into 2)
**Problem:** 6-row table + 1 policy sentence.

**Fix:** Cut to 5 rows (remove "Alliance coordination" row — it was covered on
S8). The policy sentence goes in speaker notes only.

### B1 — M5 coefficients (currently splits into 2)
**Problem:** 9-row table + footer line.

**Fix:** Cut the footer line entirely — N, log-lik, AIC are in the speaker
notes. The table alone fits.

### B2/B3 — Image backups (currently split into 2 each)
**Problem:** `![](...)` + text paragraphs below the image.

**Fix:** Remove ALL text below the image embed on backup slides. Move caption
text to speaker notes. The figure is self-annotated (β, R², labels are baked
into the PNG/chunk output).

---

## 6. QMD structural changes

### Remove `---` horizontal rules
They are ignored in PPTX mode but add source clutter. Remove all 20 of them.

### Remove `# Backup Slides` section header
Replace with a comment `<!-- Backup Slides -->`. This eliminates slide 24
(the blank section title slide).

### Fix the title slide
Change `## {.title-slide}` to a proper `---` YAML-delimited title slide
or use the Quarto PPTX title slide approach:
```
---
## [empty title — uses YAML front matter]
```
Actually the correct approach for PPTX: the **YAML front matter title/author**
renders as the first slide automatically using the "Title Slide" layout.
Remove the manual `## {.title-slide}` slide entirely — it creates a
duplicate title slide (slides 1 and 2 in the current output are both titles).

### Chunk specification per figure
Each figure chunk needs:
```
#| fig-width: [w]
#| fig-height: [h]
#| fig-dpi: 200
#| out-width: "100%"
#| fig-alt: "[description]"
```

Recommended inch dimensions for PPTX (assuming 10 inch wide content area):

| Figure | fig-width | fig-height | Slide layout |
|---|---|---|---|
| F1 conflict map | 9 | 4.2 | Two Content — right col only (5.5 in wide) → use 5.5 × 4.2 |
| F2 time series | 10 | 4.5 | Title and Content — full width |
| F3 forest plot | 9 | 5.5 | Title and Content — full width |
| F4 regime effects | 5.5 | 4.2 | Two Content — left col only |
| F5 GPR comparison | 9.5 | 4.5 | Title and Content — full width |
| F6 Check I | 8 | 5.0 | Title and Content — full width |
| F7 Bulgaria | 9 | 4.0 | Title and Content — full width |

---

## 7. File structure change

### Current:
- `render_figures.R` → runs separately, produces PNGs
- `slides.qmd` → embeds PNGs as static images

### New:
- `theme_figures.R` → defines `theme_pres_white()`, colour constants,
  `font_add()`, `showtext_auto()`, data loading helpers — **sourced at
  the top of slides.qmd via a setup chunk**
- `slides.qmd` → contains all figure code as named R chunks with
  `cache = TRUE` on the data-loading chunk

The old `render_figures.R` becomes the standalone backup renderer
(keep it — useful for generating PNGs for other purposes, e.g. the paper).
Update it to use white background.

---

## 8. Build sequence (new)

```bash
# One command only:
cd /home/other/projects/pubs/presentation
quarto render slides.qmd
# Output: slides.pptx — all figures rendered inline, white background
```

---

## 9. Files to create / modify

| File | Action |
|---|---|
| `presentation/theme_figures.R` | **Create** — white theme, colours, fonts, data helpers |
| `presentation/slides.qmd` | **Rewrite** — chunks instead of PNG embeds, 15-slide structure, content cuts |
| `presentation/render_figures.R` | **Update** — white background colours only (keep for paper use) |
| `presentation/reference.pptx` | **Keep** — already has correct layouts; optionally set white background in master |
