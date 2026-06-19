# theme_figures.R
# Shared white theme, colours, fonts and data-loading helpers.
# Sourced by the setup chunk in slides.qmd.

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggrepel)
  library(showtext)
  library(scales)
})

# ── Fonts ─────────────────────────────────────────────────────────────────────
pt_regular   <- "/usr/share/fonts/paratype/pt-sans/pt-sans_regular.ttf"
pt_bold      <- "/usr/share/fonts/paratype/pt-sans/pt-sans_bold.ttf"
pt_italic    <- "/usr/share/fonts/paratype/pt-sans/pt-sans_italic.ttf"
pt_boldital  <- "/usr/share/fonts/paratype/pt-sans/pt-sans_bold-italic.ttf"

if (file.exists(pt_regular)) {
  font_add("PT Sans",
    regular    = pt_regular,
    bold       = pt_bold,
    italic     = pt_italic,
    bolditalic = pt_boldital
  )
  .pres_font <- "PT Sans"
} else {
  # Fallback: Liberation Sans (metrically compatible, always present)
  lib_dir <- "/usr/share/fonts/liberation"
  font_add("Liberation Sans",
    regular    = file.path(lib_dir, "LiberationSans-Regular.ttf"),
    bold       = file.path(lib_dir, "LiberationSans-Bold.ttf"),
    italic     = file.path(lib_dir, "LiberationSans-Italic.ttf"),
    bolditalic = file.path(lib_dir, "LiberationSans-BoldItalic.ttf")
  )
  .pres_font <- "Liberation Sans"
}
showtext_auto()
showtext_opts(dpi = 200)

# ── Colour palette — white background ─────────────────────────────────────────
COL <- list(
  bg       = "white",
  surface  = "#F0F4F8",   # very light blue-grey panel background
  grid     = "#DDE3EC",   # subtle grid lines
  border   = "#C5CFDD",   # axis lines
  text     = "#1C2333",   # near-black for titles
  text2    = "#3D5068",   # dark blue-grey for subtitles/labels
  text3    = "#6B7F94",   # medium grey for captions/secondary
  blue     = "#2B6CB0",   # strong blue
  green    = "#276749",   # strong green
  orange   = "#C05621",   # strong orange
  red      = "#9B2C2C",   # strong red
  grey     = "#4A5568",   # dark grey neutral
  zero     = "#1C2333",   # zero reference lines
  white    = "white"
)

# Regime colours — strong, readable on white
REG_COL <- c(
  "1" = "#276749",   # green  — rational response
  "2" = "#9B2C2C",   # red    — austerity, negative elasticity
  "3" = "#C05621",   # orange — partial recovery
  "4" = "#2B6CB0"    # blue   — return/Ukraine
)

# ── Base theme ─────────────────────────────────────────────────────────────────
theme_pres <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = .pres_font) +
  theme(
    plot.background    = element_rect(fill = COL$bg,      colour = NA),
    panel.background   = element_rect(fill = COL$surface, colour = NA),
    panel.grid.major   = element_line(colour = COL$grid,  linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line          = element_line(colour = COL$border, linewidth = 0.5),
    axis.ticks         = element_line(colour = COL$border, linewidth = 0.4),
    plot.title         = element_text(colour = COL$text,  face = "bold",
                                      size = rel(1.15), hjust = 0,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = COL$text2, size = rel(0.85),
                                      hjust = 0,
                                      margin = margin(b = 10)),
    plot.caption       = element_text(colour = COL$text3, size = rel(0.70),
                                      hjust = 1),
    axis.title         = element_text(colour = COL$text2, size = rel(0.85)),
    axis.text          = element_text(colour = COL$text2, size = rel(0.80)),
    legend.background  = element_rect(fill = COL$bg,  colour = NA),
    legend.key         = element_rect(fill = NA,      colour = NA),
    legend.text        = element_text(colour = COL$text2, size = rel(0.80)),
    legend.title       = element_text(colour = COL$text,  size = rel(0.85)),
    strip.background   = element_rect(fill = "#DDE3EC", colour = NA),
    strip.text         = element_text(colour = COL$text,  face = "bold",
                                      size = rel(0.85)),
    plot.margin        = margin(10, 14, 8, 14)
  )
}

# ── Data loading (cached by caller) ───────────────────────────────────────────
load_data <- function() {
  list(
    events  = read_csv(here("scripts","output","data","ucdp_map_events.csv"),
                       show_col_types = FALSE),
    panel   = read_csv(here("scripts","output","app","app_threat_panel.csv"),
                       show_col_types = FALSE),
    coef    = read_csv(here("scripts","output","app","app_coef_long.csv"),
                       show_col_types = FALSE),
    regime  = read_csv(here("scripts","output","app","app_regime_effects.csv"),
                       show_col_types = FALSE),
    crimea  = read_csv(here("scripts","output","app","app_issue1_crimea.csv"),
                       show_col_types = FALSE),
    europe  = ne_countries(scale = "medium", returnclass = "sf") |>
                filter(continent == "Europe" |
                       name %in% c("Turkey","Russia","Azerbaijan",
                                   "Armenia","Georgia"))
  )
}
