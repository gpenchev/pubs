# render_figures.R
# Renders all presentation figures to presentation/figures/
# Run from project root: Rscript presentation/render_figures.R
# Requires: ggplot2, dplyr, patchwork, sf, rnaturalearth, ggrepel, showtext, readr, here

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

# ── Fonts ─────────────────────────────────────────────────────────────────────
font_add("PT Sans",
  regular    = "/usr/share/fonts/paratype/pt-sans/pt-sans_regular.ttf",
  bold       = "/usr/share/fonts/paratype/pt-sans/pt-sans_bold.ttf",
  italic     = "/usr/share/fonts/paratype/pt-sans/pt-sans_italic.ttf",
  bolditalic = "/usr/share/fonts/paratype/pt-sans/pt-sans_bold-italic.ttf"
)
showtext_auto()
showtext_opts(dpi = 300)

# ── Colour palette — white background ─────────────────────────────────────────
COL <- list(
  bg       = "white",
  surface  = "#F0F4F8",
  grid     = "#DDE3EC",
  border   = "#C5CFDD",
  text     = "#1C2333",
  text2    = "#3D5068",
  text3    = "#6B7F94",
  blue     = "#2B6CB0",
  green    = "#276749",
  orange   = "#C05621",
  red      = "#9B2C2C",
  grey     = "#4A5568",
  white    = "white"
)

# Regime colours — strong on white
REG_COL <- c("1" = "#276749", "2" = "#9B2C2C", "3" = "#C05621", "4" = "#2B6CB0")

# ── Base theme ────────────────────────────────────────────────────────────────
theme_pres <- function(base_size = 17) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "PT Sans") +
    ggplot2::theme(
      plot.background    = element_rect(fill = COL$bg,      colour = NA),
      panel.background   = element_rect(fill = COL$surface, colour = NA),
      panel.grid.major   = element_line(colour = COL$grid,  linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      panel.border       = element_blank(),
      axis.line          = element_line(colour = COL$border, linewidth = 0.5),
      axis.ticks         = element_line(colour = COL$border),
      plot.title         = element_text(colour = COL$text,  face = "bold",
                                        size = rel(1.15), hjust = 0,
                                        margin = margin(b = 5)),
      plot.subtitle      = element_text(colour = COL$text2, size = rel(0.85),
                                        hjust = 0,
                                        margin = margin(b = 12)),
      plot.caption       = element_text(colour = COL$text3, size = rel(0.72),
                                        hjust = 1),
      axis.title         = element_text(colour = COL$text2, size = rel(0.85)),
      axis.text          = element_text(colour = COL$text2, size = rel(0.80)),
      legend.background  = element_rect(fill = COL$bg, colour = NA),
      legend.key         = element_rect(fill = NA,     colour = NA),
      legend.text        = element_text(colour = COL$text2, size = rel(0.80)),
      legend.title       = element_text(colour = COL$text,  size = rel(0.85)),
      strip.background   = element_rect(fill = "#DDE3EC",   colour = NA),
      strip.text         = element_text(colour = COL$text,  face = "bold",
                                        size = rel(0.85)),
      plot.margin        = margin(16, 18, 12, 18)
    )
}

# ── Save helper ───────────────────────────────────────────────────────────────
out_dir <- here("presentation", "figures")
dir.create(out_dir, showWarnings = FALSE)

save_fig <- function(p, name, width_px, height_px, dpi = 300) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggsave(
    filename = path,
    plot     = p,
    width    = width_px / dpi,
    height   = height_px / dpi,
    dpi      = dpi,
    device   = "png",
    bg       = "white"
  )
  message("Saved: ", basename(path), " (", width_px, "x", height_px, "px)")
}

# ── Data paths ─────────────────────────────────────────────────────────────────
path_events  <- here("scripts", "output", "data",  "ucdp_map_events.csv")
path_panel   <- here("scripts", "output", "app",   "app_threat_panel.csv")
path_coef    <- here("scripts", "output", "app",   "app_coef_long.csv")
path_regime  <- here("scripts", "output", "app",   "app_regime_effects.csv")
path_crimea  <- here("scripts", "output", "app",   "app_issue1_crimea.csv")

# =============================================================================
# F1 — Conflict event map (two-panel: Balkans era vs Ukraine era)
# =============================================================================
message("\n── F1: Conflict map ──")

events_raw <- read_csv(path_events, show_col_types = FALSE)
events_land <- events_raw %>%
  filter(land_contiguous == TRUE, !is.na(lon), !is.na(lat), best > 0)

# Europe bounding box
xlim_eu <- c(-13, 43)
ylim_eu <- c(33, 72)

# Load geometry
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>%
  filter(continent %in% c("Europe") |
           name %in% c("Turkey", "Russia", "Kazakhstan",
                        "Azerbaijan", "Armenia", "Georgia"))

# Country label positions for key states
label_countries <- tibble(
  country = c("POL", "ROU", "LTU", "LVA", "EST",
              "HRV", "BIH", "SRB", "MKD", "UKR"),
  label   = c("PL",  "RO",  "LT",  "LV",  "EE",
              "HR",  "BA",  "RS",  "MK",  "UA"),
  x = c(19.4, 24.9, 23.9, 24.6, 25.0,
        16.4, 17.8, 21.0, 21.7, 31.0),
  y = c(51.9, 45.9, 55.9, 56.9, 58.7,
        45.1, 44.2, 44.0, 41.6, 48.4)
)

# Build one map panel
build_map_panel <- function(years_range, bubble_col, period_label, max_size = 16) {
  ev <- events_land %>%
    filter(year %in% years_range) %>%
    group_by(lon, lat) %>%
    summarise(total_fat = sum(best, na.rm = TRUE), .groups = "drop") %>%
    filter(total_fat > 0)

  ggplot() +
    theme_pres(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "#D6E4F0", colour = NA),  # light blue ocean
      panel.grid       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      axis.line        = element_blank(),
      plot.title       = element_text(size = rel(1.15), colour = COL$text,
                                      face = "bold", hjust = 0.5),
      plot.margin      = margin(4, 4, 4, 4)
    ) +
    # Country fills
    geom_sf(data = europe,
            fill = "#EEF2F7", colour = "#AABCCE",
            linewidth = 0.25) +
    # Conflict bubbles
    geom_point(
      data  = ev,
      aes(x = lon, y = lat, size = log1p(total_fat)),
      colour = bubble_col, alpha = 0.70, shape = 16
    ) +
    scale_size_continuous(range = c(1.5, max_size), guide = "none") +
    # Country labels
    geom_text(
      data  = label_countries,
      aes(x = x, y = y, label = label),
      colour = COL$text, size = 3.8, fontface = "bold",
      family = "PT Sans"
    ) +
    coord_sf(xlim = xlim_eu, ylim = ylim_eu, expand = FALSE) +
    labs(title = period_label)
}

p_balkans <- build_map_panel(1995:2004, COL$red,    "1995–2004  |  Balkans Wars")
p_east    <- build_map_panel(2014:2023, COL$orange, "2014–2023  |  Post-Crimea & Ukraine")

f1 <- p_balkans + p_east +
  plot_annotation(
    title    = "Land-Contiguous Conflict Events — Fatality-Weighted Bubbles",
    subtitle = "UCDP GED 26.1 · ≤50km sea crossing filter · Bubble area ∝ log(cumulative fatalities)",
    theme    = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(colour = COL$text,  face = "bold",
                                     size = 16, family = "PT Sans", hjust = 0.5),
      plot.subtitle   = element_text(colour = COL$text2, size = 11,
                                     family = "PT Sans", hjust = 0.5,
                                     margin = margin(b = 6))
    )
  )

save_fig(f1, "f1_conflict_map", 2400, 1100)

# =============================================================================
# F2 — Threat time series (Eastern vs Western EU, with regime bands)
# =============================================================================
message("\n── F2: Threat time series ──")

panel <- read_csv(path_panel, show_col_types = FALSE)

eastern <- c("PL", "RO", "LT", "LV", "EE")
western <- c("DE", "FR", "ES")
sel_cty <- c(eastern, western)

ts_data <- panel %>%
  filter(country %in% sel_cty) %>%
  mutate(
    group    = if_else(country %in% eastern, "Eastern EU (frontline)", "Western EU"),
    ltype    = if_else(country %in% eastern, "solid", "dashed"),
    # Colour palette per country
    col_map  = case_when(
      country == "PL" ~ "#4A90D9",
      country == "RO" ~ "#52B788",
      country == "LT" ~ "#7EC8E3",
      country == "LV" ~ "#A8D8A8",
      country == "EE" ~ "#34A0C3",
      country == "DE" ~ "#6B7F94",
      country == "FR" ~ "#5A6E80",
      country == "ES" ~ "#4A5E70",
      TRUE            ~ "#6B7F94"
    )
  )

# Endpoint labels
endpoints <- ts_data %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# Regime band data
regime_bands <- tibble(
  xmin  = c(1995, 2005, 2014, 2022),
  xmax  = c(2004, 2013, 2021, 2023),
  label = c("R1", "R2", "R3", "R4"),
  fill  = c("#52B78810", "#D6454510", "#E8863A10", "#4A90D910")
)

f2 <- ggplot(ts_data, aes(x = year, y = threat_land_log,
                           colour = country, group = country)) +
  # Regime background bands
  annotate("rect", xmin = 1995, xmax = 2004.5, ymin = -Inf, ymax = Inf,
           fill = "#52B788", alpha = 0.06) +
  annotate("rect", xmin = 2004.5, xmax = 2013.5, ymin = -Inf, ymax = Inf,
           fill = "#D64545", alpha = 0.06) +
  annotate("rect", xmin = 2013.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           fill = "#E8863A", alpha = 0.06) +
  annotate("rect", xmin = 2021.5, xmax = 2024,   ymin = -Inf, ymax = Inf,
           fill = "#4A90D9", alpha = 0.08) +
  # Regime labels at top
  annotate("text", x = c(1999.5, 2009, 2017.5, 2022.5),
           y = Inf, label = c("R1\nBalkans", "R2\nAusterity",
                              "R3\nPost-Crimea", "R4\nUkraine"),
           colour = COL$text2, size = 3.8, vjust = 1.4,
           family = "PT Sans", fontface = "bold") +
  # Event lines
  geom_vline(xintercept = 2008, colour = COL$text3,
             linetype = "dotted", linewidth = 0.7) +
  geom_vline(xintercept = 2022, colour = COL$text,
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 2008.2, y = 7.2, label = "Financial\ncrisis",
           colour = COL$text3, size = 3.2, hjust = 0, family = "PT Sans") +
  annotate("text", x = 2022.2, y = 7.2, label = "Ukraine\ninvasion",
           colour = COL$text, size = 3.2, hjust = 0, family = "PT Sans") +
  # Lines
  geom_line(aes(linetype = ltype), linewidth = 1.1, alpha = 0.9) +
  scale_linetype_identity() +
  # Colours per country
  scale_colour_manual(
    values = setNames(unique(ts_data$col_map), unique(ts_data$country)),
    guide  = "none"
  ) +
  # Endpoint labels
  geom_text_repel(
    data          = endpoints,
    aes(label = country),
    size          = 4.5,
    fontface      = "bold",
    family        = "PT Sans",
    colour        = COL$text,
    nudge_x       = 0.5,
    direction     = "y",
    hjust         = 0,
    segment.color = COL$text3,
    segment.size  = 0.4,
    box.padding   = 0.2
  ) +
  scale_x_continuous(
    breaks = seq(1995, 2023, by = 4),
    expand = expansion(mult = c(0.01, 0.12))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    title    = "Threat Proximity Index by Country, 1995–2023",
    subtitle = "Solid = Eastern EU frontline  ·  Dashed = Western EU  ·  Regime bands shaded",
    x        = NULL,
    y        = "Threat proximity index (log scale)",
    caption  = "Source: UCDP GED 26.1 · land-contiguous events only"
  ) +
  theme_pres()

save_fig(f2, "f2_threat_timeseries", 2000, 900)

# =============================================================================
# F3 — Coefficient forest plot (threat_land_log across all models)
# =============================================================================
message("\n── F3: Forest plot ──")

coef_raw <- read_csv(path_coef, show_col_types = FALSE)

# Keep one row per model for threat_land_log (drop duplicate M4 base row)
forest_data <- coef_raw %>%
  filter(term == "threat_land_log", !is.na(std_error)) %>%
  mutate(
    ci_lo    = estimate - 1.96 * std_error,
    ci_hi    = estimate + 1.96 * std_error,
    sig      = p_value < 0.05,
    pt_col   = if_else(sig, COL$green, COL$grey),
    # Clean model labels
    model_lbl = dplyr::recode(model,
      "M1: Pooled OLS"        = "M1  Pooled OLS",
      "M2: Country FE"        = "M2  Country FE",
      "M3: Two-way FE"        = "M3  Two-way FE  ★",
      "M4: FE + Regime"       = "M4  FE + Regime",
      "M5: SAR"               = "M5  SAR  ★ primary",
      "M6: SEM"               = "M6  SEM",
      "M7: SAR + Regime"      = "M7  SAR + Regime",
      "M10a: SAR no Finland"  = "M10a  SAR no Finland",
      "M10b: SAR post-2014"   = "M10b  SAR post-2014 †",
      "M10c: SAR pre-2014"    = "M10c  SAR pre-2014",
      "M12: SAR lagged DV"    = "M12  SAR lagged DV"
    )
  ) %>%
  # Sort: M5 on top, then by estimate descending
  mutate(sort_key = if_else(model == "M5: SAR", 99, estimate)) %>%
  arrange(sort_key) %>%
  mutate(model_lbl = factor(model_lbl, levels = model_lbl))

# M5 highlight band y-range
m5_lbl <- forest_data$model_lbl[forest_data$model == "M5: SAR"]

f3 <- ggplot(forest_data,
             aes(x = estimate, y = model_lbl, colour = pt_col)) +
  # M5 highlight band
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = as.numeric(m5_lbl) - 0.45,
           ymax = as.numeric(m5_lbl) + 0.45,
           fill = COL$blue, alpha = 0.10) +
  # Zero line
  geom_vline(xintercept = 0, colour = COL$zero,
             linetype = "dashed", linewidth = 0.8) +
  # CIs
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                orientation = "y",
                width = 0.28, linewidth = 0.9) +
  # Points
  geom_point(size = 4.5) +
  scale_colour_identity() +
  # Estimate label on right
  geom_text(aes(x = ci_hi + 0.003,
                label = sprintf("β = %.3f%s",
                                estimate,
                                if_else(sig, "***", ""))),
            hjust = 0, size = 3.8, colour = COL$text2,
            family = "PT Sans") +
  # M10b annotation
  annotate("text",
           x    = forest_data$ci_lo[forest_data$model == "M10b: SAR post-2014"] - 0.003,
           y    = as.numeric(forest_data$model_lbl[forest_data$model == "M10b: SAR post-2014"]),
           label = "year FE absorbs\ncommon shock",
           hjust = 1, size = 3.2, colour = COL$orange,
           fontface = "italic", family = "PT Sans") +
  scale_x_continuous(
    limits = c(-0.06, 0.22),
    breaks = seq(-0.05, 0.20, by = 0.05)
  ) +
  labs(
    title    = "Threat Coefficient Across All Model Specifications",
    subtitle = "Green = significant (p < 0.05)  ·  Grey = not significant  ·  ★ = primary models",
    x        = "Coefficient estimate (95% CI)",
    y        = NULL,
    caption  = "Dependent variable: defence spending (% GDP)  ·  Threat variable: threat_land_log"
  ) +
  theme_pres() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = rel(0.88), colour = COL$text,
                                      family = "PT Sans")
  )

save_fig(f3, "f3_forest_plot", 1800, 1100)

# =============================================================================
# F4 — Regime net effects (the KEY figure)
# =============================================================================
message("\n── F4: Regime effects ──")

regime_raw <- read_csv(path_regime, show_col_types = FALSE)

# SEs from M7 coef_long interaction terms
int_ses <- coef_raw %>%
  filter(term %in% c("threat_land_log",
                     "threat_land_log:regime2",
                     "threat_land_log:regime3",
                     "threat_land_log:regime4"),
         model == "M7: SAR + Regime") %>%
  select(term, std_error) %>%
  tidyr::pivot_wider(names_from = term, values_from = std_error)

base_se <- as.numeric(int_ses[["threat_land_log"]])
se2 <- sqrt(base_se^2 + as.numeric(int_ses[["threat_land_log:regime2"]])^2)
se3 <- sqrt(base_se^2 + as.numeric(int_ses[["threat_land_log:regime3"]])^2)
se4 <- sqrt(base_se^2 + as.numeric(int_ses[["threat_land_log:regime4"]])^2)
ses <- c(base_se, se2, se3, se4)

regime_plot_df <- regime_raw %>%
  mutate(
    se_calc   = ses,
    ci_lo     = net_coef - 1.96 * se_calc,
    ci_hi     = net_coef + 1.96 * se_calc,
    reg_col   = REG_COL[as.character(regime)],
    x_label   = case_when(
      regime == 1 ~ "R1\n1995–2004\nBalkans wars",
      regime == 2 ~ "R2\n2005–2013\nAusterity decade",
      regime == 3 ~ "R3\n2014–2021\nPost-Crimea",
      regime == 4 ~ "R4\n2022–2023\nUkraine invasion"
    ),
    x_label   = factor(x_label, levels = x_label)
  )

f4 <- ggplot(regime_plot_df,
             aes(x = x_label, y = net_coef, colour = reg_col, fill = reg_col)) +
  # Shaded zones above / below zero
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,    ymax =  Inf,
           fill = COL$green, alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = COL$red, alpha = 0.05) +
  # Zero line
  geom_hline(yintercept = 0, colour = COL$zero,
             linetype = "dashed", linewidth = 1.1) +
  # Connecting line
  geom_line(aes(group = 1), colour = COL$text3,
            linewidth = 1.0, linetype = "solid") +
  # Error bars
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.15, linewidth = 1.2) +
  # Points
  geom_point(size = 9, shape = 21, stroke = 1.8) +
  # Coefficient labels above/below points
  geom_text(
    aes(label   = sprintf("β = %+.3f", net_coef),
        vjust   = if_else(net_coef >= 0, -1.5, 2.5)),
    size    = 5.2,
    fontface = "bold",
    family  = "PT Sans",
    colour  = COL$text
  ) +
  # R4 annotation
  annotate("text",
           x = 4, y = min(regime_plot_df$ci_lo) - 0.03,
           label = "N=44 · 28% power\ndirection confirmed",
           colour = COL$text3, size = 3.6, hjust = 0.5,
           fontface = "italic", family = "PT Sans") +
  # R2 annotation
  annotate("text",
           x = 2, y = min(regime_plot_df$ci_lo) - 0.03,
           label = "Fiscal austerity\ndominated",
           colour = COL$red, size = 3.6, hjust = 0.5,
           fontface = "italic", family = "PT Sans") +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_y_continuous(
    breaks = seq(-0.3, 0.3, by = 0.1),
    labels = function(x) sprintf("%+.2f", x)
  ) +
  labs(
    title    = "Net Threat-Defence Elasticity by Regime (Model M7)",
    subtitle = "Net marginal effect of threat proximity on defence spending (% GDP)  ·  95% CI bars",
    x        = NULL,
    y        = "Net threat-defence elasticity",
    caption  = "LR test: four-regime specification preferred over no-regime (p = 0.030)"
  ) +
  theme_pres(base_size = 18) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = rel(0.90), lineheight = 1.3,
                                      colour = COL$text)
  )

save_fig(f4, "f4_regime_effects", 1600, 1000)

# =============================================================================
# F5 — UCDP vs GPR comparison (2010–2023)
# =============================================================================
message("\n── F5: GPR comparison ──")

crimea_raw <- read_csv(path_crimea, show_col_types = FALSE)

# Normalise per country, then average
gpr_ts <- crimea_raw %>%
  filter(year >= 2010) %>%
  group_by(country) %>%
  mutate(
    threat_n = (threat_land_log - min(threat_land_log, na.rm = TRUE)) /
      (max(threat_land_log, na.rm = TRUE) - min(threat_land_log, na.rm = TRUE) + 1e-9),
    gpr_n    = (gpr_mean - min(gpr_mean, na.rm = TRUE)) /
      (max(gpr_mean, na.rm = TRUE) - min(gpr_mean, na.rm = TRUE) + 1e-9)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    ucdp = mean(threat_n, na.rm = TRUE),
    gpr  = mean(gpr_n,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(ucdp, gpr), names_to = "series", values_to = "value") %>%
  mutate(series_lbl = if_else(series == "ucdp",
                               "UCDP threat index",
                               "GPR perception index"))

f5 <- ggplot(gpr_ts, aes(x = year, y = value,
                          colour = series_lbl, group = series_lbl)) +
  # Event shading: 2014
  annotate("rect", xmin = 2013.8, xmax = 2014.2, ymin = -Inf, ymax = Inf,
           fill = COL$orange, alpha = 0.15) +
  # Event shading: 2022
  annotate("rect", xmin = 2021.8, xmax = 2022.2, ymin = -Inf, ymax = Inf,
           fill = COL$white, alpha = 0.08) +
  # Vertical event lines
  geom_vline(xintercept = 2014, colour = COL$orange,
             linetype = "dashed", linewidth = 1.0) +
  geom_vline(xintercept = 2022, colour = COL$text,
             linetype = "solid",  linewidth = 0.8) +
  # Lines + points
  geom_line(linewidth = 2.0) +
  geom_point(size = 4, shape = 21,
             aes(fill = series_lbl), colour = COL$bg, stroke = 1.2) +
  # Annotations
  annotate("text", x = 2014.2, y = 0.90,
           label = "Crimea annexation\nGPR spikes · UCDP flat",
           colour = COL$orange, size = 3.8, hjust = 0,
           fontface = "italic", family = "PT Sans", lineheight = 1.2) +
  annotate("text", x = 2022.2, y = 0.55,
           label = "Ukraine invasion\nBoth indices spike",
           colour = COL$text2, size = 3.8, hjust = 0,
           fontface = "italic", family = "PT Sans", lineheight = 1.2) +
  scale_colour_manual(
    values = c("UCDP threat index" = COL$blue,
               "GPR perception index" = COL$red),
    name   = NULL
  ) +
  scale_fill_manual(
    values = c("UCDP threat index" = COL$blue,
               "GPR perception index" = COL$red),
    name   = NULL
  ) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    title    = "UCDP Threat Index vs GPR Perception Index, 2010–2023",
    subtitle = "13-country mean · Both series normalised 0–1 per country  ·  ΔAIC = −17.6 in favour of UCDP",
    x        = NULL,
    y        = "Normalised index value",
    caption  = "GPR: Caldara & Iacoviello (2022) · UCDP: GED 26.1 · land-contiguous events"
  ) +
  theme_pres() +
  theme(
    legend.position  = c(0.18, 0.88),
    legend.direction = "vertical",
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

save_fig(f5, "f5_gpr_comparison", 2000, 900)

# =============================================================================
# F6 — Check I: 2022 cross-section scatter (backup B2)
# =============================================================================
message("\n── F6: Check I scatter ──")

panel_2022 <- read_csv(path_panel, show_col_types = FALSE) %>%
  filter(year == 2022, !is.na(threat_land_log), !is.na(defence_gdp))

f6 <- ggplot(panel_2022,
             aes(x = threat_land_log, y = defence_gdp)) +
  geom_smooth(method = "lm", colour = COL$green, fill = COL$green,
              alpha = 0.15, linewidth = 1.2, se = TRUE) +
  geom_point(size = 5, colour = COL$blue, alpha = 0.85) +
  geom_text_repel(
    aes(label = country),
    size          = 4.5,
    colour        = COL$text,
    fontface      = "bold",
    family        = "PT Sans",
    box.padding   = 0.4,
    segment.color = COL$text3,
    segment.size  = 0.4
  ) +
  annotate("label",
           x     = min(panel_2022$threat_land_log, na.rm = TRUE) + 0.2,
           y     = max(panel_2022$defence_gdp, na.rm = TRUE) - 0.1,
           label = "β = +0.381  (p = 0.001)\nR² = 0.56  (N = 22)",
           fill  = COL$surface, colour = COL$green,
           size  = 5, fontface = "bold", family = "PT Sans",
           label.padding = unit(0.5, "lines")) +
  labs(
    title    = "Within-Year Cross-Section: 2022 (Check I)",
    subtitle = "Countries closer to Ukraine spent measurably more — year FE artefact confirmed",
    x        = "Threat proximity index (log)",
    y        = "Defence spending (% GDP)",
    caption  = "OLS, no fixed effects. Controls: debt_gdp, deficit_gdp, gdp_growth"
  ) +
  theme_pres()

save_fig(f6, "f6_check_i_scatter", 1400, 1000)

# =============================================================================
# F7 — Bulgaria procurement spike (backup B3)
# =============================================================================
message("\n── F7: Bulgaria series ──")

bg_data <- read_csv(path_panel, show_col_types = FALSE) %>%
  filter(country == "BG")

f7 <- ggplot(bg_data, aes(x = year)) +
  # Highlight 2019
  annotate("rect", xmin = 2018.5, xmax = 2019.5, ymin = -Inf, ymax = Inf,
           fill = COL$red, alpha = 0.18) +
  geom_line(aes(y = defence_gdp), colour = COL$blue,
            linewidth = 1.5) +
  geom_point(aes(y = defence_gdp), colour = COL$blue,
             size = 3.5, shape = 21, fill = COL$blue) +
  geom_line(aes(y = threat_land_log * 0.4), colour = COL$orange,
            linewidth = 1.0, linetype = "dashed") +
  annotate("text", x = 2019, y = 3.4,
           label = "F-16 procurement\ncontract (one-off\nbudget recording)",
           colour = COL$red, size = 3.8, hjust = 0.5, vjust = 0,
           fontface = "italic", family = "PT Sans", lineheight = 1.2) +
  annotate("text", x = 2016.5, y = 0.7,
           label = "Threat index\n(right scale ×0.4)",
           colour = COL$orange, size = 3.5, hjust = 0,
           fontface = "italic", family = "PT Sans") +
  scale_x_continuous(breaks = seq(1995, 2023, by = 4)) +
  scale_y_continuous(
    name       = "Defence spending (% GDP)",
    sec.axis   = sec_axis(~ . / 0.4, name = "Threat index (log)")
  ) +
  labs(
    title    = "Bulgaria: Defence Spending and Threat Index, 1995–2023",
    subtitle = "2019 spike = F-16 procurement recorded as single-year expenditure  ·  Cook's D = 0.099",
    x        = NULL,
    caption  = "Sources: WDI/SIPRI (defence); UCDP GED 26.1 (threat)"
  ) +
  theme_pres() +
  theme(
    axis.title.y.right = element_text(colour = COL$orange),
    axis.text.y.right  = element_text(colour = COL$orange)
  )

save_fig(f7, "f7_bulgaria_series", 1600, 800)

# ── Summary ───────────────────────────────────────────────────────────────────
message("\n── All figures rendered ──")
message("Output directory: ", out_dir)
message("Files: ", paste(list.files(out_dir, pattern = "\\.png$"), collapse = ", "))
