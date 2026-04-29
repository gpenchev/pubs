# Note: regime boundaries are hardcoded in this file because vis/ helpers
# cannot source scripts/00_setup.R directly. If regime boundaries change
# in 00_setup.R, update get_regime_bands() in helper_regime.R and the
# palette/label vectors below manually.

#' Apply the standard defence panel ggplot2 theme
#'
#' @param base_size Base font size (default 13).
#' @return A ggplot2 theme object.
theme_defence <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle     = ggplot2::element_text(colour = "grey40", size = base_size - 1),
      axis.title        = ggplot2::element_text(size = base_size - 1),
      axis.text         = ggplot2::element_text(colour = "grey30"),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(colour = "grey90"),
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(face = "bold"),
      strip.text        = ggplot2::element_text(face = "bold")
    )
}

# Regime colour palette (matches helper_regime.R band colours)
palette_regime <- c(
  "1" = "#4E79A7",
  "2" = "#F28E2B",
  "3" = "#E15759",
  "4" = "#76B7B2"
)

# Data source colour palette
palette_source <- c(
  "eurostat" = "#59A14F",
  "wdi"      = "#B07AA1",
  "imf_weo"  = "#F28E2B"
)

# Outlier flag colour palette
palette_flag <- c(
  "FALSE" = "grey60",
  "TRUE"  = "#E15759"
)

#' ggplot2 colour scale for regime
#'
#' @param ... Additional arguments passed to scale_colour_manual.
#' @return A ggplot2 scale object.
scale_colour_regime <- function(...) {
  ggplot2::scale_colour_manual(
    values = palette_regime,
    labels = c("1" = "1995-2004", "2" = "2005-2013",
               "3" = "2014-2021", "4" = "2022-2023"),
    name   = "Regime",
    ...
  )
}

#' ggplot2 fill scale for regime
#'
#' @param ... Additional arguments passed to scale_fill_manual.
#' @return A ggplot2 scale object.
scale_fill_regime <- function(...) {
  ggplot2::scale_fill_manual(
    values = palette_regime,
    labels = c("1" = "1995-2004", "2" = "2005-2013",
               "3" = "2014-2021", "4" = "2022-2023"),
    name   = "Regime",
    ...
  )
}
