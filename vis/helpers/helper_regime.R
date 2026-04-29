# Note: regime boundaries are hardcoded here because vis/ helpers cannot
# source scripts/00_setup.R directly. If boundaries change in 00_setup.R,
# update year_start and year_end values in get_regime_bands() manually.

#' Get regime band definitions for plot shading
#'
#' Returns a data frame defining the four analytical regimes used throughout
#' the pipeline. Used by add_regime_shading() to add background bands to
#' time series plots.
#'
#' Regime definitions (must match 00_setup.R):
#'   Regime 1: 1995-2004 — Post-Cold War consolidation
#'   Regime 2: 2005-2013 — Pre-crisis stability
#'   Regime 3: 2014-2021 — Post-Crimea rearmament
#'   Regime 4: 2022-2023 — Post-Ukraine invasion surge
#'
#' @return Data frame with columns: regime, label, year_start, year_end, fill.
get_regime_bands <- function() {
  data.frame(
    regime      = c(1L, 2L, 3L, 4L),
    label       = c("1995-2004", "2005-2013", "2014-2021", "2022-2023"),
    year_start  = c(1995, 2005, 2014, 2022),
    year_end    = c(2004, 2013, 2021, 2023),
    fill        = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")
  )
}

#' Add regime shading rectangles to a ggplot2 object
#'
#' Adds semi-transparent coloured rectangles corresponding to the four
#' analytical regimes. Should be called before adding data geoms so that
#' the shading appears behind the data.
#'
#' @param gg    A ggplot2 object with a continuous x axis mapped to year.
#' @param alpha Transparency of the shading rectangles (default 0.08).
#' @return The ggplot2 object with regime shading added.
add_regime_shading <- function(gg, alpha = 0.08) {
  bands <- get_regime_bands()
  for (i in seq_len(nrow(bands))) {
    gg <- gg +
      ggplot2::annotate(
        "rect",
        xmin  = bands$year_start[i] - 0.5,
        xmax  = bands$year_end[i]   + 0.5,
        ymin  = -Inf,
        ymax  = Inf,
        fill  = bands$fill[i],
        alpha = alpha
      )
  }
  gg
}
