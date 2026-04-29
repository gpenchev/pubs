# =============================================================================
# spatial_helpers.R
# Shared spatial and regression helper functions used across the pipeline.
#
# Functions:
#   Spatial weight construction:
#     build_block_w()           — queen contiguity block-diagonal W
#     build_block_w_invdist()   — inverse distance block-diagonal W
#     subset_listw()            — subset a listw to a country subset
#     subset_distband_listw()   — subset a distance-band matrix to a listw
#
#   Model fitting:
#     run_sar_pooled()          — SAR with two-way FE via block-diagonal W
#     run_sem_pooled()          — SEM with two-way FE via block-diagonal W
#
#   Coefficient extraction:
#     extract_spatialreg()      — tidy coefficients from spatialreg model
#     extract_spatial_param()   — extract rho or lambda from spatialreg model
#     extract_plm()             — tidy coefficients from plm model
#     extract_coef()            — scalar coefficient from spatialreg model
#     extract_se()              — scalar SE from spatialreg model
#
#   Hypothesis tests:
#     chow_test()               — Chow structural break test at a given year
#
# All functions are sourced by regression scripts via:
#   source(here::here("scripts", "helpers", "spatial_helpers.R"))
# =============================================================================


# =============================================================================
# SPATIAL WEIGHT CONSTRUCTION
# =============================================================================

#' Build a block-diagonal queen-contiguity spatial weight matrix
#'
#' Constructs a block-diagonal W matrix by stacking per-year queen contiguity
#' matrices. Isolated observations (zero row-sum) are removed rather than
#' self-weighted, because self-weighting would introduce spurious spatial
#' autocorrelation.
#'
#' @param data       Data frame with columns `country` and `year`.
#' @param sp_weights List produced by `01_spatial_weights.R`, containing
#'                   `$countries` (character vector) and `$W_queen` (listw).
#' @return A list with:
#'   \item{W_listw}{Row-standardised block-diagonal listw object.}
#'   \item{data_valid}{Subset of `data` after removing isolated observations.}
#'   \item{years_valid}{Years for which a valid W block was constructed.}
#'   Returns NULL if no valid blocks can be constructed.
build_block_w <- function(data, sp_weights) {
  years_present <- sort(unique(data$year))

  W_blocks <- lapply(years_present, function(yr) {
    d_yr <- data %>%
      dplyr::filter(year == yr) %>%
      dplyr::arrange(country)

    countries_yr <- d_yr$country
    logical_idx  <- sp_weights$countries %in% countries_yr
    n_present    <- sum(logical_idx)

    if (n_present < 2) return(NULL)

    w_sub <- tryCatch(
      spdep::subset.listw(
        sp_weights$W_queen,
        logical_idx,
        zero.policy = TRUE
      ),
      error = function(e) NULL
    )

    if (is.null(w_sub)) return(NULL)

    w_mat <- spdep::listw2mat(w_sub)
    rownames(w_mat) <- sp_weights$countries[logical_idx]
    colnames(w_mat) <- sp_weights$countries[logical_idx]

    w_mat[countries_yr, countries_yr]
  })

  valid          <- !sapply(W_blocks, is.null)
  years_valid    <- years_present[valid]
  W_blocks_valid <- W_blocks[valid]

  if (sum(valid) == 0) return(NULL)

  data_valid <- data %>%
    dplyr::filter(year %in% years_valid) %>%
    dplyr::arrange(year, country)

  n_total <- nrow(data_valid)
  W_full  <- matrix(0, nrow = n_total, ncol = n_total)

  row_idx <- 1
  for (i in seq_along(W_blocks_valid)) {
    b   <- W_blocks_valid[[i]]
    n_b <- nrow(b)
    W_full[row_idx:(row_idx + n_b - 1),
           row_idx:(row_idx + n_b - 1)] <- b
    row_idx <- row_idx + n_b
  }

  rs        <- rowSums(W_full)
  zero_rows <- which(rs == 0)
  if (length(zero_rows) > 0) {
    warning(length(zero_rows),
            " isolated observations removed from spatial estimation (zero neighbours).")
    W_full     <- W_full[-zero_rows, -zero_rows]
    data_valid <- data_valid[-zero_rows, ]
    rs         <- rowSums(W_full)
  }
  W_full_std <- W_full / rs

  list(
    W_listw     = spdep::mat2listw(W_full_std,
                                    style       = "W",
                                    zero.policy = TRUE),
    data_valid  = data_valid,
    years_valid = years_valid
  )
}


#' Build a block-diagonal inverse-distance spatial weight matrix
#'
#' Constructs a block-diagonal W matrix using inverse centroid distances,
#' capped at 2000 km. Distances are pre-computed in `01_spatial_weights.R`
#' using ETRS89-LAEA projection (EPSG:3035) for metric accuracy.
#'
#' @param data       Data frame with columns `country` and `year`.
#' @param sp_weights List produced by `01_spatial_weights.R`, containing
#'                   `$countries` and `$W_inv_dist` (listw).
#' @return Same structure as `build_block_w`. Returns NULL if no valid blocks.
build_block_w_invdist <- function(data, sp_weights) {
  years_present <- sort(unique(data$year))
  all_countries <- sp_weights$countries

  inv_dist_mat <- spdep::listw2mat(sp_weights$W_inv_dist)
  rownames(inv_dist_mat) <- all_countries
  colnames(inv_dist_mat) <- all_countries

  W_blocks <- lapply(years_present, function(yr) {
    d_yr <- data %>%
      dplyr::filter(year == yr) %>%
      dplyr::arrange(country)

    countries_yr <- d_yr$country
    present      <- countries_yr[countries_yr %in% all_countries]

    if (length(present) < 2) return(NULL)

    w_sub <- inv_dist_mat[present, present]
    rs    <- rowSums(w_sub)

    zero_rows <- which(rs == 0)
    if (length(zero_rows) > 0) {
      w_sub <- w_sub[-zero_rows, -zero_rows]
      rs    <- rowSums(w_sub)
    }

    if (nrow(w_sub) < 2) return(NULL)
    w_sub / rs
  })

  valid          <- !sapply(W_blocks, is.null)
  years_valid    <- years_present[valid]
  W_blocks_valid <- W_blocks[valid]

  if (sum(valid) == 0) return(NULL)

  data_valid <- data %>%
    dplyr::filter(year %in% years_valid) %>%
    dplyr::arrange(year, country)

  n_total <- nrow(data_valid)
  W_full  <- matrix(0, nrow = n_total, ncol = n_total)

  row_idx <- 1
  for (i in seq_along(W_blocks_valid)) {
    b   <- W_blocks_valid[[i]]
    n_b <- nrow(b)
    W_full[row_idx:(row_idx + n_b - 1),
           row_idx:(row_idx + n_b - 1)] <- b
    row_idx <- row_idx + n_b
  }

  rs        <- rowSums(W_full)
  zero_rows <- which(rs == 0)
  if (length(zero_rows) > 0) {
    warning(length(zero_rows),
            " isolated observations removed from inverse distance spatial estimation.")
    W_full     <- W_full[-zero_rows, -zero_rows]
    data_valid <- data_valid[-zero_rows, ]
    rs         <- rowSums(W_full)
  }

  W_full_std <- W_full / rs

  list(
    W_listw     = spdep::mat2listw(W_full_std,
                                    style       = "W",
                                    zero.policy = TRUE),
    data_valid  = data_valid,
    years_valid = years_valid
  )
}


#' Subset a queen-contiguity listw to a set of countries
#'
#' @param listw        A listw object covering `countries_all`.
#' @param countries_all Character vector of all countries in `listw`.
#' @param countries_sub Character vector of the desired subset.
#' @return A listw object restricted to `countries_sub`.
subset_listw <- function(listw, countries_all, countries_sub) {
  logical_idx <- countries_all %in% countries_sub
  spdep::subset.listw(listw, logical_idx, zero.policy = TRUE)
}


#' Build a listw from a distance-band matrix subset
#'
#' Subsets a pre-computed binary distance-band matrix to `countries_sub`,
#' row-normalises, and returns a listw object.
#'
#' @param dist_band_mat Full distance-band matrix (countries x countries).
#' @param countries_sub Character vector of countries to retain.
#' @return A row-standardised listw, or NULL if fewer than 2 countries present.
subset_distband_listw <- function(dist_band_mat, countries_sub) {
  present <- countries_sub[countries_sub %in% rownames(dist_band_mat)]
  if (length(present) < 2) return(NULL)
  w_sub <- dist_band_mat[present, present]
  rs    <- rowSums(w_sub)
  rs[rs == 0] <- 1
  w_sub_std <- w_sub / rs
  tryCatch(
    spdep::mat2listw(w_sub_std, style = "W", zero.policy = TRUE),
    error = function(e) NULL
  )
}


# =============================================================================
# MODEL FITTING
# =============================================================================

#' Fit a SAR model with two-way fixed effects via block-diagonal W
#'
#' Constructs the block-diagonal queen W, removes isolated observations,
#' re-sets factor levels after row removal, and fits a spatial lag model
#' using `spatialreg::lagsarlm`.
#'
#' `zero.policy = TRUE` allows observations with no neighbours to remain
#' in the dataset (their spatial lag is set to zero). This is appropriate
#' here because isolated observations arise from the block structure, not
#' from genuine geographic isolation.
#'
#' @param data         Data frame containing all regression variables.
#' @param formula_vars Character string of the RHS formula (without FE dummies).
#' @param sp_weights   Spatial weights list from `01_spatial_weights.R`.
#' @param label        Label printed in progress messages.
#' @return A `lagsarlm` object, or NULL on failure.
run_sar_pooled <- function(data,
                           formula_vars,
                           sp_weights,
                           label = "SAR") {

  data <- data %>%
    dplyr::arrange(year, country) %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year)
    )

  w_obj <- build_block_w(data, sp_weights)

  if (is.null(w_obj)) {
    message(label, ": no valid W blocks")
    return(NULL)
  }

  data_valid <- w_obj$data_valid %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year),
      regime    = factor(as.character(regime), levels = c("1", "2", "3", "4"))
    )

  fe_formula <- as.formula(
    paste(formula_vars, "+ country_f + year_f")
  )

  result <- tryCatch(
    spatialreg::lagsarlm(
      formula     = fe_formula,
      data        = data_valid,
      listw       = w_obj$W_listw,
      zero.policy = TRUE,
      quiet       = TRUE
    ),
    error = function(e) {
      message(label, " failed: ", e$message)
      NULL
    }
  )

  if (!is.null(result)) {
    message("\n", label, ":")
    print(summary(result))
  }

  result
}


#' Fit a SEM model with two-way fixed effects via block-diagonal W
#'
#' Identical structure to `run_sar_pooled` but uses
#' `spatialreg::errorsarlm` (spatial error model).
#'
#' @param data         Data frame containing all regression variables.
#' @param formula_vars Character string of the RHS formula (without FE dummies).
#' @param sp_weights   Spatial weights list from `01_spatial_weights.R`.
#' @param label        Label printed in progress messages.
#' @return An `errorsarlm` object, or NULL on failure.
run_sem_pooled <- function(data,
                           formula_vars,
                           sp_weights,
                           label = "SEM") {

  data <- data %>%
    dplyr::arrange(year, country) %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year)
    )

  w_obj <- build_block_w(data, sp_weights)

  if (is.null(w_obj)) {
    message(label, ": no valid W blocks")
    return(NULL)
  }

  data_valid <- w_obj$data_valid %>%
    dplyr::mutate(
      country_f = as.factor(country),
      year_f    = as.factor(year),
      regime    = factor(as.character(regime), levels = c("1", "2", "3", "4"))
    )

  fe_formula <- as.formula(
    paste(formula_vars, "+ country_f + year_f")
  )

  result <- tryCatch(
    spatialreg::errorsarlm(
      formula     = fe_formula,
      data        = data_valid,
      listw       = w_obj$W_listw,
      zero.policy = TRUE,
      quiet       = TRUE
    ),
    error = function(e) {
      message(label, " failed: ", e$message)
      NULL
    }
  )

  if (!is.null(result)) {
    message("\n", label, ":")
    print(summary(result))
  }

  result
}


# =============================================================================
# COEFFICIENT EXTRACTION
# =============================================================================

#' Extract tidy coefficient table from a spatialreg model
#'
#' Handles column name variation across spatialreg versions
#' (e.g. "Std. Error" vs "Std.Error", "Pr(>|z|)" vs "Pr(>|t|)").
#'
#' @param model      A `lagsarlm` or `errorsarlm` object.
#' @param model_name Character label for the model column.
#' @return Data frame with columns: model, term, estimate, std_error,
#'         z_stat, p_value. Returns NULL if extraction fails.
extract_spatialreg <- function(model, model_name) {
  if (is.null(model)) return(NULL)

  s        <- summary(model)
  coef_mat <- tryCatch(s$Coef, error = function(e) NULL)
  if (is.null(coef_mat)) {
    coef_mat <- tryCatch(s$coefficients, error = function(e) NULL)
  }
  if (is.null(coef_mat)) return(NULL)
  if (!is.matrix(coef_mat)) coef_mat <- as.matrix(coef_mat)

  col_names <- colnames(coef_mat)
  est_col <- which(col_names %in% c("Estimate", "estimate"))[1]
  se_col  <- which(col_names %in% c("Std. Error", "Std.Error", "std_error"))[1]
  z_col   <- which(col_names %in% c("z value", "z-value", "t value", "t-value"))[1]
  p_col   <- which(col_names %in% c("Pr(>|z|)", "Pr(>|t|)", "p-value"))[1]

  if (any(is.na(c(est_col, se_col, z_col, p_col)))) return(NULL)

  data.frame(
    model     = model_name,
    term      = rownames(coef_mat),
    estimate  = round(as.numeric(coef_mat[, est_col]), 4),
    std_error = round(as.numeric(coef_mat[, se_col]),  4),
    z_stat    = round(as.numeric(coef_mat[, z_col]),   4),
    p_value   = round(as.numeric(coef_mat[, p_col]),   4),
    stringsAsFactors = FALSE
  )
}


#' Extract the spatial parameter (rho or lambda) from a spatialreg model
#'
#' Appends rho (SAR) or lambda (SEM) as an additional row to the coefficient
#' table. The p-value is computed from a Wald z-test (param / SE); if the
#' SE is unavailable the LR test p-value from the model summary is used.
#'
#' @param model      A `lagsarlm` or `errorsarlm` object.
#' @param model_name Character label for the model column.
#' @return Single-row data frame with the spatial parameter, or NULL.
extract_spatial_param <- function(model, model_name) {
  if (is.null(model)) return(NULL)

  s <- summary(model)

  rho    <- tryCatch(as.numeric(model$rho),    error = function(e) NA_real_)
  lambda <- tryCatch(as.numeric(model$lambda), error = function(e) NA_real_)
  if (length(rho)    == 0) rho    <- NA_real_
  if (length(lambda) == 0) lambda <- NA_real_

  is_sar     <- !is.na(rho)
  param_val  <- if (is_sar) rho    else lambda
  param_name <- if (is_sar) "rho"  else "lambda"

  if (is.na(param_val)) return(NULL)

  param_se <- tryCatch({
    se_val <- if (is_sar) as.numeric(s$rho.se) else as.numeric(s$lambda.se)
    if (length(se_val) == 0) NA_real_ else se_val
  }, error = function(e) NA_real_)

  param_z <- tryCatch({
    z_val <- param_val / param_se
    if (is.finite(z_val)) z_val else NA_real_
  }, error = function(e) NA_real_)

  lr_pval <- tryCatch(as.numeric(s$LR1$p.value), error = function(e) NA_real_)
  wald_pval <- tryCatch({
    p_val <- 2 * pnorm(abs(param_z), lower.tail = FALSE)
    if (is.finite(p_val)) p_val else lr_pval
  }, error = function(e) lr_pval)

  data.frame(
    model      = model_name,
    term       = param_name,
    estimate   = round(param_val,  4),
    std_error  = round(param_se,   4),
    z_stat     = round(param_z,    4),
    p_value    = round(wald_pval,  4),
    stringsAsFactors = FALSE
  )
}


#' Extract tidy coefficient table from a plm model
#'
#' @param model      A `plm` object.
#' @param model_name Character label for the model column.
#' @return Data frame with columns: model, term, estimate, std_error,
#'         z_stat, p_value. Returns NULL if extraction fails.
extract_plm <- function(model, model_name) {
  if (is.null(model)) return(NULL)

  coefs <- tryCatch(summary(model)$coefficients, error = function(e) NULL)
  if (is.null(coefs)) return(NULL)

  col_names <- colnames(coefs)
  est_col <- which(col_names %in% c("Estimate"))[1]
  se_col  <- which(col_names %in% c("Std. Error"))[1]
  t_col   <- which(col_names %in% c("t-value", "t value"))[1]
  p_col   <- which(col_names %in% c("Pr(>|t|)"))[1]

  if (any(is.na(c(est_col, se_col, t_col, p_col)))) return(NULL)

  data.frame(
    model     = model_name,
    term      = rownames(coefs),
    estimate  = round(as.numeric(coefs[, est_col]), 4),
    std_error = round(as.numeric(coefs[, se_col]),  4),
    z_stat    = round(as.numeric(coefs[, t_col]),   4),
    p_value   = round(as.numeric(coefs[, p_col]),   4),
    stringsAsFactors = FALSE
  )
}


#' Extract a single coefficient estimate from a spatialreg model
#'
#' @param model A `lagsarlm` or `errorsarlm` object.
#' @param var   Name of the coefficient to extract.
#' @return Numeric scalar, or NA if not found.
extract_coef <- function(model, var) {
  if (is.null(model)) return(NA_real_)
  s  <- summary(model)
  cm <- tryCatch(s$Coef, error = function(e) s$coefficients)
  if (is.null(cm) || !var %in% rownames(cm)) return(NA_real_)
  col_est <- which(colnames(cm) %in% c("Estimate", "estimate"))[1]
  as.numeric(cm[var, col_est])
}


#' Extract a single standard error from a spatialreg model
#'
#' @param model A `lagsarlm` or `errorsarlm` object.
#' @param var   Name of the coefficient whose SE to extract.
#' @return Numeric scalar, or NA if not found.
extract_se <- function(model, var) {
  if (is.null(model)) return(NA_real_)
  s  <- summary(model)
  cm <- tryCatch(s$Coef, error = function(e) s$coefficients)
  if (is.null(cm) || !var %in% rownames(cm)) return(NA_real_)
  col_se <- which(colnames(cm) %in% c("Std. Error", "Std.Error"))[1]
  as.numeric(cm[var, col_se])
}


# =============================================================================
# HYPOTHESIS TESTS
# =============================================================================

#' Chow structural break test at a candidate break year
#'
#' Tests whether the regression coefficients differ significantly between
#' the pre-break and post-break subsamples using an F-test.
#'
#' Formula:
#'   F = [(RSS_restricted - RSS_unrestricted) / k] /
#'       [RSS_unrestricted / (n - 2k)]
#'
#' where RSS_restricted  = pooled model RSS (single regime),
#'       RSS_unrestricted = RSS_pre + RSS_post (split models),
#'       k = number of parameters, n = total observations.
#'
#' @param formula    A formula object for the regression.
#' @param data       Data frame containing all variables.
#' @param break_year Integer. Observations with year >= break_year are "post".
#' @return Data frame with columns: break_year, f_stat, p_value, conclusion.
#'         Returns a row with NA statistics if fitting fails.
chow_test <- function(formula, data, break_year) {
  tryCatch({
    lm_pool <- lm(formula, data = data)
    lm_pre  <- lm(formula, data = data %>% dplyr::filter(year <  break_year))
    lm_post <- lm(formula, data = data %>% dplyr::filter(year >= break_year))

    rss_restricted   <- sum(residuals(lm_pool)^2)
    rss_unrestricted <- sum(residuals(lm_pre)^2) + sum(residuals(lm_post)^2)

    k      <- length(coef(lm_pool))
    n      <- length(residuals(lm_pool))
    f_stat <- ((rss_restricted - rss_unrestricted) / k) /
              (rss_unrestricted / (n - 2 * k))
    p_val  <- pf(f_stat, df1 = k, df2 = n - 2 * k, lower.tail = FALSE)

    data.frame(
      break_year = break_year,
      f_stat     = round(f_stat, 4),
      p_value    = round(p_val,  4),
      conclusion = ifelse(p_val < 0.05,
                          "Structural break present",
                          "No structural break")
    )
  }, error = function(e) {
    message("Chow test at ", break_year, " failed: ", e$message)
    data.frame(
      break_year = break_year,
      f_stat     = NA_real_,
      p_value    = NA_real_,
      conclusion = "Failed"
    )
  })
}
