#' PCA variance target and low-loading variable check
#'
#' Runs PCA on complete cases and reports:
#' (1) the minimum number of components needed to reach `var_target` cumulative variance explained
#' (2) variables whose maximum absolute loading across the first k components is < `loading_threshold`
#'
#' @param df A data.frame of numeric variables. NAs allowed (rows with any NA are removed for PCA).
#' @param var_target Cumulative variance target in `(0, 1]`. Default 0.90.
#' @param loading_threshold Loading threshold in `[0, 1]`. Default 0.10.
#' @param scale Logical; should variables be scaled before PCA? Default TRUE.
#'
#' @return A list with elements:
#'   \item{pca}{The `prcomp` object.}
#'   \item{k}{Number of components required to reach `var_target`.}
#'   \item{cum_variance}{Cumulative variance explained at k.}
#'   \item{low_loading_vars}{Character vector of variables with max abs loading < `loading_threshold` across first k.}
#'
#' The function also prints a 2-line summary to the console.
#'
#' @export
factor_var <- function(df,
                       var_target = 0.90,
                       loading_threshold = 0.10,
                       scale = TRUE) {

  .check_index_df(df)

  if (!is.numeric(var_target) || length(var_target) != 1 || is.na(var_target) ||
      var_target <= 0 || var_target > 1) {
    stop("`var_target` must be a single numeric value in (0, 1].", call. = FALSE)
  }

  if (!is.numeric(loading_threshold) || length(loading_threshold) != 1 || is.na(loading_threshold) ||
      loading_threshold < 0 || loading_threshold > 1) {
    stop("`loading_threshold` must be a single numeric value in [0, 1].", call. = FALSE)
  }

  if (!is.logical(scale) || length(scale) != 1) {
    stop("`scale` must be TRUE or FALSE.", call. = FALSE)
  }

  if (ncol(df) < 2) {
    stop("`df` must have at least 2 columns to run PCA.", call. = FALSE)
  }

  # Remove rows with any missing values
  cc <- stats::complete.cases(df)
  df_cc <- df[cc, , drop = FALSE]

  if (nrow(df_cc) < 2) {
    stop("Not enough complete rows for PCA after removing missing values (need at least 2).", call. = FALSE)
  }

  # PCA with optional scaling
  pca <- stats::prcomp(df_cc, center = TRUE, scale. = scale)

  # Variance explained
  sdev2 <- pca$sdev^2
  prop_var <- sdev2 / sum(sdev2)
  cum_var <- cumsum(prop_var)

  # Minimum k such that cum_var[k] >= var_target
  k <- which(cum_var >= var_target)[1]
  cum_at_k <- unname(cum_var[k])

  # Loadings and low-loading variables across first k components
  loadings <- pca$rotation[, seq_len(k), drop = FALSE]
  max_abs_loading <- apply(abs(loadings), 1, max)

  low_vars <- names(max_abs_loading)[max_abs_loading < loading_threshold]

  # Printed summary
  line1 <- sprintf("%d FACTORS EXPLAIN %.0f%% VARIANCE", k, cum_at_k * 100)

  if (length(low_vars) == 0) {
    line2 <- sprintf("No variables have factor loadings all less than %.0f%%",
                     loading_threshold * 100)
  } else {
    line2 <- sprintf("%s have factor loadings all less than %.0f%%",
                     paste(low_vars, collapse = ", "),
                     loading_threshold * 100)
  }

  cat(line1, "\n", sep = "")
  cat(line2, "\n", sep = "")

  list(
    pca = pca,
    k = k,
    cum_variance = cum_at_k,
    low_loading_vars = low_vars
  )
}
