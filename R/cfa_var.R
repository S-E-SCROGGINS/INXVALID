#' Confirmatory factor analysis for theoretical groupings
#'
#' Fits a CFA model (lavaan syntax) on complete cases only, reports basic fit,
#' and flags variables whose maximum absolute standardized loading is below
#' `loading_threshold` (across their specified factor loadings).
#'
#' @param df A data.frame of numeric variables. NAs allowed (rows with any NA are removed listwise).
#' @param model A lavaan model string defining theoretical groupings (e.g., "f1 =~ x1 + x2").
#' @param loading_threshold Standardized loading threshold in `[0, 1]`. Default 0.30.
#'
#' @return A list with elements:
#'   \item{fit}{The lavaan fitted model object.}
#'   \item{fit_measures}{A 1-row data.frame of selected fit indices (CFI, TLI, RMSEA, SRMR).}
#'   \item{loadings}{A data.frame of factor loadings with standardized loadings.}
#'   \item{low_loading_vars}{Character vector of variables with max abs standardized loading < `loading_threshold`.}
#'
#' The function also prints a 2-line summary to the console.
#'
#' @export
cfa_var <- function(df, model, loading_threshold = 0.30) {
  .check_index_df(df)

  if (!is.character(model) || length(model) != 1 || is.na(model) || nchar(model) == 0) {
    stop("`model` must be a single non-empty character string in lavaan syntax.", call. = FALSE)
  }

  if (!is.numeric(loading_threshold) || length(loading_threshold) != 1 || is.na(loading_threshold) ||
      loading_threshold < 0 || loading_threshold > 1) {
    stop("`loading_threshold` must be a single numeric value in `[0, 1]`.", call. = FALSE)
  }

  # Complete cases only (listwise deletion)
  cc <- stats::complete.cases(df)
  df_cc <- df[cc, , drop = FALSE]

  if (nrow(df_cc) < 5) {
    stop("Not enough complete rows for CFA after removing missing values (need at least 5).", call. = FALSE)
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required for cfa_var(). Please install it.", call. = FALSE)
  }

  # Fit CFA
  fit <- lavaan::cfa(model = model, data = df_cc)

  # Selected fit measures
  fm <- lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
  fit_measures <- as.data.frame(as.list(fm), stringsAsFactors = FALSE)

  # Standardized loadings table (robust across lavaan versions)
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  loadings <- pe[pe$op == "=~", , drop = FALSE]

  # Ensure required columns exist
  required <- c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")
  missing_cols <- setdiff(required, names(loadings))
  if (length(missing_cols) > 0) {
    stop(
      "Unexpected lavaan output: missing columns in parameterEstimates(): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  loadings <- loadings[, required, drop = FALSE]
  names(loadings) <- c("factor", "variable", "loading", "se", "z", "pvalue", "std_loading")

  # Flag low-loading variables: max abs standardized loading across their specified loadings
  # (If a variable appears in multiple loadings, we take the max abs.)
  max_abs <- tapply(abs(loadings$std_loading), loadings$variable, max)
  low_vars <- names(max_abs)[max_abs < loading_threshold]

  # Printed 2-line summary
  line1 <- sprintf(
    "MODEL FIT: CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f",
    fit_measures$cfi, fit_measures$tli, fit_measures$rmsea, fit_measures$srmr
  )

  if (length(low_vars) == 0) {
    line2 <- sprintf("No variables have standardized loadings all less than %.0f%%", loading_threshold * 100)
  } else {
    line2 <- sprintf("LOW LOADING (<%.2f): %s", loading_threshold, paste(low_vars, collapse = ", "))
  }

  cat(line1, "\n", sep = "")
  cat(line2, "\n", sep = "")

  list(
    fit = fit,
    fit_measures = fit_measures,
    loadings = loadings,
    low_loading_vars = low_vars
  )
}
