#' Identify bivariate Pearson correlations above a threshold
#'
#' Computes pairwise Pearson correlations among numeric columns in `df`
#' using pairwise complete observations (i.e., complete cases per variable pair),
#' and returns the pairs with absolute correlation >= `threshold`.
#'
#' @param df A data.frame of numeric index variables. NAs allowed.
#' @param threshold Correlation threshold. Must be one of 0.5, 0.7, or 0.9.
#'
#' @return A data.frame with columns: var1, var2, r.
#' The function also prints the filtered pairs as a summary.
#'
#' @export
bi_var <- function(df, threshold = 0.7) {
  .check_index_df(df)

  allowed <- c(0.5, 0.7, 0.9)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("`threshold` must be a single numeric value: 0.5, 0.7, or 0.9.", call. = FALSE)
  }
  if (!(threshold %in% allowed)) {
    stop("`threshold` must be one of: 0.5, 0.7, 0.9.", call. = FALSE)
  }

  vars <- names(df)
  if (length(vars) < 2) {
    out <- data.frame(var1 = character(), var2 = character(), r = numeric())
    message("Need at least 2 variables to compute bivariate correlations.")
    print(out, row.names = FALSE)
    return(out)
  }

  # All unique variable pairs
  pairs <- utils::combn(vars, 2, simplify = FALSE)

  res_list <- lapply(pairs, function(p) {
    x <- df[[p[1]]]
    y <- df[[p[2]]]

    keep <- stats::complete.cases(x, y)
    if (sum(keep) < 2) return(NULL)

    r <- suppressWarnings(stats::cor(x[keep], y[keep], method = "pearson"))
    if (is.na(r)) return(NULL)

    if (abs(r) >= threshold) {
      data.frame(var1 = p[1], var2 = p[2], r = unname(r), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  out <- do.call(rbind, res_list)
  if (is.null(out)) {
    out <- data.frame(var1 = character(), var2 = character(), r = numeric())
    message(sprintf("No bivariate correlations found with |r| >= %s.", threshold))
    print(out, row.names = FALSE)
    return(out)
  }

  # Order by absolute strength (largest first)
  out <- out[order(abs(out$r), decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL

  # Printed summary
  print(out, row.names = FALSE)

  out
}
