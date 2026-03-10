#' Plot bivariate correlations from bi_var()
#'
#' Creates a horizontal bar chart of Pearson correlations returned by `bi_var()`.
#'
#' @param x Result object returned by `bi_var()`
#'
#' @return A ggplot object
#' @export
plot_bi_var <- function(x) {

  if (!is.data.frame(x)) {
    stop("Input must be the data.frame returned by bi_var().", call. = FALSE)
  }

  required_cols <- c("var1", "var2", "r")
  if (!all(required_cols %in% names(x))) {
    stop("Input must contain columns: var1, var2, r.", call. = FALSE)
  }

  if (nrow(x) == 0) {
    stop("Input has no rows to plot. bi_var() returned no correlations meeting the threshold.", call. = FALSE)
  }

  df <- x
  df$pair <- paste(df$var1, "-", df$var2)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(pair, abs(r)),
      y = r
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable Pair",
      y = "Pearson Correlation",
      title = "Bivariate Correlations Above Threshold"
    ) +
    ggplot2::theme_minimal()

  return(p)
}
