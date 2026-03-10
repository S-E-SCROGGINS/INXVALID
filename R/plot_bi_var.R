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

  # Detect column names automatically
  cn <- names(x)

  var1_col <- cn[1]
  var2_col <- cn[2]
  r_col <- cn[3]

  df <- x
  df$pair <- paste(df[[var1_col]], "-", df[[var2_col]])

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(pair, abs(.data[[r_col]])),
      y = .data[[r_col]]
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
