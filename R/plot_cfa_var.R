#' Plot standardized CFA loadings
#'
#' Creates a bar chart of standardized loadings from the result
#' returned by `cfa_var()`.
#'
#' @param x Result object returned by `cfa_var()`
#'
#' @return A ggplot object
#' @export
plot_cfa_var <- function(x) {

  if (!is.list(x) || !"loadings" %in% names(x)) {
    stop("Input must be the result object returned by cfa_var().", call. = FALSE)
  }

  df <- x$loadings

  required_cols <- c("factor", "variable", "std_loading")
  if (!all(required_cols %in% names(df))) {
    stop("`x$loadings` must contain: factor, variable, std_loading.", call. = FALSE)
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = reorder(variable, std_loading),
      y = std_loading
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ factor, scales = "free_y") +
    ggplot2::labs(
      x = "Variable",
      y = "Standardized Loading",
      title = "Standardized CFA Loadings"
    ) +
    ggplot2::theme_minimal()

  return(p)
}
