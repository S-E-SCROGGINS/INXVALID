#' Plot contribution balance
#'
#' Creates a horizontal bar chart of variable contribution shares from
#' the `contrib_balance()` function.
#'
#' @param x Result object returned from `contrib_balance()`
#'
#' @return A ggplot object
#' @export
plot_contrib_balance <- function(x) {

  if (!is.list(x) || !"table" %in% names(x)) {
    stop("Input must be the result object returned by contrib_balance().")
  }

  df <- x$table

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = reorder(variable, share),
    y = share
  )) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "Contribution Share",
      title = "Index Contribution Balance"
    ) +
    ggplot2::theme_minimal()

  return(p)
}
