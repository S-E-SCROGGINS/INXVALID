#' Plot cumulative variance explained from factor_var()
#'
#' Creates a plot of cumulative variance explained by PCA components
#' using the result returned by `factor_var()`.
#'
#' @param x Result object returned by `factor_var()`
#'
#' @return A ggplot object
#' @export
plot_factor_var <- function(x) {

  if (!is.list(x) || !"pca" %in% names(x) || !"k" %in% names(x) || !"cum_variance" %in% names(x)) {
    stop("Input must be the result object returned by factor_var().", call. = FALSE)
  }

  pca <- x$pca
  k <- x$k
  cum_at_k <- x$cum_variance

  sdev2 <- pca$sdev^2
  prop_var <- sdev2 / sum(sdev2)
  cum_var <- cumsum(prop_var)

  df_plot <- data.frame(
    component = seq_along(cum_var),
    cumulative_variance = cum_var
  )

  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = component, y = cumulative_variance)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = k, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = cum_at_k, linetype = "dotted") +
    ggplot2::labs(
      x = "Component",
      y = "Cumulative Variance Explained",
      title = "Cumulative Variance Explained by PCA",
      subtitle = paste0(k, " factors/components explain ", round(cum_at_k * 100, 1), "% of variance")
    ) +
    ggplot2::scale_x_continuous(breaks = seq_along(cum_var)) +
    ggplot2::theme_minimal()

  return(p)
}
