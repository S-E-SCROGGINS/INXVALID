#' Plot bivariate correlation heatmap
#'
#' Creates a lower-triangle heatmap of Pearson correlations among all
#' numeric columns in a data frame using pairwise complete observations.
#'
#' @param df A data.frame of numeric variables. NAs allowed.
#'
#' @return A ggplot object
#' @export
plot_bi_var <- function(df) {

  .check_index_df(df)

  if (ncol(df) < 2) {
    stop("`df` must have at least 2 columns to plot bivariate correlations.", call. = FALSE)
  }

  cor_mat <- stats::cor(df, use = "pairwise.complete.obs", method = "pearson")

  cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  names(cor_df) <- c("var1", "var2", "r")

  # Keep only lower triangle including diagonal
  cor_df$var1 <- factor(cor_df$var1, levels = colnames(cor_mat))
  cor_df$var2 <- factor(cor_df$var2, levels = colnames(cor_mat))

  cor_df$row_id <- as.integer(cor_df$var1)
  cor_df$col_id <- as.integer(cor_df$var2)

  cor_df <- cor_df[cor_df$row_id >= cor_df$col_id, , drop = FALSE]

  p <- ggplot2::ggplot(
    cor_df,
    ggplot2::aes(x = var2, y = var1, fill = r)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", r)),
      size = 3
    ) +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "r",
      title = "Bivariate Correlation Heatmap"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed()

  return(p)
}
