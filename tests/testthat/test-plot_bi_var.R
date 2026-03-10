library(testthat)
library(INXVALID)

test_that("plot_bi_var returns a ggplot object", {
  df <- data.frame(
    x1 = 1:20,
    x2 = 1:20,
    x3 = rnorm(20)
  )

  res <- bi_var(df, threshold = 0.9)

  p <- plot_bi_var(res)

  expect_s3_class(p, "ggplot")
})

test_that("plot_bi_var errors when there is nothing to plot", {
  set.seed(123)

  df <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  res <- bi_var(df, threshold = 0.9)

  if (nrow(res) == 0) {
    expect_error(plot_bi_var(res))
  }
})
