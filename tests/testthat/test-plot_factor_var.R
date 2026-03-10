library(testthat)
library(INXVALID)

test_that("plot_factor_var returns a ggplot object", {
  set.seed(123)

  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100)
  )

  res <- factor_var(df, var_target = 0.9)

  p <- plot_factor_var(res)

  expect_s3_class(p, "ggplot")
})
