test_that("plot_contrib_balance returns a ggplot object", {

  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )

  res <- contrib_balance(df)

  p <- plot_contrib_balance(res)

  expect_s3_class(p, "ggplot")
})

