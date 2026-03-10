library(testthat)
library(INXVALID)

test_that("plot_cfa_var returns a ggplot object", {
  skip_if_not_installed("lavaan")
  set.seed(123)

  model <- "
    f1 =~ x1 + x2 + x3
    f2 =~ x4 + x5 + x6
    f1 ~~ f2
  "

  df <- lavaan::simulateData(model, sample.nobs = 200)

  res <- cfa_var(df, model = model, loading_threshold = 0.30)

  p <- plot_cfa_var(res)

  expect_s3_class(p, "ggplot")
})
