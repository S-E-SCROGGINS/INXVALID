library(testthat)
library(INXVALID)

test_that("cfa_var fits CFA on complete cases and returns expected objects", {
  skip_if_not_installed("lavaan")
  set.seed(123)

  # Simulate data from a simple 2-factor model
  model <- "
    f1 =~ x1 + x2 + x3
    f2 =~ x4 + x5 + x6
    f1 ~~ f2
  "

  df <- lavaan::simulateData(model, sample.nobs = 200)

  # Introduce missingness, ensuring complete cases still exist
  df$x2[1:10] <- NA
  df$x5[11:20] <- NA

  res <- cfa_var(df, model = model, loading_threshold = 0.30)

  expect_true(is.list(res))
  expect_true(all(c("fit", "fit_measures", "loadings", "low_loading_vars") %in% names(res)))
  expect_true(inherits(res$fit, "lavaan"))
  expect_true(is.data.frame(res$fit_measures))
  expect_true(all(c("cfi", "tli", "rmsea", "srmr") %in% names(res$fit_measures)))
  expect_true(is.data.frame(res$loadings))
  expect_true(all(c("factor", "variable", "std_loading") %in% names(res$loadings)))
  expect_true(is.character(res$low_loading_vars))
})

test_that("cfa_var errors on bad inputs", {
  skip_if_not_installed("lavaan")

  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))

  expect_error(cfa_var(df, model = 123))
  expect_error(cfa_var(df, model = ""))
  expect_error(cfa_var(df, model = "f1 =~ a + b", loading_threshold = -0.1))

  # Not enough complete rows
  df2 <- df
  df2[1:9, ] <- NA
  expect_error(cfa_var(df2, model = "f1 =~ a + b + c"))
})
