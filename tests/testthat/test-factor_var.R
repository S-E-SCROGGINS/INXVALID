library(testthat)
library(INXVALID)

test_that("factor_var returns PCA object, k, cum_variance, and low-loading vars", {
  set.seed(123)

  # Construct data:
  # - x1 and x2 are strongly related
  # - x3 is mostly noise (likely low loading vs first component(s), depending on random draw)
  n <- 200
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)
  x3 <- rnorm(n)

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  res <- factor_var(df, var_target = 0.90, loading_threshold = 0.10)

  expect_true(is.list(res))
  expect_true(all(c("pca", "k", "cum_variance", "low_loading_vars") %in% names(res)))
  expect_true(inherits(res$pca, "prcomp"))
  expect_true(is.numeric(res$k) && length(res$k) == 1)
  expect_true(res$cum_variance >= 0.90)
  expect_true(is.character(res$low_loading_vars))
})

test_that("factor_var removes missing rows (complete cases) and still runs when possible", {
  set.seed(1)
  df <- data.frame(
    a = c(rnorm(20), NA, NA),
    b = c(rnorm(20), 1, 2),
    c = c(rnorm(20), 3, 4)
  )

  res <- factor_var(df, var_target = 0.80, loading_threshold = 0.10)

  expect_true(inherits(res$pca, "prcomp"))
  expect_true(res$cum_variance >= 0.80)
})

test_that("factor_var errors on invalid thresholds or insufficient data", {
  df <- data.frame(a = c(1, NA), b = c(2, 3))

  expect_error(factor_var(df, var_target = 0))         # invalid
  expect_error(factor_var(df, var_target = 1.1))       # invalid
  expect_error(factor_var(df, loading_threshold = -1)) # invalid

  # insufficient complete cases (only 1 complete row)
  df2 <- data.frame(a = c(1, NA), b = c(2, 3), c = c(4, 5))
  expect_error(factor_var(df2))
})

test_that("factor_var works with scale = FALSE", {
  set.seed(1)
  df <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  res <- factor_var(df, var_target = 0.8, scale = FALSE)

  expect_true(inherits(res$pca, "prcomp"))
})
