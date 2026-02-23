library(testthat)
library(INXVALID)

# ----------------------------
# All-NA columns
# ----------------------------
test_that("bi_var handles all-NA column (no crash, xNA not included)", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4),
    x2 = c(1, 2, 3, 4),
    xNA = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  res <- bi_var(df, threshold = 0.5)
  expect_true(is.data.frame(res))
  expect_false(any(res$var1 == "xNA" | res$var2 == "xNA"))
})

test_that("factor_var errors when complete cases vanish due to all-NA column", {
  df <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(4, 5, 6),
    xNA = c(NA_real_, NA_real_, NA_real_)
  )
  expect_error(factor_var(df))
})

test_that("contrib_balance handles all-NA column (score/share = 0)", {
  df <- data.frame(
    x1 = c(0.1, 0.2, 0.3, 0.4),
    x2 = c(0.2, 0.1, 0.4, 0.3),
    xNA = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  res <- contrib_balance(df)
  row <- res$table[res$table$variable == "xNA", ]
  expect_equal(row$score, 0)
  expect_equal(row$share, 0)
})

# ----------------------------
# Zero-variance (constant) variables
# ----------------------------
test_that("bi_var handles constant column (no pairs involving it)", {
  df <- data.frame(
    x1 = 1:10,
    x2 = 1:10,
    x_const = rep(5, 10)
  )
  res <- bi_var(df, threshold = 0.5)
  expect_true(is.data.frame(res))
  expect_false(any(res$var1 == "x_const" | res$var2 == "x_const"))
})

test_that("factor_var errors with constant column when scale=TRUE", {
  df <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x_const = rep(1, 50)
  )
  expect_error(factor_var(df, scale = TRUE))
})

test_that("contrib_balance assigns zero contribution to constant variable", {
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x_const = rep(7, 100)
  )
  res <- contrib_balance(df)
  row <- res$table[res$table$variable == "x_const", ]
  expect_equal(row$sd, 0)
  expect_equal(row$score, 0)
  expect_equal(row$share, 0)
})

# ----------------------------
# Perfect collinearity
# ----------------------------
test_that("bi_var detects perfect collinearity", {
  df <- data.frame(
    x1 = 1:20,
    x2 = 1:20,
    x3 = rnorm(20)
  )
  res <- bi_var(df, threshold = 0.9)
  pairs <- paste(pmin(res$var1, res$var2), pmax(res$var1, res$var2), sep = "-")
  expect_true("x1-x2" %in% pairs)
})

test_that("contrib_balance reflects collinearity (x1 and x2 near top)", {
  set.seed(1)
  x1 <- rnorm(200)
  x2 <- x1
  x3 <- rnorm(200)
  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  res <- contrib_balance(df)
  top2 <- head(res$table$variable, 2)
  expect_true(all(c("x1", "x2") %in% top2))
})

# ----------------------------
# Small sample sizes
# ----------------------------
test_that("factor_var errors with too few complete rows", {
  df <- data.frame(
    x1 = c(1, NA),
    x2 = c(2, 3),
    x3 = c(4, 5)
  )
  expect_error(factor_var(df))
})

test_that("cfa_var errors with too few complete rows", {
  skip_if_not_installed("lavaan")

  df <- data.frame(
    x1 = c(1, 2, 3, 4, NA),
    x2 = c(1, 2, 3, 4, 5),
    x3 = c(2, 3, 4, 5, 6)
  )

  model <- "f1 =~ x1 + x2 + x3"
  expect_error(cfa_var(df, model = model))
})

# ----------------------------
# Extreme / problematic weights
# ----------------------------
test_that("contrib_balance errors on incomplete named weights", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  expect_error(contrib_balance(df, weights = c(a = 0.5, b = 0.5)))
})

test_that("contrib_balance errors on all-zero weights", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_error(contrib_balance(df, weights = c(0, 0)))
})

test_that("contrib_balance allows extreme weights; shares sum to 1", {
  set.seed(1)
  df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  res <- contrib_balance(df, weights = c(a = 0.999, b = 0.001, c = 0))
  expect_true(abs(sum(res$table$share) - 1) < 1e-8)
})

