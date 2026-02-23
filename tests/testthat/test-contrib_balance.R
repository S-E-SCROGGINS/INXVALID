library(testthat)
library(INXVALID)

test_that("contrib_balance returns shares that sum to 1 when informative", {
  set.seed(1)
  n <- 200
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.2)  # correlated with x1
  x3 <- rnorm(n)                # mostly independent

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  res <- contrib_balance(df)

  expect_true(is.list(res))
  expect_true(all(c("summary", "table", "index") %in% names(res)))
  expect_true(is.data.frame(res$table))
  expect_true(abs(sum(res$table$share) - 1) < 1e-8)
})

test_that("contrib_balance handles missing values without dropping all rows", {
  set.seed(2)
  df <- data.frame(
    a = c(rnorm(50), NA, NA),
    b = c(rnorm(50), 1, 2),
    c = c(rnorm(50), 3, 4)
  )

  res <- contrib_balance(df)

  expect_true(length(res$index) == nrow(df))
  expect_true(is.numeric(res$index))
})

test_that("contrib_balance accepts named and unnamed weights", {
  df <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))

  res1 <- contrib_balance(df, weights = c(a = 0.2, b = 0.3, c = 0.5))
  expect_true(abs(sum(res1$table$share) - 1) < 1e-8)

  res2 <- contrib_balance(df, weights = c(0.2, 0.3, 0.5))
  expect_true(abs(sum(res2$table$share) - 1) < 1e-8)

  expect_error(contrib_balance(df, weights = c(a = 0.2, b = 0.8))) # missing c
})
