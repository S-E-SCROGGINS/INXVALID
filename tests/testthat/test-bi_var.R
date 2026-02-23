library(testthat)
library(INXVALID)

test_that("bi_var returns pairs above threshold using pairwise complete observations", {
  set.seed(1)

  # x2 is strongly correlated with x1
  x1 <- c(1, 2, 3, 4, 5, NA)
  x2 <- c(1, 2, 3, 4, 5, 100) # last pair is excluded due to NA in x1
  x3 <- c(5, 4, 3, 2, 1, 0)   # strong negative correlation with x1 (on complete cases)

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  res <- bi_var(df, threshold = 0.9)

  expect_true(is.data.frame(res))
  expect_true(all(c("var1", "var2", "r") %in% names(res)))

  # Should include x1-x2 and x1-x3 at high threshold
  pairs <- paste(pmin(res$var1, res$var2), pmax(res$var1, res$var2), sep = "-")
  expect_true("x1-x2" %in% pairs)
  expect_true("x1-x3" %in% pairs)

  # Ensure correlations are strong
  expect_true(all(abs(res$r) >= 0.9))
})

test_that("bi_var errors on invalid threshold", {
  df <- data.frame(a = c(0, 1), b = c(1, 0))
  expect_error(bi_var(df, threshold = 0.6))
  expect_error(bi_var(df, threshold = "0.7"))
})
