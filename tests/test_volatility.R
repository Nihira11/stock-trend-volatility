library(testthat)
root <- if (file.exists("global.R")) "." else ".."
source(file.path(root, "global.R"), chdir = TRUE)

test_that("rolling_vol is sqrt(252)-scaled and correctly warmed up", {
  set.seed(7)
  df <- tibble::tibble(date = as.Date("2020-01-01") + 0:99,
                       adjusted = cumprod(c(100, 1 + rnorm(99, 0, 0.01))))
  rv <- rolling_vol(df, window = 21)
  expect_equal(nrow(rv), 99)
  expect_true(all(is.na(rv$roll_vol[1:20])))
  expect_false(any(is.na(rv$roll_vol[21:99])))
})

test_that("fit_garch refuses to fit too-short series", {
  expect_null(fit_garch(rnorm(100), "sGARCH"))
})

test_that("half-life formula is sane for a persistent process", {
  p <- 0.97
  expect_equal(log(0.5) / log(p), 22.76, tolerance = 0.05)
})