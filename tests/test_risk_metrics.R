library(testthat)
root <- if (file.exists("global.R")) "." else ".."
source(file.path(root, "global.R"), chdir = TRUE)

test_that("annualisation formulas match by hand", {
  ret <- c(0.01, -0.02, 0.015, -0.005)
  expect_equal(ann_volatility(ret), sd(ret) * sqrt(252), tolerance = 1e-10)
  expect_equal(ann_return(ret),     mean(ret) * 252,      tolerance = 1e-10)
  expect_equal(sharpe_ratio(ret, rf = 0.04),
               (mean(ret) * 252 - 0.04) / (sd(ret) * sqrt(252)), tolerance = 1e-10)
})

test_that("monotonically rising wealth has zero drawdown", {
  df <- tibble::tibble(date = as.Date("2020-01-01") + 0:9,
                       cum_return = seq(1, 2, length.out = 10))
  expect_equal(max_drawdown(df)$max, 0)
})

test_that("max drawdown finds the right trough, peak and recovery", {
  df <- tibble::tibble(date = as.Date("2020-01-01") + 0:4,
                       cum_return = c(1.0, 1.2, 0.9, 1.1, 1.3))
  m <- max_drawdown(df)
  expect_equal(m$max, 0.9 / 1.2 - 1, tolerance = 1e-12)  # -25%
  expect_equal(m$peak_date,     df$date[2])
  expect_equal(m$trough_date,   df$date[3])
  expect_equal(m$recovery_date, df$date[5])
})

test_that("VaR on N(0, 0.01) is ~1.645% and CVaR exceeds VaR", {
  set.seed(42)
  r <- rnorm(1e5, 0, 0.01)
  expect_equal(var_historical(r, 0.95)[["95%"]], 0.01 * 1.645, tolerance = 0.001)
  expect_equal(var_parametric(r, 0.95)[["95%"]], 0.01 * qnorm(0.95), tolerance = 0.001)
  expect_equal(var_historical(r, 0.95)[["95%"]],
               var_parametric(r, 0.95)[["95%"]], tolerance = 0.001)   # agree on normal data
  expect_true(cvar_historical(r, 0.95) > var_historical(r, 0.95)[["95%"]])
})