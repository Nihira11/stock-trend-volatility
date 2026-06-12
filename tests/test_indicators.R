library(testthat)

root <- if (file.exists("global.R")) "." else ".."
source(file.path(root, "global.R"), chdir = TRUE)

test_that("detect_crossovers finds known golden then death flips", {
  dates <- as.Date("2020-01-01") + 0:5
  df <- tibble::tibble(
    date     = dates,
    adjusted = c(10, 11, 12, 13, 12, 11),
    sma_50   = c(1, 1, 3, 3, 3, 1),
    sma_200  = rep(2, 6)
  )
  cx <- detect_crossovers(df)
  expect_equal(nrow(cx), 2)
  expect_equal(cx$type, c("golden", "death"))
  expect_equal(cx$date[cx$type == "golden"], dates[3])  # -1 -> +1
  expect_equal(cx$date[cx$type == "death"],  dates[6])  # +1 -> -1
})

test_that("first non-NA point is never a crossover", {
  df <- tibble::tibble(
    date    = as.Date("2020-01-01") + 0:2,
    adjusted = c(10, 11, 12),
    sma_50  = c(3, 3, 3),     # already above on row 1
    sma_200 = c(2, 2, 2)
  )
  expect_equal(nrow(detect_crossovers(df)), 0)
})

test_that("RSI stays within [0, 100]", {
  set.seed(1)
  df <- tibble::tibble(adjusted = cumsum(rnorm(300)) + 100)
  r  <- na.omit(add_rsi(df, n = 14)$rsi)
  expect_true(all(r >= 0 & r <= 100))
})

test_that("Bollinger upper >= mavg >= lower where defined", {
  set.seed(2)
  df <- add_bollinger(tibble::tibble(adjusted = cumsum(rnorm(120)) + 100), n = 20, sd = 2)
  ok <- !is.na(df$bb_upper)
  expect_true(all(df$bb_upper[ok] >= df$bb_mavg[ok]))
  expect_true(all(df$bb_mavg[ok]  >= df$bb_lower[ok]))
})

test_that("indicators degrade to NA on short series, never error", {
  df <- tibble::tibble(adjusted = c(100, 101, 102))
  expect_silent(add_macd(add_bollinger(add_rsi(df, 14), 20)))
  expect_true(all(is.na(add_rsi(df, 14)$rsi)))
})