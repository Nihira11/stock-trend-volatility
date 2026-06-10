library(testthat)

# load constants + functions without starting the app
suppressPackageStartupMessages({
  library(tidyquant); library(tidyverse)
})
DEFAULT_TICKERS     <- c("NVDA","TSLA","AAPL","MSFT","JPM","KO","^GSPC","CBA.AX","BHP.AX")
root <- if (file.exists("R/data_fetch.R")) "." else ".."

DEFAULT_YEARS       <- 10
CACHE_DIR           <- file.path(root, "cache")
CACHE_MAX_AGE_HOURS <- 24
source(file.path(root, "R", "data_fetch.R"))

test_that("valid ticker returns well-formed data with log returns", {
df <- get_prices("AAPL")
  expect_false(is.null(df))
  expect_true(all(c("date","adjusted","log_return","cum_return") %in% names(df)))
  expect_true(is.na(df$log_return[1]))            # first return undefined
  expect_true(all(!is.na(df$log_return[-1])))     # rest are filled
  expect_gt(nrow(df), 1000)                       # ~10y of daily data
})

test_that("log return math is correct", {
  df <- get_prices("AAPL")
  manual <- log(df$adjusted[3] / df$adjusted[2])
  expect_equal(df$log_return[3], manual, tolerance = 1e-12)
})

test_that("special-character tickers work (index + ASX)", {
  expect_false(is.null(get_prices("^GSPC")))
  expect_false(is.null(get_prices("CBA.AX")))
})

test_that("cache file is created and second call is fast", {
  df <- get_prices("MSFT")
  skip_if(is.null(df), "MSFT fetch failed (Yahoo rate limit?) - rerun in a minute")
  expect_true(file.exists(.cache_path("MSFT")))
  t <- system.time(get_prices("MSFT"))[["elapsed"]]
  expect_lt(t, 1)
})

test_that("garbage input returns NULL, never errors", {
  expect_null(get_prices("DEFINITELYNOTATICKER123"))
  expect_null(get_prices(""))
  expect_null(get_prices("   "))
})

test_that("date filtering respects the window", {
  df <- get_prices("KO", from = Sys.Date() - 365, to = Sys.Date())
  expect_gte(min(df$date), Sys.Date() - 366)
})

test_that("multi-ticker fetch drops invalid tickers silently", {
  df <- get_prices_multi(c("NVDA", "NOTREAL999", "KO"))
  expect_setequal(unique(df$symbol), c("NVDA", "KO"))
})
