library(testthat)
root <- if (file.exists("global.R")) "." else ".."
source(file.path(root, "global.R"), chdir = TRUE)

mk <- function(sym, seed) {
  set.seed(seed); dates <- as.Date("2020-01-01") + 0:99
  tibble::tibble(symbol = sym, date = dates,
                 adjusted = cumprod(c(100, 1 + rnorm(99, 0, 0.01))))
}

test_that("cor_matrix is symmetric with unit diagonal", {
  dm <- dplyr::bind_rows(mk("AAA", 1), mk("BBB", 2))
  cm <- cor_matrix(dm)
  expect_equal(unname(diag(cm)), c(1, 1), tolerance = 1e-8)
  expect_equal(cm["AAA", "BBB"], cm["BBB", "AAA"])
})

test_that("summary_table returns one row per symbol with expected columns", {
  dm <- dplyr::bind_rows(mk("AAA", 1), mk("BBB", 2), mk("CCC", 3))
  s  <- summary_table(dm)
  expect_equal(nrow(s), 3)
  expect_true(all(c("symbol","ann_return","ann_vol","sharpe","max_dd") %in% names(s)))
})