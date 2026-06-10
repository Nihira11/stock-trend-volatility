# ============================================================
# Design:
#   * Fetch the FULL available daily history for a ticker once,
#     cache it as an RDS file, then filter locally by date.
#     -> any date-range change in the UI is instant, no refetch.
#   * Cache is considered fresh for CACHE_MAX_AGE_HOURS.
#   * All failures return NULL (never an error) so the app can
#     show a friendly message instead of crashing.
# ============================================================

# internal helpers

# "^GSPC" / "CBA.AX" -> safe filename "GSPC" / "CBA_AX"
.cache_path <- function(ticker) {
  safe <- gsub("[^A-Za-z0-9]", "_", toupper(trimws(ticker)))
  safe <- gsub("^_+|_+$", "", safe)
  file.path(CACHE_DIR, paste0(safe, ".rds"))
}

.cache_is_fresh <- function(path) {
  if (!file.exists(path)) return(FALSE)
  age_hours <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "hours"))
  age_hours < CACHE_MAX_AGE_HOURS
}

# basic sanity check on whatever Yahoo returned
.is_valid_price_data <- function(df) {
  is.data.frame(df) &&
    nrow(df) > 0 &&
    all(c("date", "open", "high", "low", "close", "volume", "adjusted") %in% names(df))
}

# public API

#' Fetch full daily history for one ticker, with RDS caching.
#' Returns a tibble (symbol, date, open, high, low, close, volume, adjusted) 
#' or NULL if the ticker is invalid / Yahoo is unreachable.
fetch_full_history <- function(ticker) {
  ticker <- toupper(trimws(ticker))
  if (!nzchar(ticker)) return(NULL)

  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  path <- .cache_path(ticker)

  # 1. fresh cache -> use it
  if (.cache_is_fresh(path)) {
    cached <- tryCatch(readRDS(path), error = function(e) NULL)
    if (.is_valid_price_data(cached)) return(cached)
  }

  # 2. fetch full history from Yahoo
  fetched <- tryCatch(
    suppressWarnings(
      tidyquant::tq_get(ticker, get = "stock.prices",
                        from = "1950-01-01", to = Sys.Date())
    ),
    error = function(e) NULL
  )

  if (.is_valid_price_data(fetched)) {
    fetched <- dplyr::arrange(fetched, date)
    saveRDS(fetched, path)
    return(fetched)
  }

  # 3. fetch failed -> fall back to a stale cache if one exists
  if (file.exists(path)) {
    stale <- tryCatch(readRDS(path), error = function(e) NULL)
    if (.is_valid_price_data(stale)) return(stale)
  }

  NULL
}

#' Main entry point used by the app.
#' Filters cached full history to [from, to] and appends log returns.
#' Returns NULL if the ticker is invalid or has no data in range.
get_prices <- function(ticker,
                       from = Sys.Date() - lubridate::years(DEFAULT_YEARS),
                       to   = Sys.Date()) {
  df <- fetch_full_history(ticker)
  if (is.null(df)) return(NULL)

  df <- dplyr::filter(df, date >= as.Date(from), date <= as.Date(to))
  if (nrow(df) < 2) return(NULL)   # need >= 2 rows for a return

  add_log_returns(df)
}

#' Append daily log returns based on adjusted close.
#' Adds: log_return (NA for first row), cum_return (growth of 1).
add_log_returns <- function(df) {
  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      log_return = log(adjusted / dplyr::lag(adjusted)),
      cum_return = exp(cumsum(tidyr::replace_na(log_return, 0)))
    )
}

#' Fetch several tickers into one long tibble (invalid ones dropped).
#' Returns NULL if *none* of the tickers were valid.
get_prices_multi <- function(tickers,
                             from = Sys.Date() - lubridate::years(DEFAULT_YEARS),
                             to   = Sys.Date()) {
  out <- purrr::map(tickers, get_prices, from = from, to = to)
  out <- purrr::compact(out)
  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

#' TRUE if Yahoo knows this ticker (cheap check used for input validation).
ticker_exists <- function(ticker) {
  !is.null(fetch_full_history(ticker))
}
