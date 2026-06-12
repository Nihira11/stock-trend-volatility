#' append simple moving averages of adjusted close.
#' adds columns sma_20, sma_50, sma_200 (NA until enough history).
#' windows longer than the data are skipped silently.
add_moving_averages <- function(df, windows = c(20, 50, 200)) {
  df <- dplyr::arrange(df, date)
  for (w in windows) {
    if (nrow(df) >= w) {
      df[[paste0("sma_", w)]] <- TTR::SMA(df$adjusted, n = w)
    }
  }
  df
}

#' 52-week (last ~252 trading days) high / low of adjusted close.
#' returns list(high = , low = ). Uses whatever history is available
#' if the window is shorter than a year.
stats_52w <- function(df) {
  recent <- utils::tail(dplyr::arrange(df, date), 252)
  list(
    high = max(recent$adjusted, na.rm = TRUE),
    low  = min(recent$adjusted, na.rm = TRUE)
  )
}

# --- Phase 3 indicators ------------------------------------------------------

#' RSI. NA column (never error) if series shorter than the window.
add_rsi <- function(df, n = 14) {
  df$rsi <- if (nrow(df) > n) TTR::RSI(df$adjusted, n = n) else NA_real_
  df
}

#' MACD in price units (percent = FALSE). Adds macd, macd_signal, macd_hist.
add_macd <- function(df, fast = 12, slow = 26, signal = 9) {
  if (nrow(df) > slow + signal) {
    m <- TTR::MACD(df$adjusted, nFast = fast, nSlow = slow, nSig = signal,
                   maType = "EMA", percent = FALSE)
    df$macd        <- m[, "macd"]
    df$macd_signal <- m[, "signal"]
    df$macd_hist   <- df$macd - df$macd_signal
  } else {
    df$macd <- NA_real_; df$macd_signal <- NA_real_; df$macd_hist <- NA_real_
  }
  df
}

#' Bollinger bands. Adds bb_lower, bb_mavg, bb_upper, bb_pct (position in band).
add_bollinger <- function(df, n = 20, sd = 2) {
  if (nrow(df) >= n) {
    b <- TTR::BBands(df$adjusted, n = n, sd = sd)
    df$bb_lower <- b[, "dn"]
    df$bb_mavg  <- b[, "mavg"]
    df$bb_upper <- b[, "up"]
    df$bb_pct   <- b[, "pctB"]
  } else {
    df$bb_lower <- NA_real_; df$bb_mavg <- NA_real_
    df$bb_upper <- NA_real_; df$bb_pct  <- NA_real_
  }
  df
}

#' Golden/death crosses from sma_50 vs sma_200. tibble(date, type, price).
#' The first non-NA row has no prior sign, so it can never register as a cross.
detect_crossovers <- function(df) {
  empty <- tibble::tibble(date = as.Date(character()),
                          type = character(), price = numeric())
  if (!all(c("sma_50", "sma_200") %in% names(df))) return(empty)
  
  d <- df[!is.na(df$sma_50) & !is.na(df$sma_200), ]
  if (nrow(d) < 2) return(empty)
  
  s    <- sign(d$sma_50 - d$sma_200)
  prev <- dplyr::lag(s)
  golden <- which(prev == -1 & s == 1)   # 50 crosses ABOVE 200
  death  <- which(prev ==  1 & s == -1)  # 50 crosses BELOW 200
  
  dplyr::arrange(
    dplyr::bind_rows(
      tibble::tibble(date = d$date[golden], type = "golden", price = d$adjusted[golden]),
      tibble::tibble(date = d$date[death],  type = "death",  price = d$adjusted[death])
    ),
    date
  )
}

#' Trend label from the last row + the numbers that justify it (for the badge).
classify_trend <- function(df) {
  last   <- df[nrow(df), ]
  price  <- last$adjusted
  sma50  <- if ("sma_50"  %in% names(df)) last$sma_50  else NA_real_
  sma200 <- if ("sma_200" %in% names(df)) last$sma_200 else NA_real_
  
  label <- if (is.na(sma50) || is.na(sma200)) {
    "Insufficient data"
  } else if (price > sma200 && sma50 > sma200) {
    "Uptrend"
  } else if (price < sma200 && sma50 < sma200) {
    "Downtrend"
  } else {
    "Sideways"
  }
  list(label = label, price = price, sma_50 = sma50, sma_200 = sma200)
}