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
