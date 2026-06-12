# =====================================================================
# All metrics on daily LOG returns, annualised with TRADING_DAYS (252)
# VaR/CVaR reported as POSITIVE losses
# =====================================================================

#' daily log returns from adjusted close (length n-1, no leading NA)
log_returns <- function(df) diff(log(df$adjusted))

ann_return     <- function(ret) mean(ret, na.rm = TRUE) * TRADING_DAYS
ann_volatility <- function(ret) stats::sd(ret, na.rm = TRUE) * sqrt(TRADING_DAYS)

sharpe_ratio <- function(ret, rf = 0.04) {
  (ann_return(ret) - rf) / ann_volatility(ret)
}

#' max drawdown from the cumulative-return (wealth) path
max_drawdown <- function(df) {
  wealth <- df$cum_return
  peak   <- cummax(wealth)
  dd     <- wealth / peak - 1
  curve  <- tibble::tibble(date = df$date, dd = dd)
  
  i_trough <- which.min(dd)
  i_peak   <- max(which(wealth[seq_len(i_trough)] == peak[seq_len(i_trough)]))
  after    <- seq.int(i_trough, length(wealth))
  rec_hits <- which(wealth[after] >= wealth[i_peak])
  i_rec    <- if (length(rec_hits)) after[rec_hits[1]] else NA_integer_
  
  list(
    curve         = curve,
    max           = dd[i_trough],
    peak_date     = df$date[i_peak],
    trough_date   = df$date[i_trough],
    recovery_date = if (is.na(i_rec)) NA else df$date[i_rec]
  )
}

#' historical VaR: empirical quantile of losses, returned POSITIVE
var_historical <- function(ret, p = c(0.95, 0.99)) {
  q <- stats::quantile(ret, probs = 1 - p, na.rm = TRUE, names = FALSE)
  stats::setNames(-q, paste0(p * 100, "%"))
}

#' parametric (normal) VaR, returned POSITIVE
var_parametric <- function(ret, p = c(0.95, 0.99)) {
  mu <- mean(ret, na.rm = TRUE); sig <- stats::sd(ret, na.rm = TRUE)
  v  <- mu + sig * stats::qnorm(1 - p)
  stats::setNames(-v, paste0(p * 100, "%"))
}

#' historical CVaR / Expected Shortfall: mean loss beyond the VaR cutoff, POSITIVE
cvar_historical <- function(ret, p = 0.95) {
  cutoff <- stats::quantile(ret, probs = 1 - p, na.rm = TRUE, names = FALSE)
  tail   <- ret[ret <= cutoff]
  if (!length(tail)) return(NA_real_)
  -mean(tail, na.rm = TRUE)
}