INVALID_TICKER_MSG <- "Couldn't load that ticker. Check the symbol — e.g. NVDA, CBA.AX, ^GSPC."

#' Stop a render with a friendly message unless df is a usable price frame.
require_prices <- function(df) {
  validate(need(!is.null(df) && is.data.frame(df) && nrow(df) > 0, INVALID_TICKER_MSG))
  df
}