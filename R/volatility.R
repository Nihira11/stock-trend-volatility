# realised vol + GARCH family (rugarch)
# All vols annualised with sqrt(TRADING_DAYS)

#' rolling realised volatility (annualised) from log returns
rolling_vol <- function(df, window = 21) {
  ret <- log_returns(df)
  rv  <- zoo::rollapply(ret, width = window, FUN = stats::sd,
                        align = "right", fill = NA) * sqrt(TRADING_DAYS)
  tibble::tibble(date = df$date[-1], roll_vol = rv)
}

#' fit a GARCH(1,1) with Student-t innovations. NULL on too-short data or non-convergence
fit_garch <- function(ret, model = c("sGARCH", "eGARCH", "gjrGARCH")) {
  model <- match.arg(model)
  ret   <- as.numeric(stats::na.omit(ret))
  if (length(ret) < 500) return(NULL)            # rugarch is unstable on short windows
  
  spec <- rugarch::ugarchspec(
    variance.model     = list(model = model, garchOrder = c(1, 1)),
    mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"                   # Student-t: matches the fat tails
  )
  fit <- tryCatch(rugarch::ugarchfit(spec, ret, solver = "hybrid"),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  ok <- tryCatch(fit@fit$convergence == 0, error = function(e) FALSE)
  if (!isTRUE(ok)) NULL else fit
}

#' one-row summary of a fit: ICs, persistence, key params, leverage gamma + p-value
garch_summary <- function(fit) {
  if (is.null(fit)) return(NULL)
  ic   <- rugarch::infocriteria(fit)
  cf   <- fit@fit$matcoef
  pars <- rownames(cf)
  getp  <- function(n) if (n %in% pars) cf[n, 1] else NA_real_ 
  getpv <- function(n) if (n %in% pars) cf[n, 4] else NA_real_
  tibble::tibble(
    model       = fit@model$modeldesc$vmodel,
    AIC         = as.numeric(ic[1]),
    BIC         = as.numeric(ic[2]),
    logLik      = rugarch::likelihood(fit),
    persistence = rugarch::persistence(fit),
    alpha       = getp("alpha1"),
    beta        = getp("beta1"),
    gamma       = getp("gamma1"),
    gamma_pval  = getpv("gamma1")
  )
}

#' conditional (in-sample) annualised vol aligned to dates
conditional_vol <- function(fit, dates) {
  if (is.null(fit)) return(NULL)
  s <- as.numeric(rugarch::sigma(fit)) * sqrt(TRADING_DAYS)
  tibble::tibble(date = dates[seq_along(s)], cond_vol = s)
}

#' n-day-ahead annualised vol forecast (point path)
garch_forecast <- function(fit, n = 30) {
  if (is.null(fit)) return(NULL)
  fc <- rugarch::ugarchforecast(fit, n.ahead = n)
  tibble::tibble(h = seq_len(n),
                 ann_vol = as.numeric(rugarch::sigma(fc)) * sqrt(TRADING_DAYS))
}

#' news-impact curve: shock z -> next-day variance
news_impact <- function(fit) {
  if (is.null(fit)) return(NULL)
  ni <- rugarch::newsimpact(fit)
  tibble::tibble(z = ni$zx, sigma2 = ni$zy)
}

#' stack summaries of several fits, dropping any that failed
compare_garch <- function(fits) {
  rows <- Filter(Negate(is.null), lapply(fits, garch_summary))
  if (!length(rows)) NULL else dplyr::bind_rows(rows)
}

# memoised fit — keyed on (ret, model); changing ticker/lookback changes ret => fresh fit
fit_garch_m <- memoise::memoise(fit_garch)