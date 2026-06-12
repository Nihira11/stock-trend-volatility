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

# memoised fit – keyed on (ret, model); changing ticker/lookback changes ret => fresh fit
fit_garch_m <- memoise::memoise(fit_garch)

#' train on the first `split` of the data, then produce 1-step-ahead
#' conditional vol over the held-out tail using the TRAIN parameters
#' (ugarchfilter with fixed coefficients). Compares to realised vol
garch_oos_eval <- function(ret, dates, model = "gjrGARCH", split = 0.8) {
  keep <- !is.na(ret); ret <- as.numeric(ret[keep]); dates <- dates[keep]
  n <- length(ret)
  if (n < 600) return(NULL)
  n_train <- floor(split * n)
  
  spec <- rugarch::ugarchspec(
    variance.model = list(model = model, garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std")
  fit <- tryCatch(rugarch::ugarchfit(spec, ret[seq_len(n_train)], solver = "hybrid"),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  
  # apply train params to the FULL series -> genuine 1-step-ahead OOS sigma
  rugarch::setfixed(spec) <- as.list(rugarch::coef(fit))
  filt <- tryCatch(rugarch::ugarchfilter(spec, ret), error = function(e) NULL)
  if (is.null(filt)) return(NULL)
  sigma_all <- as.numeric(rugarch::sigma(filt)) * sqrt(TRADING_DAYS)
  realized  <- zoo::rollapply(ret, 21, stats::sd, align = "right", fill = NA) * sqrt(TRADING_DAYS)
  
  test <- (n_train + 1):n
  df <- tibble::tibble(date = dates[test], forecast = sigma_all[test], realized = realized[test])
  df <- df[stats::complete.cases(df), ]
  
  rmse  <- sqrt(mean((df$forecast - df$realized)^2))
  naive <- realized[n_train]                                   # "vol stays flat" benchmark
  rmse0 <- sqrt(mean((df$realized - naive)^2))
  list(n_train = n_train, n_test = nrow(df), rmse = rmse,
       skill = 1 - rmse / rmse0, series = df)
}

#' engle's ARCH-LM test for volatility clustering (no extra dependency).
arch_lm_test <- function(ret, lags = 12) {
  e <- as.numeric(stats::na.omit(ret)); e <- e - mean(e)
  u <- e^2; n <- length(u)
  if (n <= lags + 1) return(NULL)
  em  <- stats::embed(u, lags + 1)
  r2  <- summary(stats::lm(em[, 1] ~ em[, -1, drop = FALSE]))$r.squared
  list(statistic = (n - lags) * r2, df = lags,
       p_value = stats::pchisq((n - lags) * r2, df = lags, lower.tail = FALSE))
}