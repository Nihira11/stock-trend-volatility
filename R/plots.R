# ============================================================
# one function per chart, all themed consistently
# ============================================================

MA_COLORS <- c(sma_20 = "#E3C565", sma_50 = "#8A93A6", sma_200 = "#C0564B")
MA_LABELS <- c(sma_20 = "SMA 20",  sma_50 = "SMA 50",  sma_200 = "SMA 200")

#' price line (or candlestick) with SMA overlays.
#' df must already have sma_* columns from add_moving_averages().
plot_price_mas <- function(df, ticker = "", style = c("line", "candlestick")) {
  style <- match.arg(style)
  
  p <- plot_ly(df, x = ~date)
  
  if (style == "candlestick") {
    p <- add_trace(
      p, type = "candlestick",
      open = ~open, high = ~high, low = ~low, close = ~close,
      increasing = list(line = list(color = GAIN_GRN)),
      decreasing = list(line = list(color = LOSS_RED)),
      name = ticker
    )
  } else {
    p <- add_trace(
      p, y = ~adjusted, type = "scatter", mode = "lines",
      line = list(color = GOLD, width = 1.6),
      name = "Adjusted close",
      hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>Close</extra>"
    )
  }
  
  for (col in intersect(names(MA_COLORS), names(df))) {
    p <- add_trace(
      p, y = df[[col]], type = "scatter", mode = "lines",
      line = list(color = MA_COLORS[[col]], width = 1, dash = "solid"),
      name = MA_LABELS[[col]], opacity = 0.9,
      hovertemplate = paste0("%{x|%d %b %Y}<br>$%{y:.2f}<extra>",
                             MA_LABELS[[col]], "</extra>")
    )
  }
  
  plotly_base_layout(
    p,
    legend = list(orientation = "h", x = 0, y = 1.08,
                  bgcolor = "rgba(0,0,0,0)"),
    yaxis  = list(title = "Price", gridcolor = "#33363D"),
    xaxis  = list(title = "", gridcolor = "#33363D",
                  rangeslider = list(visible = FALSE))
  )
}

#' volume bars colored by up/down day.
plot_volume <- function(df) {
  df <- df |>
    dplyr::mutate(updown = ifelse(close >= dplyr::lag(close, default = dplyr::first(close)),
                                  GAIN_GRN, LOSS_RED))
  plot_ly(df, x = ~date, y = ~volume, type = "bar",
          marker = list(color = ~updown, line = list(width = 0)),
          name = "Volume",
          hovertemplate = "%{x|%d %b %Y}<br>%{y:,.0f}<extra>Volume</extra>") |>
    plotly_base_layout(
      showlegend = FALSE,
      bargap = 0,
      yaxis = list(title = "Volume", gridcolor = "#33363D"),
      xaxis = list(title = "", gridcolor = "#33363D")
    )
}

#' price (with MAs) stacked above volume, shared x-axis.
plot_price_volume <- function(df, ticker = "", style = "line") {
  subplot(
    plot_price_mas(df, ticker, style),
    plot_volume(df),
    nrows = 2, heights = c(0.75, 0.25),
    shareX = TRUE, titleY = TRUE
  ) |>
    plotly::layout(
      hovermode = "x unified",
      xaxis  = list(rangeslider = list(visible = FALSE)),
      xaxis2 = list(rangeslider = list(visible = FALSE))
    )
}


# small shape helpers for indicator reference lines / zones
.hline  <- function(y, color, dash = "dash") {
  list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y",
       y0 = y, y1 = y, line = list(color = color, width = 1, dash = dash))
}
.yband  <- function(y0, y1, fill) {
  list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y",
       y0 = y0, y1 = y1, fillcolor = fill, line = list(width = 0), layer = "below")
}

plot_price_bollinger <- function(df, ticker = "") {
  d <- df[!is.na(df$bb_upper), ]
  plot_ly(d, x = ~date) |>
    add_ribbons(ymin = ~bb_lower, ymax = ~bb_upper,
                line = list(color = "rgba(0,0,0,0)"),
                fillcolor = "rgba(201,162,39,0.15)",   # gold @ 15%
                name = "Band", hoverinfo = "skip") |>
    add_trace(y = ~bb_mavg, type = "scatter", mode = "lines",
              line = list(color = MUTED, width = 1, dash = "dash"), name = "SMA 20",
              hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>Mid</extra>") |>
    add_trace(y = ~adjusted, type = "scatter", mode = "lines",
              line = list(color = GOLD, width = 1.6), name = "Adjusted close",
              hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>Close</extra>") |>
    plotly_base_layout(
      legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
      yaxis = list(title = "Price"), xaxis = list(title = "")
    )
}

plot_rsi <- function(df) {
  d <- df[!is.na(df$rsi), ]
  plot_ly(d, x = ~date) |>
    add_trace(y = ~rsi, type = "scatter", mode = "lines",
              line = list(color = GOLD, width = 1.4), name = "RSI",
              hovertemplate = "%{x|%d %b %Y}<br>RSI %{y:.1f}<extra></extra>") |>
    plotly_base_layout(
      showlegend = FALSE,
      yaxis = list(title = "RSI", range = c(0, 100), tickvals = c(0, 30, 50, 70, 100)),
      xaxis = list(title = ""),
      shapes = list(
        .yband(70, 100, "rgba(192,86,75,0.10)"),   # overbought
        .yband(0,  30,  "rgba(95,158,110,0.10)"),  # oversold
        .hline(70, LOSS_RED), .hline(30, GAIN_GRN)
      )
    )
}

plot_macd <- function(df) {
  d <- df[!is.na(df$macd), ]
  bar_col <- ifelse(d$macd_hist >= 0, GAIN_GRN, LOSS_RED)
  plot_ly(d, x = ~date) |>
    add_trace(y = ~macd_hist, type = "bar", opacity = 0.6, name = "Histogram",
              marker = list(color = bar_col, line = list(width = 0)),
              hovertemplate = "%{x|%d %b %Y}<br>Hist %{y:.3f}<extra></extra>") |>
    add_trace(y = ~macd, type = "scatter", mode = "lines",
              line = list(color = GOLD, width = 1.4), name = "MACD",
              hovertemplate = "%{x|%d %b %Y}<br>MACD %{y:.3f}<extra></extra>") |>
    add_trace(y = ~macd_signal, type = "scatter", mode = "lines",
              line = list(color = MUTED, width = 1.2, dash = "dot"), name = "Signal",
              hovertemplate = "%{x|%d %b %Y}<br>Signal %{y:.3f}<extra></extra>") |>
    plotly_base_layout(
      legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
      yaxis = list(title = "MACD"), xaxis = list(title = ""),
      shapes = list(.hline(0, "#33363D", dash = "solid"))
    )
}

plot_price_crossovers <- function(df, crosses, ticker = "") {
  p <- plot_ly(df, x = ~date) |>
    add_trace(y = ~adjusted, type = "scatter", mode = "lines",
              line = list(color = GOLD, width = 1.4), name = "Adjusted close",
              hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>Close</extra>")
  
  if ("sma_50" %in% names(df))
    p <- add_trace(p, y = ~sma_50, type = "scatter", mode = "lines",
                   line = list(color = GOLD_SOFT, width = 1), name = "SMA 50",
                   hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>SMA 50</extra>")
  if ("sma_200" %in% names(df))
    p <- add_trace(p, y = ~sma_200, type = "scatter", mode = "lines",
                   line = list(color = MUTED, width = 1), name = "SMA 200",
                   hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra>SMA 200</extra>")
  
  g <- crosses[crosses$type == "golden", ]
  d <- crosses[crosses$type == "death",  ]
  if (nrow(g) > 0)
    p <- add_trace(p, data = g, x = ~date, y = ~price, type = "scatter",
                   mode = "markers", name = "Golden cross",
                   marker = list(color = GAIN_GRN, size = 11, symbol = "triangle-up",
                                 line = list(color = INK, width = 1)),
                   hovertemplate = "Golden cross<br>%{x|%d %b %Y}<br>$%{y:.2f}<extra></extra>")
  if (nrow(d) > 0)
    p <- add_trace(p, data = d, x = ~date, y = ~price, type = "scatter",
                   mode = "markers", name = "Death cross",
                   marker = list(color = LOSS_RED, size = 11, symbol = "triangle-down",
                                 line = list(color = INK, width = 1)),
                   hovertemplate = "Death cross<br>%{x|%d %b %Y}<br>$%{y:.2f}<extra></extra>")
  
  plotly_base_layout(
    p,
    legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
    yaxis = list(title = "Price"), xaxis = list(title = "")
  )
}

.vline <- function(x, color, dash = "dash") {
  list(type = "line", yref = "paper", y0 = 0, y1 = 1, xref = "x",
       x0 = x, x1 = x, line = list(color = color, width = 1.4, dash = dash))
}

#' histogram of daily log returns + normal fit + VaR markers. Fat tails are the story
plot_return_hist <- function(df) {
  ret  <- log_returns(df)
  mu   <- mean(ret); sig <- stats::sd(ret)
  grid <- seq(min(ret), max(ret), length.out = 200)
  v95  <- var_historical(ret, 0.95)[[1]]
  v99  <- var_historical(ret, 0.99)[[1]]
  
  plot_ly() |>
    add_trace(x = ret, type = "histogram", histnorm = "probability density",
              nbinsx = 80, name = "Daily log returns",
              marker = list(color = "rgba(201,162,39,0.45)",
                            line = list(color = SURFACE, width = 0.5)),
              hovertemplate = "%{x:.3f}<extra></extra>") |>
    add_trace(x = grid, y = dnorm(grid, mu, sig), type = "scatter", mode = "lines",
              line = list(color = INK, width = 1.6), name = "Normal fit",
              hovertemplate = "%{x:.3f}<extra>Normal</extra>") |>
    plotly_base_layout(
      legend = list(orientation = "h", x = 0, y = 1.1, bgcolor = "rgba(0,0,0,0)"),
      bargap = 0.02,
      xaxis = list(title = "Daily log return", tickformat = ".1%"),
      yaxis = list(title = "Density"),
      shapes = list(.vline(-v95, GOLD), .vline(-v99, LOSS_RED)),
      annotations = list(
        list(x = -v95, y = 1, yref = "paper", yanchor = "bottom", showarrow = FALSE,
             text = "95% VaR", font = list(color = GOLD, size = 11)),
        list(x = -v99, y = 1, yref = "paper", yanchor = "bottom", showarrow = FALSE,
             text = "99% VaR", font = list(color = LOSS_RED, size = 11)),
        list(x = 0.98, xref = "paper", y = 0.95, yref = "paper",
             xanchor = "right", yanchor = "top", showarrow = FALSE,
             text = "Empirical tails exceed the normal fit",
             font = list(color = MUTED, size = 11))
      )
    )
}

#' drawdown area chart, max-drawdown trough annotated
plot_drawdown <- function(dd, mdd = NULL) {
  p <- plot_ly(dd, x = ~date, y = ~dd, type = "scatter", mode = "lines",
               fill = "tozeroy", fillcolor = "rgba(192,86,75,0.25)",
               line = list(color = LOSS_RED, width = 1), name = "Drawdown",
               hovertemplate = "%{x|%d %b %Y}<br>%{y:.1%}<extra></extra>") |>
    plotly_base_layout(
      showlegend = FALSE,
      yaxis = list(title = "Drawdown", tickformat = ".0%"),
      xaxis = list(title = "")
    )
  if (!is.null(mdd) && !is.na(mdd$trough_date)) {
    p <- plotly::layout(p, annotations = list(list(
      x = mdd$trough_date, y = mdd$max, xref = "x", yref = "y",
      text = paste0("Max DD ", scales::percent(mdd$max, accuracy = 0.1)),
      showarrow = TRUE, arrowcolor = INK, ax = 0, ay = -30,
      font = list(color = INK, size = 11)
    )))
  }
  p
}

plot_vol_compare <- function(df, cond_vol, window = 21) {
  rv <- rolling_vol(df, window)
  p <- plot_ly() |>
    add_trace(data = rv, x = ~date, y = ~roll_vol, type = "scatter", mode = "lines",
              line = list(color = MUTED, width = 1),
              name = paste0("Realised (", window, "d)"),
              hovertemplate = "%{x|%d %b %Y}<br>%{y:.1%}<extra>Realised</extra>")
  if (!is.null(cond_vol))
    p <- add_trace(p, data = cond_vol, x = ~date, y = ~cond_vol, type = "scatter", mode = "lines",
                   line = list(color = GOLD, width = 1.4), name = "GARCH conditional",
                   hovertemplate = "%{x|%d %b %Y}<br>%{y:.1%}<extra>GARCH</extra>")
  plotly_base_layout(p,
                     legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
                     yaxis = list(title = "Annualised volatility", tickformat = ".0%"),
                     xaxis = list(title = ""))
}

plot_garch_forecast <- function(hist_vol, fcast, tail_days = 250) {
  if (is.null(fcast) || is.null(hist_vol)) return(NULL)
  h <- utils::tail(hist_vol, tail_days)
  last_date <- max(h$date)
  fdates <- last_date + fcast$h
  fc <- tibble::tibble(date = c(last_date, fdates),
                       ann_vol = c(h$cond_vol[nrow(h)], fcast$ann_vol))
  plot_ly() |>
    add_trace(data = h, x = ~date, y = ~cond_vol, type = "scatter", mode = "lines",
              line = list(color = GOLD, width = 1.4), name = "Conditional vol",
              hovertemplate = "%{x|%d %b %Y}<br>%{y:.1%}<extra></extra>") |>
    add_trace(data = fc, x = ~date, y = ~ann_vol, type = "scatter", mode = "lines",
              line = list(color = GOLD_SOFT, width = 1.6, dash = "dash"), name = "GARCH forecast",
              hovertemplate = "%{x|%d %b %Y}<br>%{y:.1%}<extra>Forecast</extra>") |>
    plotly_base_layout(
      legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
      yaxis = list(title = "Annualised volatility", tickformat = ".0%"),
      xaxis = list(title = ""),
      shapes = list(list(type = "rect", xref = "x", x0 = last_date, x1 = max(fdates),
                         yref = "paper", y0 = 0, y1 = 1,
                         fillcolor = "rgba(201,162,39,0.06)", line = list(width = 0),
                         layer = "below")))
}

plot_news_impact <- function(ni_gjr, ni_sgarch = NULL) {
  p <- plot_ly()
  if (!is.null(ni_sgarch))
    p <- add_trace(p, data = ni_sgarch, x = ~z, y = ~sigma2, type = "scatter", mode = "lines",
                   line = list(color = MUTED, width = 1.4, dash = "dot"),
                   name = "sGARCH (symmetric)",
                   hovertemplate = "z=%{x:.2f}<br>\u03c3\u00b2=%{y:.4f}<extra>sGARCH</extra>")
  if (!is.null(ni_gjr))
    p <- add_trace(p, data = ni_gjr, x = ~z, y = ~sigma2, type = "scatter", mode = "lines",
                   line = list(color = GOLD, width = 1.8), name = "gjrGARCH (asymmetric)",
                   hovertemplate = "z=%{x:.2f}<br>\u03c3\u00b2=%{y:.4f}<extra>GJR</extra>")
  plotly_base_layout(p,
                     legend = list(orientation = "h", x = 0, y = 1.1, bgcolor = "rgba(0,0,0,0)"),
                     yaxis = list(title = "Next-day variance"),
                     xaxis = list(title = "Return shock (z)", zeroline = TRUE, zerolinecolor = "#33363D"))
}

COMPARE_COLS <- c("#C9A227", "#5F9E6E", "#C0564B", "#6FA8DC", "#B07CC6", "#E3C565")

plot_cum_returns <- function(df_multi) {
  d <- df_multi |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(rebased = 100 * adjusted / dplyr::first(adjusted)) |>
    dplyr::ungroup()
  plot_ly(d, x = ~date, y = ~rebased, color = ~symbol, colors = COMPARE_COLS,
          type = "scatter", mode = "lines", line = list(width = 1.4),
          hovertemplate = "%{x|%d %b %Y}<br>%{y:.0f}<extra>%{fullData.name}</extra>") |>
    plotly_base_layout(
      legend = list(orientation = "h", x = 0, y = 1.08, bgcolor = "rgba(0,0,0,0)"),
      yaxis = list(title = "Growth of 100 (log)", type = "log"), xaxis = list(title = ""))
}

plot_cor_heatmap <- function(cm) {
  syms <- colnames(cm)
  ann <- list()
  for (i in seq_along(syms)) for (j in seq_along(syms))
    ann[[length(ann) + 1]] <- list(x = syms[j], y = syms[i],
                                   text = sprintf("%.2f", cm[i, j]), showarrow = FALSE,
                                   font = list(color = INK, size = 11))
  plot_ly(x = syms, y = syms, z = cm, type = "heatmap", zmin = -1, zmax = 1,
          colorscale = list(list(0, "#C0564B"), list(0.5, "#1C1E22"), list(1, "#5F9E6E")),
          colorbar = list(title = "\u03c1", tickformat = ".1f"),
          hovertemplate = "%{y} \u2013 %{x}: %{z:.2f}<extra></extra>") |>
    plotly_base_layout(
      xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed"),
      annotations = ann)
}

plot_risk_return <- function(summary) {
  plot_ly(summary, x = ~ann_vol, y = ~ann_return, type = "scatter",
          mode = "markers+text", text = ~symbol, textposition = "top center",
          textfont = list(color = INK, size = 11),
          marker = list(size = 14, color = ~sharpe, colorscale = "Viridis",
                        showscale = TRUE, colorbar = list(title = "Sharpe"),
                        line = list(color = INK, width = 1)),
          hovertemplate = "%{text}<br>Vol %{x:.1%}<br>Return %{y:.1%}<extra></extra>") |>
    plotly_base_layout(
      xaxis = list(title = "Annualised volatility", tickformat = ".0%"),
      yaxis = list(title = "Annualised return",     tickformat = ".0%"))
}