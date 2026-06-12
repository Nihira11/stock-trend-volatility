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