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
      xaxis = list(rangeslider = list(visible = FALSE))
    )
}
