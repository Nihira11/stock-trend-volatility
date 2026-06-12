# ============================================================
# Receives reactive `prices` and `ticker` from app.R.
# Owns page-local indicator settings (RSI window, Bollinger n/sd).
# ============================================================
mod_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 3, 4),
      value_box(title = "Trend", value = uiOutput(ns("trend_pill")),
                theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box(title = "Last RSI", value = textOutput(ns("vb_rsi")),
                theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box(title = "Days since last cross", value = textOutput(ns("vb_cross")),
                theme = value_box_theme(bg = SURFACE, fg = INK))
    ),
    layout_sidebar(
      sidebar = sidebar(
        title = "Indicator settings", position = "right", open = FALSE,
        sliderInput(ns("rsi_n"), "RSI window",       min = 5,  max = 30, value = 14, step = 1),
        sliderInput(ns("bb_n"),  "Bollinger window", min = 10, max = 40, value = 20, step = 1),
        sliderInput(ns("bb_sd"), "Bollinger \u03c3", min = 1,  max = 3,  value = 2,  step = 0.5)
      ),
      navset_card_tab(
        nav_panel("Crossovers",
                  withSpinner(plotlyOutput(ns("p_cross"), height = "460px"), color = GOLD, type = 4)),
        nav_panel("Bollinger",
                  withSpinner(plotlyOutput(ns("p_boll"),  height = "460px"), color = GOLD, type = 4)),
        nav_panel("RSI",
                  withSpinner(plotlyOutput(ns("p_rsi"),   height = "460px"), color = GOLD, type = 4)),
        nav_panel("MACD",
                  withSpinner(plotlyOutput(ns("p_macd"),  height = "460px"), color = GOLD, type = 4))
      )
    )
  )
}

mod_trends_server <- function(id, prices, ticker) {
  moduleServer(id, function(input, output, session) {
    
    enriched <- reactive({
      df <- require_prices(prices())
      df |>
        add_moving_averages(windows = c(20, 50, 200)) |>
        add_rsi(n = input$rsi_n) |> add_macd() |> add_bollinger(n = input$bb_n, sd = input$bb_sd)
    })
    
    crosses <- reactive(detect_crossovers(enriched()))
    
    output$trend_pill <- renderUI({
      df <- enriched(); validate(need(nrow(df) > 0, "\u2014"))
      tr  <- classify_trend(df)
      cls <- switch(tr$label,
                    "Uptrend"   = "bg-success",
                    "Downtrend" = "bg-danger",
                    "bg-secondary")
      span(class = paste("badge rounded-pill px-3 py-2 fs-6", cls), toupper(tr$label))
    })
    
    output$vb_rsi <- renderText({
      df <- enriched(); validate(need("rsi" %in% names(df), "\u2014"))
      v <- dplyr::last(na.omit(df$rsi))
      if (length(v) == 0) "\u2014" else sprintf("%.1f", v)
    })
    
    output$vb_cross <- renderText({
      cx <- crosses(); df <- enriched()
      validate(need(nrow(cx) > 0, "none in window"))
      paste0(format(as.integer(max(df$date) - max(cx$date)), big.mark = ","), " days")
    })
    
    output$p_cross <- renderPlotly({
      df <- enriched(); validate(need(nrow(df) > 0, "No data."))
      plot_price_crossovers(df, crosses(), ticker())
    })
    output$p_boll <- renderPlotly({
      df <- enriched()
      validate(need(any(!is.na(df$bb_upper)), "Not enough data for the Bollinger window."))
      plot_price_bollinger(df, ticker())
    })
    output$p_rsi <- renderPlotly({
      df <- enriched()
      validate(need(any(!is.na(df$rsi)), "Not enough data for the RSI window."))
      plot_rsi(df)
    })
    output$p_macd <- renderPlotly({
      df <- enriched()
      validate(need(any(!is.na(df$macd)), "Not enough data for MACD."))
      plot_macd(df)
    })
  })
}