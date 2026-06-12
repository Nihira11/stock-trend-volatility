# ============================================================
# modules/mod_overview.R — Page 1: Overview
#
# Receives a reactive `prices` (already filtered + log returns)
# and a reactive `ticker` (the active symbol) from app.R.
# Owns only page-local controls (chart style, MA toggles).
# ============================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Last close",
                value = textOutput(ns("vb_price")),
                theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box(title = "Period return",
                value = textOutput(ns("vb_return")),
                theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box(title = "52-week high",
                value = textOutput(ns("vb_high")),
                theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box(title = "52-week low",
                value = textOutput(ns("vb_low")),
                theme = value_box_theme(bg = SURFACE, fg = INK))
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "d-flex flex-column",
          div(class = "fw-semibold mb-1",
              textOutput(ns("chart_title"), inline = TRUE)),
          div(class = "d-flex flex-wrap gap-3 align-items-center",
              checkboxGroupInput(
                ns("mas"), NULL, inline = TRUE,
                choices  = c("SMA 20" = "20", "SMA 50" = "50", "SMA 200" = "200"),
                selected = c("20", "50", "200")
              ),
              radioButtons(
                ns("style"), NULL, inline = TRUE,
                choices = c("Line" = "line", "Candles" = "candlestick"),
                selected = "line"
              ))
        )
      ),
      withSpinner(plotlyOutput(ns("chart"), height = "520px"),
                  color = GOLD, type = 4),
      card_footer(class = "text-muted small",
                  textOutput(ns("range_note"), inline = TRUE))
    )
  )
}

mod_overview_server <- function(id, prices, ticker) {
  moduleServer(id, function(input, output, session) {
    
    # prices + the MAs the user has toggled on
    enriched <- reactive({
      df <- prices()
      req(!is.null(df))
      add_moving_averages(df, windows = as.integer(input$mas))
    })
    
    output$vb_price <- renderText({
      df <- prices(); validate(need(!is.null(df), "—"))
      scales::dollar(dplyr::last(df$adjusted), accuracy = 0.01)
    })
    
    output$vb_return <- renderText({
      df <- prices(); validate(need(!is.null(df), "—"))
      scales::percent(dplyr::last(df$cum_return) - 1, accuracy = 0.1, big.mark = ",")
    })
    
    output$vb_high <- renderText({
      df <- prices(); validate(need(!is.null(df), "—"))
      scales::dollar(stats_52w(df)$high, accuracy = 0.01)
    })
    
    output$vb_low <- renderText({
      df <- prices(); validate(need(!is.null(df), "—"))
      scales::dollar(stats_52w(df)$low, accuracy = 0.01)
    })
    
    output$chart_title <- renderText({
      paste0(ticker(), " — price & moving averages")
    })
    
    output$range_note <- renderText({
      df <- prices(); validate(need(!is.null(df), ""))
      paste0("Showing ", format(min(df$date), "%d %b %Y"),
             " – ", format(max(df$date), "%d %b %Y"),
             " (", format(nrow(df), big.mark = ","), " trading days). ",
             "Source: Yahoo Finance, adjusted close.")
    })
    
    output$chart <- renderPlotly({
      df <- enriched()
      
      validate(need(
        !is.null(df) && nrow(df) > 0,
        paste0("No data for '", ticker(),
               "'. Check the ticker symbol (e.g. NVDA, CBA.AX, ^GSPC).")
      ))
      
      plot_price_volume(
        df,
        ticker = ticker(),
        style = input$style
      )
    })
  })
}
