source("global.R")

ui <- page_sidebar(
  title  = "Stock Trend & Volatility - build check",
  theme  = app_theme,

  sidebar = sidebar(
    title = "Controls",
    selectizeInput(
      "ticker", "Ticker",
      choices  = DEFAULT_TICKERS,
      selected = "NVDA",
      options  = list(create = TRUE,  # allow typing any ticker
                      placeholder = "Type any ticker…")
    ),
    textInput("ticker_custom", "…or type any ticker",
              placeholder = "e.g. GOOG, WES.AX"),
    actionButton("go", "Load ticker", class = "btn-primary w-100"),
    sliderInput(
      "years", "Lookback (years)",
      min = 1, max = 25, value = DEFAULT_YEARS, step = 1
    ),
    helpText("Data: Yahoo Finance via tidyquant. ",
             "Cached locally for 24h.")
  ),

  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "Last close",      value = textOutput("vb_price"),
              theme = value_box_theme(bg = SURFACE, fg = INK)),
    value_box(title = "Period return",   value = textOutput("vb_return"),
              theme = value_box_theme(bg = SURFACE, fg = INK)),
    value_box(title = "Observations",    value = textOutput("vb_n"),
              theme = value_box_theme(bg = SURFACE, fg = INK))
  ),
  
  textOutput("data_range_note"),

  card(
    full_screen = TRUE,
    card_header(textOutput("chart_title", inline = TRUE)),
    withSpinner(plotlyOutput("price_plot", height = "420px"),
                color = GOLD, type = 4)
  )
)

server <- function(input, output, session) {

  active_ticker <- reactiveVal("NVDA")
  
  observeEvent(input$ticker, {
    active_ticker(input$ticker)
  })
  
  session_tickers <- reactiveVal(character(0))
  
  observeEvent(input$go, {
    req(nzchar(trimws(input$ticker_custom)))
    tk <- toupper(trimws(input$ticker_custom))
    active_ticker(tk)
    
    session_tickers(unique(c(session_tickers(), tk)))   # remember it
    
    updateSelectizeInput(session, "ticker",
                         choices  = unique(c(DEFAULT_TICKERS, session_tickers())),
                         selected = tk)
    updateTextInput(session, "ticker_custom", value = "")
  })
  
  prices <- reactive({
    req(active_ticker())
    get_prices(
      active_ticker(),
      from = Sys.Date() - lubridate::years(input$years)
    )
  })

  output$vb_price <- renderText({
    df <- prices()
    validate(need(!is.null(df), "-"))
    scales::dollar(dplyr::last(df$adjusted), accuracy = 0.01)
  })

  output$vb_return <- renderText({
    df <- prices()
    validate(need(!is.null(df), "-"))
    scales::percent(dplyr::last(df$cum_return) - 1, accuracy = 0.1)
  })

  output$vb_n <- renderText({
    df <- prices()
    validate(need(!is.null(df), "-"))
    format(nrow(df), big.mark = ",")
  })
  
  output$data_range_note <- renderText({
    df <- prices()
    validate(need(!is.null(df), ""))
    paste0("Showing ", format(min(df$date), "%d %b %Y"),
           " – ", format(max(df$date), "%d %b %Y"),
           "  (", format(nrow(df), big.mark = ","), " trading days)")
  })

  output$price_plot <- renderPlotly({
    df <- prices()
    validate(need(
      !is.null(df),
      paste0("No data for '", active_ticker(),
             "'. Check the ticker symbol (e.g. NVDA, CBA.AX, ^GSPC).")
    ))

    plot_ly(df, x = ~date, y = ~adjusted,
            type = "scatter", mode = "lines",
            line = list(color = GOLD, width = 1.6),
            hovertemplate = "%{x|%d %b %Y}<br>$%{y:.2f}<extra></extra>") |>
      plotly_base_layout(
        showlegend = FALSE,
        yaxis = list(title = "Adjusted close (USD/AUD)",
                     gridcolor = "#33363D"),
        xaxis = list(title = "", gridcolor = "#33363D")
      )
  })
  
  output$chart_title <- renderText({
    paste0(active_ticker(), "  <- <-  adjusted close")
  })
}

shinyApp(ui, server)
