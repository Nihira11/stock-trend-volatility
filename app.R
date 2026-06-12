source("global.R")
purrr::walk(list.files("modules", full.names = TRUE, pattern = "\\.R$"), source)

ui <- page_navbar(
  title = "Stock Trend & Volatility",
  theme = app_theme,
  
  sidebar = sidebar(
    title = "Controls",
    selectizeInput(
      "ticker", "Ticker",
      choices  = DEFAULT_TICKERS,
      selected = "NVDA",
      options  = list(create = TRUE, placeholder = "Type any ticker…")
    ),
    textInput("ticker_custom", "…or type any ticker",
              placeholder = "e.g. GOOG, WES.AX"),
    actionButton("go", "Load ticker", class = "btn-primary w-100"),
    sliderInput("years", "Lookback (years)",
                min = 1, max = 25, value = DEFAULT_YEARS, step = 1),
    helpText("Data: Yahoo Finance via tidyquant. Cached locally for 24h.")
  ),
  
  nav_panel("Overview",   mod_overview_ui("overview")),
  nav_panel("Trends",     mod_trends_ui("trends")),
  nav_panel("Risk",       div(class = "p-4 text-muted", "TBD")),
  nav_panel("Volatility", div(class = "p-4 text-muted", "TBD")),
  nav_panel("Compare",    div(class = "p-4 text-muted", "TBD"))
)

server <- function(input, output, session) {
  
  # active ticker: dropdown OR custom text + Load
  active_ticker  <- reactiveVal("NVDA")
  session_tickers <- reactiveVal(character(0))
  
  observeEvent(input$ticker, {
    req(nzchar(input$ticker))
    active_ticker(input$ticker)
  })
  
  observeEvent(input$go, {
    req(nzchar(trimws(input$ticker_custom)))
    tk <- toupper(trimws(input$ticker_custom))
    active_ticker(tk)
    session_tickers(unique(c(session_tickers(), tk)))
    updateSelectizeInput(session, "ticker",
                         choices  = unique(c(DEFAULT_TICKERS, session_tickers())),
                         selected = tk)
    updateTextInput(session, "ticker_custom", value = "")
  })
  
  # shared data: every page consumes this
  prices <- reactive({
    req(active_ticker())
    get_prices(
      active_ticker(),
      from = Sys.Date() - lubridate::years(input$years)
    )
  })
  
  # mount page modules
  mod_overview_server("overview", prices = prices,
                      ticker = reactive(active_ticker()))
  
  mod_trends_server("trends", prices = prices,
                    ticker = reactive(active_ticker()))
}

shinyApp(ui, server)