mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(div(class = "d-flex flex-column",
                      div(class = "fw-semibold mb-1", "Compare tickers (2\u20136)"),
                      selectizeInput(ns("syms"), NULL, choices = NULL, multiple = TRUE, width = "100%",
                                     options = list(create = TRUE, maxItems = 6, placeholder = "Add tickers\u2026")))),
      withSpinner(plotlyOutput(ns("cum"), height = "380px"), color = GOLD, type = 4)
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Risk vs return"),
           withSpinner(plotlyOutput(ns("scatter"), height = "360px"), color = GOLD, type = 4),
           card_footer(class = "text-muted small", "Point colour = Sharpe ratio.")),
      card(full_screen = TRUE,
           card_header("Return correlation"),
           withSpinner(plotlyOutput(ns("heatmap"), height = "360px"), color = GOLD, type = 4))
    ),
    card(card_header("Summary"), tableOutput(ns("summary_tbl")))
  )
}

mod_compare_server <- function(id, years) {
  moduleServer(id, function(input, output, session) {
    
    updateSelectizeInput(session, "syms", choices = DEFAULT_TICKERS,
                         selected = c("NVDA", "MSFT", "KO", "^GSPC"), server = TRUE)
    
    data_multi <- reactive({
      syms <- input$syms
      validate(need(length(syms) >= 2, "Pick at least 2 tickers to compare."))
      syms <- utils::head(unique(syms), 6)
      withProgress(message = "Loading tickers\u2026", value = 0.5, {
        get_prices_multi(syms, from = Sys.Date() - lubridate::years(years()))
      })
    })
    
    summ <- reactive(summary_table(data_multi()))
    
    output$cum <- renderPlotly({
      validate(need(nrow(data_multi()) > 0, "No data."))
      plot_cum_returns(data_multi())
    })
    output$scatter <- renderPlotly(plot_risk_return(summ()))
    output$heatmap <- renderPlotly({
      cm <- cor_matrix(data_multi())
      validate(need(ncol(cm) >= 2, "Need \u2265 2 valid tickers."))
      plot_cor_heatmap(cm)
    })
    output$summary_tbl <- renderTable({
      s <- summ()
      data.frame(
        Ticker            = s$symbol,
        `Ann. return`     = scales::percent(s$ann_return, accuracy = 0.1),
        `Ann. volatility` = scales::percent(s$ann_vol,    accuracy = 0.1),
        Sharpe            = sprintf("%.2f", s$sharpe),
        `Max drawdown`    = scales::percent(s$max_dd,     accuracy = 0.1),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
  })
}