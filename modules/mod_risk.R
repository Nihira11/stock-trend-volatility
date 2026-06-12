mod_risk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Annualised return",     textOutput(ns("vb_ret")),    theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box("Annualised volatility", textOutput(ns("vb_vol")),    theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box("Sharpe ratio",          textOutput(ns("vb_sharpe")), theme = value_box_theme(bg = SURFACE, fg = INK)),
      value_box("Max drawdown",          textOutput(ns("vb_mdd")),    theme = value_box_theme(bg = SURFACE, fg = INK))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Distribution of daily returns"),
           withSpinner(plotlyOutput(ns("hist"), height = "380px"), color = GOLD, type = 4),
           card_footer(class = "text-muted small", "Sharpe assumes a 4% annual risk-free rate.")
      ),
      card(
        card_header("Value at Risk & Expected Shortfall"),
        tableOutput(ns("var_table")),
        card_footer(class = "text-muted small",
                    "1-day VaR: the loss exceeded only (1 \u2212 confidence) of days.")
      )
    ),
    card(full_screen = TRUE,
         card_header("Drawdown from peak"),
         withSpinner(plotlyOutput(ns("dd"), height = "320px"), color = GOLD, type = 4),
         card_footer(class = "text-muted small", textOutput(ns("dd_note"), inline = TRUE))
    )
  )
}

mod_risk_server <- function(id, prices, ticker) {
  moduleServer(id, function(input, output, session) {
    
    ret <- reactive({ df <- prices(); req(!is.null(df), nrow(df) > 30); log_returns(df) })
    mdd <- reactive({ df <- prices(); req(!is.null(df), nrow(df) > 1);  max_drawdown(df) })
    
    output$vb_ret    <- renderText(scales::percent(ann_return(ret()),     accuracy = 0.1))
    output$vb_vol    <- renderText(scales::percent(ann_volatility(ret()), accuracy = 0.1))
    output$vb_sharpe <- renderText(sprintf("%.2f", sharpe_ratio(ret())))
    output$vb_mdd    <- renderText(scales::percent(mdd()$max, accuracy = 0.1))
    
    output$hist <- renderPlotly({
      validate(need(length(ret()) > 30, "Not enough data to plot a distribution."))
      plot_return_hist(prices())
    })
    
    output$var_table <- renderTable({
      r  <- ret()
      vh <- var_historical(r); vp <- var_parametric(r)
      es <- cvar_historical(r, 0.95)
      data.frame(
        Metric     = c("95% VaR", "99% VaR", "95% CVaR (ES)"),
        Historical = scales::percent(c(vh[["95%"]], vh[["99%"]], es), accuracy = 0.1),
        Parametric = c(scales::percent(c(vp[["95%"]], vp[["99%"]]), accuracy = 0.1), "\u2014"),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    output$dd <- renderPlotly({
      m <- mdd(); validate(need(nrow(m$curve) > 0, "No data."))
      plot_drawdown(m$curve, m)
    })
    
    output$dd_note <- renderText({
      m <- mdd(); if (is.na(m$trough_date)) return("")
      rec <- if (is.na(m$recovery_date)) "not yet recovered"
      else paste0("recovered ", format(m$recovery_date, "%d %b %Y"))
      paste0("Worst drawdown ", scales::percent(m$max, accuracy = 0.1),
             ": peak ", format(m$peak_date, "%d %b %Y"),
             " \u2192 trough ", format(m$trough_date, "%d %b %Y"), " (", rec, ").")
    })
  })
}