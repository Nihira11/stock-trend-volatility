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
    card(
      card_header("VaR backtest \u2014 Kupiec proportion-of-failures"),
      uiOutput(ns("var_backtest")),
      card_footer(class = "text-muted small",
                  "In-sample check of the parametric (normal) VaR. Historical VaR is calibrated in-sample by construction, so only the parametric model is tested.")
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
    
    ret <- reactive({
      df <- require_prices(prices())
      validate(need(nrow(df) > 30, "Need more history for return statistics."))
      log_returns(df)
    })
    mdd <- reactive(max_drawdown(require_prices(prices())))

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
    
    output$var_backtest <- renderUI({
      bt <- var_backtest(ret())
      rows <- lapply(seq_len(nrow(bt)), function(i) {
        r <- bt[i, ]
        light <- if (r$p_value >= 0.05) c("#5F9E6E", "well calibrated")
        else if (r$p_value >= 0.01) c("#C9A227", "borderline")
        else c("#C0564B", "rejected \u2014 too many breaches")
        tags$div(class = "d-flex align-items-center gap-2 mb-2",
                 tags$span(style = paste0("color:", light[1], ";font-size:1.2rem;"), "\u25CF"),
                 tags$span(HTML(sprintf(
                   "<b>%s VaR</b> \u2014 expected %.0f breaches, observed %d (%.1f\u00d7), Kupiec p = %s \u2014 %s",
                   r$level, r$expected, r$observed, r$ratio, fmt_p(r$p_value), light[2]))))
      })
      tags$div(rows)
    })
  })
}