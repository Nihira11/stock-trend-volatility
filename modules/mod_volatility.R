mod_volatility_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(full_screen = TRUE,
         card_header("Realised vs GARCH conditional volatility"),
         withSpinner(plotlyOutput(ns("vol_compare"), height = "380px"), color = GOLD, type = 4)
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Model comparison"),
           tableOutput(ns("model_table")),
           card_footer(class = "text-muted small",
                       "Lower AIC/BIC = better fit. Persistence = how long vol shocks linger.")),
      card(full_screen = TRUE,
           card_header("News-impact curve"),
           withSpinner(plotlyOutput(ns("news"), height = "300px"), color = GOLD, type = 4),
           card_footer(class = "text-muted small",
                       "Steeper left arm = negative shocks raise volatility more (leverage)."))
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE,
           card_header(div(class = "d-flex flex-column",
                           div(class = "fw-semibold mb-1", "Volatility forecast"),
                           sliderInput(ns("horizon"), NULL, min = 10, max = 60, value = 30, step = 5, width = "100%"))),
           withSpinner(plotlyOutput(ns("forecast"), height = "320px"), color = GOLD, type = 4)),
      card(card_header("What the model says"),
           uiOutput(ns("interpretation")))
    )
  )
}

mod_volatility_server <- function(id, prices, ticker) {
  moduleServer(id, function(input, output, session) {
    
    rets <- reactive({
      df <- prices(); req(!is.null(df), nrow(df) > 1)
      list(ret = log_returns(df), dates = df$date[-1], df = df)
    })
    
    fits <- reactive({
      r <- rets()$ret
      validate(need(length(stats::na.omit(r)) >= 500,
                    "Not enough history to fit GARCH \u2014 extend the lookback to \u2265 ~2 years."))
      withProgress(message = "Fitting GARCH models\u2026", value = 0.5, {
        list(sGARCH   = fit_garch_m(r, "sGARCH"),
             eGARCH   = fit_garch_m(r, "eGARCH"),
             gjrGARCH = fit_garch_m(r, "gjrGARCH"))
      })
    })
    
    summ <- reactive({ s <- compare_garch(fits()); validate(need(!is.null(s), "No models converged.")); s })
    best <- reactive({ s <- summ(); s$model[which.min(s$BIC)] })
    
    output$vol_compare <- renderPlotly({
      rr <- rets(); cv <- conditional_vol(fits()[[best()]], rr$dates)
      plot_vol_compare(rr$df, cv)
    })
    
    output$model_table <- renderTable({
      s <- summ()
      data.frame(
        Model        = s$model,
        AIC          = sprintf("%.3f", s$AIC),
        BIC          = sprintf("%.3f", s$BIC),
        Persistence  = sprintf("%.3f", s$persistence),
        `Leverage γ` = ifelse(is.na(s$gamma), "—",
                              sprintf("%.3f (p=%.3f)", s$gamma, s$gamma_pval)),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    output$news <- renderPlotly({
      f <- fits(); plot_news_impact(news_impact(f$gjrGARCH), news_impact(f$sGARCH))
    })
    
    output$forecast <- renderPlotly({
      rr <- rets(); f <- fits()
      cv <- conditional_vol(f[[best()]], rr$dates)
      fc <- garch_forecast(f[[best()]], n = input$horizon)
      validate(need(!is.null(fc), "Forecast unavailable."))
      plot_garch_forecast(cv, fc)
    })
    
    output$interpretation <- renderUI({
      s <- summ(); bm <- best()
      bestrow <- s[s$model == bm, ]; gjr <- s[s$model == "gjrGARCH", ]
      bullets <- list(paste0("Best fit by BIC: <b>", bm, "</b> (BIC ",
                             sprintf("%.3f", bestrow$BIC), ")."))
      if (nrow(gjr) == 1 && !is.na(gjr$gamma)) {
        sig <- if (!is.na(gjr$gamma_pval) && gjr$gamma_pval < 0.05) "significant" else "not significant"
        bullets <- c(bullets, paste0(
          "Leverage γ = ", sprintf("%.3f", gjr$gamma), " (p = ",
          sprintf("%.3f", gjr$gamma_pval), ", ", sig,
          "): negative shocks raise next-day variance more than equal positive ones."))
      }
      p <- bestrow$persistence
      if (!is.na(p) && p < 1) bullets <- c(bullets, paste0(
        "Persistence = ", sprintf("%.3f", p),
        " \u2192 vol shocks decay with a half-life of ~",
        sprintf("%.0f", log(0.5) / log(p)), " trading days."))
      tags$ul(lapply(bullets, function(b) tags$li(HTML(b))))
    })
  })
}