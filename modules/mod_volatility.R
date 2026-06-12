mod_volatility_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      card_header("Realised vs GARCH conditional volatility"),
      withSpinner(plotlyOutput(ns("vol_compare"), height = "360px"), color = GOLD, type = 4)
    ),
    
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Model comparison"),
        tableOutput(ns("model_table")),
        card_footer(
          class = "text-muted small",
          "Lower AIC/BIC = better fit. Persistence = how long vol shocks linger."
        )
      ),
      card(
        card_header("What the model says"),
        uiOutput(ns("interpretation"))
      )
    ),
    
    navset_card_tab(
      nav_panel(
        "Forecast",
        card_header(
          div(
            class = "d-flex flex-column",
            div(class = "fw-semibold mb-1", "Volatility forecast"),
            sliderInput(ns("horizon"), NULL, min = 10, max = 60, value = 30, step = 5, width = "100%")
          )
        ),
        withSpinner(plotlyOutput(ns("forecast"), height = "320px"), color = GOLD, type = 4)
      ),
      nav_panel(
        "Out-of-sample check",
        withSpinner(plotlyOutput(ns("oos_chart"), height = "320px"), color = GOLD, type = 4),
        card_footer(class = "text-muted small", uiOutput(ns("oos_note")))
      ),
      nav_panel(
        "News-impact curve",
        withSpinner(plotlyOutput(ns("news"), height = "320px"), color = GOLD, type = 4),
        card_footer(
          class = "text-muted small",
          "Steeper left arm = negative shocks raise volatility more (leverage)."
        )
      )
    )
  )
}

mod_volatility_server <- function(id, prices, ticker) {
  moduleServer(id, function(input, output, session) {
    
    rets <- reactive({
      df <- require_prices(prices())
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
        `Leverage γ` = ifelse(is.na(s$gamma), "–",
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
    
    archTest <- reactive(arch_lm_test(rets()$ret, lags = 12))
    
    output$interpretation <- renderUI({
      s <- summ(); bm <- best(); bullets <- list()
      
      a <- archTest()
      if (!is.null(a)) {
        cl <- if (a$p_value < 0.05) "present" else "not detected"
        tail_txt <- if (a$p_value >= 0.05) " \u2014 GARCH may add little here."
        else " \u2014 GARCH is warranted."
        bullets <- c(bullets, sprintf(
          "Volatility clustering: <b>%s</b> (ARCH-LM p = %s)%s", cl, fmt_p(a$p_value), tail_txt))
      }
      
      bullets <- c(bullets, sprintf("Best fit by BIC: <b>%s</b> (BIC %.3f).",
                                    bm, s$BIC[s$model == bm]))
      
      # leverage from the model that actually carries it
      lev_rows <- s[!is.na(s$gamma), ]
      lev <- if (bm %in% lev_rows$model) lev_rows[lev_rows$model == bm, ]
      else if (nrow(lev_rows) > 0) lev_rows[which.min(lev_rows$gamma_pval), ]
      else NULL
      if (!is.null(lev) && nrow(lev) == 1) {
        sig <- if (!is.na(lev$gamma_pval) && lev$gamma_pval < 0.05) "significant" else "not significant"
        bullets <- c(bullets, sprintf(
          "Leverage (%s): \u03b3 = %.3f (p = %s, %s) \u2014 negative shocks raise next-day variance more than equal positive ones.",
          lev$model, lev$gamma, fmt_p(lev$gamma_pval), sig))
      }
      
      pr <- s$persistence[s$model == bm]
      if (length(pr) == 1 && !is.na(pr) && pr < 1)
        bullets <- c(bullets, sprintf(
          "Persistence = %.3f \u2192 vol shocks decay with a half-life of ~%.0f trading days.",
          pr, log(0.5) / log(pr)))
      
      tags$ul(lapply(bullets, function(b) tags$li(HTML(b))))
    })
    
    oos <- reactive({
      rr <- rets()
      validate(need(length(rr$ret) >= 600, "Need \u2265 ~3 years of history for an out-of-sample test."))
      withProgress(message = "Backtesting the forecast\u2026", value = 0.5,
                   garch_oos_eval(rr$ret, rr$dates, model = best()))
    })
    output$oos_chart <- renderPlotly({
      e <- oos(); validate(need(!is.null(e), "Out-of-sample fit didn't converge."))
      plot_oos_vol(e$series)
    })
    output$oos_note <- renderUI({
      e <- oos(); req(!is.null(e))
      verdict <- if (e$skill > 0)
        sprintf("beats a constant-vol benchmark by %.0f%% (RMSE)", 100 * e$skill)
      else sprintf("does <b>not</b> beat a constant-vol benchmark (%.0f%%)", 100 * e$skill)
      HTML(sprintf("Trained on %d days, tested on %d held-out days. The GARCH forecast %s.",
                   e$n_train, e$n_test, verdict))
    })
  })
}