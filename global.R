suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyquant)   # tq_get() - Yahoo Finance
  library(tidyverse)
  library(plotly)
  library(shinycssloaders)
  library(zoo)        # rolling windows
  library(rugarch)    # GARCH family
  library(memoise)    # cache slow fits
})

# constants
DEFAULT_TICKERS <- c(
  "NVDA", "TSLA",          # high volatility
  "AAPL", "MSFT",          # large-cap moderate
  "JPM",                   # financials
  "KO",                    # low-vol defensive contrast
  "^GSPC",                 # S&P 500 benchmark
  "CBA.AX", "BHP.AX"       # ASX
)

DEFAULT_YEARS       <- 10        # default lookback window
TRADING_DAYS        <- 252       # annualisation factor
CACHE_DIR           <- "cache"
CACHE_MAX_AGE_HOURS <- 24        # refetch if cache older than this

# theme: charcoal & gold
GOLD     <- "#C9A227"
GOLD_SOFT<- "#E3C565"
CHARCOAL <- "#1C1E22"
SURFACE  <- "#26292F"
INK      <- "#E8E6E3"
MUTED    <- "#9A9890"
LOSS_RED <- "#C0564B"
GAIN_GRN <- "#5F9E6E"

app_theme <- bs_theme(
  version    = 5,
  bg         = CHARCOAL,
  fg         = INK,
  primary    = GOLD,
  secondary  = SURFACE,
  success    = GAIN_GRN,
  danger     = LOSS_RED,
  base_font    = font_google("Inter"),
  heading_font = font_google("Fraunces"),
  code_font    = font_google("JetBrains Mono")
)

# shared plotly layout so every chart matches the theme
plotly_base_layout <- function(p, ...) {
  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = INK, family = "Inter"),
    xaxis = list(gridcolor = "#33363D", zerolinecolor = "#33363D"),
    yaxis = list(gridcolor = "#33363D", zerolinecolor = "#33363D"),
    ...
  )
}

# source all pure functions
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
