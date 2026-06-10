# Stock Trend Analysis + Volatility Insights

Interactive R/Shiny dashboard for equity trend, risk, and volatility analysis.
Live data from Yahoo Finance via `tidyquant`; GARCH-family volatility modelling
via `rugarch`. *(Full README later)*

## Run locally
```r
install.packages(c("shiny","bslib","tidyquant","tidyverse","plotly",
                   "TTR","PerformanceAnalytics","rugarch",
                   "shinycssloaders","memoise","testthat","rsconnect"))
shiny::runApp()
```

## Test the data layer
```r
testthat::test_file("tests/test_data_fetch.R")
```