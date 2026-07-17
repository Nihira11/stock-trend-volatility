
# Stock Trend Analysis & Volatility Insights

An interactive R/Shiny dashboard for analysing stock prices, trends, risk and volatility.

The app downloads daily market data from Yahoo Finance, calculates common technical indicators and risk measures and uses GARCH models to study and forecast volatility. It can also measure the leverage effect, where negative price movements often increase future volatility more than positive movements of the same size.

[![Live app](https://img.shields.io/badge/live%20app-shinyapps.io-C9A227)](https://nihirasharma.shinyapps.io/stock-trend-volatility/)
&nbsp;
![R](https://img.shields.io/badge/R-Shiny-276DC3)
&nbsp;
![Models](https://img.shields.io/badge/volatility-rugarch%20GARCH-555)

> **Live demo:** https://nihirasharma.shinyapps.io/stock-trend-volatility/ &nbsp;·&nbsp;
*The app uses a free hosting plan, so the first load may take around 30 seconds.*

---

## What the app does

Enter any supported ticker, such as:
- US stock: `NVDA` or `MSFT`
- Market index: `^GSPC`
- Australian stock: `CBA.AX`

Then choose a time period. The app downloads the data and analyses it across five pages.

All calculations use the **adjusted closing price**, which accounts for stock splits and dividends. Risk calculations use **daily log returns**, which are commonly used in financial and GARCH models.

### Overview
The Overview page provides a summary of the selected stock.

It includes:
- Price chart with 20-day, 50-day and 200-day moving averages
- Line chart or candlestick chart
- Daily trading volume
- Latest closing price
- Total return over the selected period
- 52-week high and low

The default `NVDA` view covers ten years and around 2,500 trading days.

![Overview page](screenshots/overview.png)

### Trends
The Trends page studies the direction and strength of the stock’s price movement.

It includes:
- Uptrend, downtrend or sideways trend label
- Current RSI value
- Days since the latest moving-average crossover
- Golden cross and death cross signals
- Bollinger Bands
- RSI chart
- MACD chart

The trend label is based on:
- Current price compared with the 200-day moving average
- Relationship between the 50-day and 200-day moving averages

Golden crosses are marked with gold triangles, while death crosses are marked with red triangles

![Trends page](screenshots/trends.png)

### Risk
The Risk page measures the return and downside risk of the selected stock.

Main results include:
- Annualised return
- Annualised volatility
- Sharpe ratio
- Maximum drawdown
- Historical Value at Risk
- Parametric Value at Risk
- Conditional Value at Risk

A return distribution chart compares the stock’s actual returns with a fitted normal distribution. This makes it easier to see whether the stock has more extreme returns than a normal model expects.

The chart also marks the 95% and 99% Value at Risk limits.

#### Value at Risk Backtesting

The app checks whether the VaR model worked correctly using:
- Number of actual VaR breaches
- Expected number of breaches
- Kupiec proportion-of-failures test

For example, a 99% VaR model should be breached on approximately 1% of trading days.

Traffic-light labels show whether the model:
- Estimates risk reasonably
- Underestimates risk
- Overestimates risk

#### Drawdown Analysis

The drawdown chart shows periods when the stock remained below its previous highest price.

It identifies:
- Previous price peak
- Lowest point
- Maximum loss
- Recovery date

![Risk page](screenshots/risk.png)

### Volatility 

Before fitting any GARCH model, an ARCH-LM test checks whether volatility clustering is present and therefore whether GARCH modelling is warranted.

The page also evaluates predictive performance out-of-sample using an 80/20 train-test split: models are fit on the training window and forecast volatility into the held-out period, where forecast accuracy is compared against realised volatility and a constant-volatility benchmark.

The Volatility page is the main modelling section of the dashboard. It compares actual market volatility with volatility estimated by GARCH models.

The page includes:
- Rolling realised volatility
- GARCH conditional volatility
- sGARCH, eGARCH, and gjrGARCH model comparison
- AIC and BIC scores
- Volatility persistence
- Leverage effect
- News-impact curve
- Volatility forecasts from 10 to 60 days
- Simple explanations of the winning model

#### ARCH-LM Test
Before fitting a GARCH model, the app runs an ARCH-LM test. This checks whether volatility clustering is present. Volatility clustering means that calm periods and highly volatile periods tend to appear in groups. If this pattern is not present, using a GARCH model may not be useful.

#### Out-of-Sample Testing
The GARCH models are also tested on data that was not used for training. 

The data is divided into:
- 80% training data
- 20% test data

The models are trained on the first 80% and then used to predict volatility during the remaining 20%

The forecasts are compared with:
- Actual realised volatility
- A simple constant-volatility model

This shows whether the models can predict future volatility rather than only explain past movements

![Volatility page](screenshots/volatility.png)

### Compare
The Compare page allows users to analyse between two and six assets together.

It includes:
- Cumulative returns starting from 100
- Risk-versus-return chart
- Sharpe ratio comparison
- Return correlation heatmap
- Summary table

Trading dates are matched before calculating correlations. This means Australian and US stocks are only compared on dates when both markets have data.

![Compare page](screenshots/compare.png)

---

## Findings

The following findings use the default ticker group over a ten-year period ending on 11 June 2026. They are examples of what the dashboard can identify and are not investment advice.

- **A risk model is only useful if it survives backtesting.** The VaR backtest compares observed exceedances against the model's expected breach rate using the Kupiec proportion-of-failures test. Several equities showed materially more 99% VaR breaches than predicted by the normal distribution, reinforcing the conclusion that equity returns are fat-tailed and that parametric normal VaR systematically understates extreme downside risk.

- **The normal distribution lies about tail risk – in both directions.** For KO, the historical 95% 1-day VaR (1.6%) is *lower* than the parametric normal estimate (1.9%), but the historical 99% VaR (3.2%) is *higher* than parametric (2.6%). The normal fit overstates ordinary down days and understates the rare crisis day by roughly a quarter – the textbook fat-tail signature, visible directly on the Risk histogram.

- **Leverage is real and statistically significant for a bank.** On JPM, both asymmetric models reject symmetry: eGARCH γ = 0.166 and gjrGARCH γ = 0.152, each with p < 0.001. Negative shocks raise next-day variance materially more than equal-sized positive ones – exactly the dynamic a symmetric sGARCH model misses.

- **Volatility shocks have wildly different memory across names.** JPM's GARCH persistence of 0.970 implies a vol-shock half-life of ~23 trading days. HIMS sits at 0.998 – a near-unit-root process whose shocks decay with a half-life of ~279 days, so turbulence lingers for more than a year.

- **Raw return is a trap without risk adjustment.** In the compare set, MSFT posts the best Sharpe (0.66) on 27% annual volatility. HIMS earns a comparable headline return (16.1%) but its Sharpe collapses to 0.16, because 77% annualised volatility and an -87% drawdown swamp the return. The risk-vs-return scatter makes the trade-off legible at a glance.

- **Drawdowns recover on geological timescales.** HIMS fell −87.3% from its Feb-2021 peak to a May-2022 trough and did not reclaim that high until Jun 2024 – over three years underwater. KO's worst drawdown (−37.0%) was the Feb–Mar 2020 COVID crash, fully recovered by Jul 2021.

---

## Methodology

| Method | Purpose |
| --- | --- |
| **Adjusted close** | Accounts for stock splits and dividends so returns show the full change in value |
| **Log returns** | Makes daily returns easier to combine across time and matches standard GARCH methods |
| **√252 annualisation** | Converts daily volatility into annual volatility using approximately 252 trading days |
| **Student-t innovations** | Better handles extreme stock returns than a normal distribution |
| **Historical VaR** | Calculates risk using actual past returns without assuming a specific distribution |
| **Parametric VaR** | Calculates risk using an assumed normal distribution |
| **CVaR / Expected Shortfall** | Measures the average loss after the VaR limit has already been exceeded |
| **sGARCH** | Provides a basic model where positive and negative shocks have the same effect |
| **eGARCH and gjrGARCH** | Allow negative shocks to affect volatility differently from positive shocks |
| **Kupiec test** | Checks whether VaR is breached as often as the model predicts |
| **ARCH-LM Test** | Checks whether volatility clustering exists before using GARCH |
| **Out-of-Sample Validation** | Measures model performance using data that was not included during training |

All risk and volatility calculations are stored as reusable functions inside the `R/` folder.
The functions are tested using expected values calculated by hand.

---

## Architecture

```
stock-trend-volatility/
│
├── app.R                              # Controls the selected ticker, time period, shared price data, and pages
├── global.R                           # Loads packages, settings, themes, and functions
├── stock-trend-volatility.Rproj
├── .gitignore
├── .rscignore
├── .lintr
├── README.md
│
├── R/
│   ├── data_fetch.R                   # Downloads Yahoo Finance data and manages cached files
│   ├── indicators.R                   # Moving averages, RSI, MACD, Bollinger Bands, and crossovers
│   ├── plots.R                        # Creates Plotly charts and shared chart designs
│   ├── risk_metrics.R                 # Returns, Sharpe ratio, drawdowns, VaR, CVaR, and correlations
│   ├── utils.R                        # Formatting, validation, and shared helper functions
│   └── volatility.R                   # ARCH tests, GARCH models, forecasts, and validation
│
├── modules/
│   ├── mod_compare.R                  # Multi-ticker comparison page
│   ├── mod_overview.R                 # Price, volume, stats overview page
│   ├── mod_risk.R                     # Risk metrics, VaR, backtesting, drawdown page
│   ├── mod_trends.R                   # RSI, MACD, Bollinger, crossover page
│   └── mod_volatility.R               # GARCH modelling, forecast, validation page
│
├── tests/
│   ├── test_compare.R
│   ├── test_data_fetch.R
│   ├── test_indicators.R
│   ├── test_risk_metrics.R
│   └── test_volatility.R
│
├── screenshots/
│   ├── overview.png
│   ├── trends.png
│   ├── risk.png
│   ├── volatility.png
│   └── compare.png
│
└── www/
    └── custom.css                     # App styling
```

### Application Design

The project follows three main design rules.

#### 1. Calculations Are Separate From the Dashboard
All financial calculations are stored inside the `R/` folder. The functions receive data and return results without depending directly on Shiny. This makes them easier to test, reuse and understand.

#### 2. Each Page Has Its Own Module
Each dashboard page is stored in a separate module. The main `app.R` file only manages information shared across the app, such as the selected ticker and downloaded prices. This keeps the main file short and organised.

#### 3. Data and Models Are Cached
The app uses two levels of caching:

- Full price history is downloaded once and stored locally
- GARCH model results are saved so they do not need to be calculated again

Changing the time period filters the saved price data instead of downloading it again. Returning to a previously analysed ticker also loads the saved GARCH result more quickly.

---

## Run locally

```r
# Install the required R packages:
install.packages(c(
  "shiny", "bslib", "tidyquant", "tidyverse", "plotly",
  "TTR", "PerformanceAnalytics", "rugarch", "zoo",
  "shinycssloaders", "memoise", "testthat", "rsconnect"
))

# Start the application:
shiny::runApp()
```
The first launch downloads and saves the required price history, so it may take a little longer. Later loads use the locally saved data.

## Tests

```r
testthat::test_dir("tests")
```

The tests check:
- Moving-average crossover detection
- RSI remains between 0 and 100
- Drawdown peak, lowest point, and recovery calculations
- 95% VaR calculations
- CVaR is greater than VaR
- Rolling-volatility calculations
- Log-return calculations
- Data downloading and comparison functions
- GARCH and volatility functions

---

## Stack

R · Shiny · bslib · tidyquant · tidyverse · TTR · PerformanceAnalytics · rugarch · plotly · zoo · memoise · testthat

---

## Future work

Possible improvements include:

- Export risk reports as PDF files
- Add rolling beta against the S&P 500
- Add rolling correlation analysis
- Add portfolio analysis using custom investment weights
- Compare every stock with the S&P 500
- Add GARCH residual checks such as QQ plots and Ljung–Box tests
- Add monitoring and clearer error messages when Yahoo Finance data is unavailable

---

*Data: Yahoo Finance via `tidyquant`using adjusted closing prices. This project is for analysis and demonstration only and is not investment advice*
