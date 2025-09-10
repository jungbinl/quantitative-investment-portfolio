# --- Libraries ---
library(quantmod)            # Get financial data (e.g., Yahoo Finance)
library(PerformanceAnalytics) # Performance and risk analysis
library(magrittr)             # Pipe operator %>%
library(tidyr)                # Data wrangling
library(dplyr)                # Data manipulation
library(corrplot)             # Correlation plot visualization
library(nloptr)               # Nonlinear optimization
library(quadprog)             # Quadratic programming for portfolio optimization
library(RiskPortfolios)       # Risk-based portfolio construction
library(cccp)                 # Convex optimization
library(timeSeries)           # Time series data handling


# ===============================
# ⚖️ 60/40 Portfolio
# ===============================

ticker = c('SPY' , 'TLT')     # SPY = US Stocks, TLT = US Long-Term Treasuries

getSymbols(ticker)            # Download price data from Yahoo Finance

# Extract adjusted close prices and combine into one xts object
prices = do.call(cbind, lapply(ticker, function(x) Ad(get(x))))

# Calculate log returns
rets = Return.calculate(prices) %>% na.omit()

# Correlation between asset returns
cor(rets)

# Build a 60% SPY + 40% TLT portfolio, annual rebalancing
portfolio = Return.portfolio(R = rets, weights = c(0.6,0.4), rebalance_on = 'years', verbose = T)
portfolio$EOP.Value            # End of period value

# Combine stock, bond, and portfolio returns for comparison
portfolios = cbind(rets, portfolio$returns) %>% setNames(c('Stocks', 'Bonds', '60-40'))

# Performance summary (cumulative returns, drawdown, etc.)
charts.PerformanceSummary(portfolios, main = '60/40 Portfolio')

# Calculate turnover (portfolio rebalancing activity)
turnover = xts(rowSums(abs(portfolio$BOP.Weight - timeSeries::lag(portfolio$EOP.Weight)), na.rm = T), 
               order.by = index(portfolio$BOP.Weight))

# Plot turnover over time
chart.TimeSeries(turnover)

# ===============================
# portfolio_results
# ===============================
portfolio_results <- data.frame(
  trade_date = index(portfolio$returns),
  portfolio_return = as.numeric(portfolio$returns),
  cumulative_return = cumprod(1 + as.numeric(portfolio$returns)) - 1,
  turnover = as.numeric(turnover)
)

write.csv(portfolio_results, "60_40_portfolio_results.csv", row.names = FALSE)

# ===============================
# portfolio_stocks
# ===============================
stocks_info <- data.frame(
  trade_date = index(rets),
  SPY_return = rets[, 1],
  TLT_return = rets[, 2],
  SPY_BOP_weight = portfolio$BOP.Weight[, 1],
  TLT_BOP_weight = portfolio$BOP.Weight[, 2],
  SPY_EOP_weight = portfolio$EOP.Weight[, 1],
  TLT_EOP_weight = portfolio$EOP.Weight[, 2]
)

write.csv(stocks_info, "60_40_portfolio_stocks.csv", row.names = FALSE)

# ===============================
# asset_prices
# ===============================
prices <- prices[2:nrow(prices), ]

prices_df <- data.frame(
  trade_date = index(prices),
  SPY_adj_close = as.numeric(prices[, 1]),
  TLT_adj_close = as.numeric(prices[, 2]),
  SPY_return = rets[, 1],
  TLT_return = rets[, 2]
)

write.csv(prices_df, "60_40_asset_prices.csv", row.names = FALSE)
