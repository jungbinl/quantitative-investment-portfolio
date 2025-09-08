# ===============================
# 📊 Quant Portfolio Strategies in R
# ===============================

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
# ⏱️ Tactical Allocation Strategy (SMA Rule)
# ===============================

symbol = c('SPY', 'SHY')      # SPY = US Stocks, SHY = US Short-term Treasuries
getSymbols(symbol, src = 'yahoo')

# Adjusted close prices
prices = do.call(cbind, lapply(symbol, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()

# Monthly endpoints
ep = endpoints(rets, on = 'months')

wts = list()
lookback = 10                  # Lookback period = 10 months

# Loop through each rebalancing date starting after lookback window
for(i in (lookback + 1) : length(ep)) {
  sub_price = prices[ep[i-lookback] : ep[i], 1]   # SPY price history for last 10 months
  sma = mean(sub_price)                           # Calculate SMA (Simple Moving Average)
  
  wt = rep(0,2)                                   # Initialize weights
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)     # If SPY > SMA → invest 100% in SPY
  wt[2] = 1 - wt[1]                               # Else invest 100% in SHY
  
  # Store weights at current rebalancing point
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

# Combine weight history into xts
wts = do.call(rbind, wts)

# Build tactical allocation portfolio
Tactical = Return.portfolio(rets, wts, verbose = T)

# Compare Buy & Hold (SPY) vs Tactical Strategy
portfolio = na.omit(cbind(rets[,1], Tactical$returns)) %>% setNames(c('Buy & Hold', 'Tactical Strategy'))

# Performance comparison
charts.PerformanceSummary(portfolio, main = "Buy & Hold VS Tactical")

# Turnover for tactical strategy
turnover = xts(rowSums(abs(Tactical$BOP.Weight - timeSeries::lag(Tactical$EOP.Weight)), na.rm = T), 
               order.by = index(Tactical$BOP.Weight))

# Plot turnover
chart.TimeSeries(turnover)
