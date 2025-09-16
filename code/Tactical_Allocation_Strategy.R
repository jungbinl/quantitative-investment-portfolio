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
# ⏱️ Tactical Allocation Strategy (SMA Rule)
# ===============================

symbol = c('SPY', 'SHY')      # SPY = US Stocks, SHY = US Short-term Treasuries
getSymbols(symbol, src = 'yahoo')

# Adjusted close prices
prices = do.call(cbind, lapply(symbol, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()

# Monthly endpoints

wts = list()
lookback = 10

ep = endpoints(rets, on = 'months')

for(i in (lookback + 1):length(ep)) {
  start_idx <- max(ep[i - lookback] + 1, 1)
  end_idx   <- ep[i]
  
  sub_price <- prices[start_idx:end_idx, 1]
  sma <- mean(as.numeric(sub_price), na.rm = TRUE)
  
  wt <- rep(0, 2)
  wt[1] <- ifelse(as.numeric(last(sub_price)) > sma, 1, 0)
  wt[2] <- 1 - wt[1]
  
  wts[[i]] <- xts(t(wt), order.by = index(rets[end_idx]))
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
table(turnover)
# Plot turnover
chart.TimeSeries(turnover)

# ===============================
# portfolio_results
# ===============================
portfolio_results <- data.frame(
  trade_date = index(Tactical$returns),
  portfolio_return = as.numeric(Tactical$returns),
  cumulative_return = cumprod(1 + as.numeric(Tactical$returns)) - 1,
  turnover = as.numeric(turnover)
)

write.csv(portfolio_results, "tactical_sma_portfolio_results.csv", row.names = FALSE)

# ===============================
# portfolio_stocks
# ===============================
rets <- rets[210:nrow(rets), ]

stocks_info <- data.frame(
  trade_date = index(rets),
  SPY_return = rets[ , 1],
  SHY_return = rets[ , 2],
  SPY_BOP_weight = Tactical$BOP.Weight[ ,1], 
  SHY_BOP_weight = Tactical$BOP.Weight[ ,2],
  SPY_EOP_weight = Tactical$EOP.Weight[ ,1], 
  SHY_EOP_weight = Tactical$EOP.Weight[ ,2]
)

write.csv(stocks_info, "tactical_sma_portfolio_stocks.csv", row.names = FALSE)

# ===============================
# asset_prices
# ===============================
prices <- prices[211:nrow(prices), ]

prices_df <- data.frame(
  trade_date = index(prices),
  SPY_adj_close = as.numeric(prices[, 1]),
  SHY_adj_close = as.numeric(prices[, 2]),
  SPY_return = rets[, 1],
  SHY_return = rets[, 2]
)

write.csv(prices_df, "tactical_sma_asset_prices.csv", row.names = FALSE)
