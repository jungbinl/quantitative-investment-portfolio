
# ===============================
# 📈 Tactical Allocation Strategy with SMA (Simple Moving Average)
# ===============================

# --- Libraries ---
library(quantmod)             # Financial data (Yahoo, FRED, etc.)
library(PerformanceAnalytics) # Performance and risk analysis
library(magrittr)             # Pipe operator %>%
library(tidyr)                # Data wrangling
library(dplyr)                # Data manipulation
library(corrplot)             # Correlation visualization
library(nloptr)               # Nonlinear optimization
library(quadprog)             # Quadratic programming
library(RiskPortfolios)       # Risk-based portfolios
library(cccp)                 # Convex optimization
library(timeSeries)           # Time series utilities


# ===============================
# 🏦 Data Preparation
# ===============================

# Define symbols: 
# SPY = US Stocks (S&P500 ETF), SHY = US Short-term Treasuries ETF
symbol = c('SPY', 'SHY')

# Download historical prices from Yahoo Finance
getSymbols(symbol, src = 'yahoo')

# Extract adjusted close prices and combine into single xts object
prices = do.call(cbind, lapply(symbol, function(x) Ad(get(x))))

# Compute daily returns
rets = Return.calculate(prices) %>% na.omit()

# Identify month-end points (for monthly rebalancing)
ep = endpoints(rets, on = 'months')


# ===============================
# ⚙️ Example: Single Rebalancing Step
# ===============================

wts = list()                  # Store portfolio weights
lookback = 10                  # Lookback = 10 months

i = lookback + 1
sub_price = prices[ep[i-lookback] : ep[i], 1]   # SPY price history (last 10 months)

head(sub_price , 3)            # First 3 values of lookback window
tail(sub_price)                # Last values of lookback window

sma = mean(sub_price)          # Calculate simple moving average (SMA)

wt = rep(0,2)                  # Initialize weights (SPY, SHY)
wt[1] = ifelse(last(sub_price) > sma, 1, 0)   # If SPY > SMA → 100% SPY
wt[2] = 1 - wt[1]                               # Else → 100% SHY

# Save weights for this rebalancing date
wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))


# ===============================
# 🔁 Loop through all Rebalancing Periods
# ===============================

ep = endpoints(rets, on = 'months')
wts = list()
lookback = 10

for(i in (lookback + 1) : length(ep)) {
  sub_price = prices[ep[i-lookback] : ep[i], 1]  # SPY price history
  sma = mean(sub_price)                          # SMA over lookback window
  
  wt = rep(0,2)                                  # Initialize weights
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)    # Rule: invest in SPY if above SMA
  wt[2] = 1 - wt[1]                              # Else invest in SHY
  
  # Store weights
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

# Combine all weight histories into xts
wts = do.call(rbind, wts)


# ===============================
# 📊 Build Tactical Portfolio
# ===============================

# Construct tactical strategy portfolio returns
Tactical = Return.portfolio(rets, wts, verbose = T)

# Compare Buy & Hold (SPY) vs Tactical SMA Strategy
portfolio = na.omit(cbind(rets[,1], Tactical$returns)) %>% 
            setNames(c('Buy & Hold', 'Tactical SMA Strategy'))

# Performance charts (cumulative returns, drawdowns, risk metrics)
charts.PerformanceSummary(portfolio, main = "Buy & Hold VS Tactical")


# ===============================
# 🔄 Turnover Analysis
# ===============================

# Calculate portfolio turnover (rebalancing intensity)
turnover = xts(
  rowSums(abs(Tactical$BOP.Weight - timeSeries::lag(Tactical$EOP.Weight)), na.rm = T), 
  order.by = index(Tactical$BOP.Weight)
)

# Plot turnover over time
chart.TimeSeries(turnover)
