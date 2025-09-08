library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)
library(RiskPortfolios)


# Define ETF tickers for asset classes:
symbols = c('SPY',   # US Equity
            'IEV',   # Europe Equity
            'EWJ',   # Japan Equity
            'EEM',   # Emerging Markets Equity
            'TLT',   # Long-term Treasuries
            'IEF',   # Intermediate Treasuries
            'IYR',   # US Real Estate
            'RWX',   # Global Real Estate
            'GLD',   # Gold
            'DBC')   # Commodities

# Download data from Yahoo Finance
getSymbols(symbols, src = 'yahoo')

# Extract adjusted prices and combine into one xts object
prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>% 
  setNames(symbols)

# Calculate daily returns and remove NA values
rets <- Return.calculate(prices) %>% na.omit()

# Covariance matrix of asset returns
covmat = cov(rets)

# Function: Compute Risk Contribution (RC) for given weights
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w          # Portfolio variance
  port_std = sqrt(port_vol)                 # Portfolio volatility
  
  MRC = (covmat %*% w) / as.numeric(port_std) # Marginal Risk Contribution
  RC = MRC * w                               # Total Risk Contribution per asset
  RC = c(RC / sum(RC))                       # Normalize so they sum to 1
  
  return(RC)
}

# Example: Stock (SPY) and Bond (TLT) portfolio
ret_stock_bond = rets[, c(1,5)]           # Select SPY & TLT returns
cov_stock_bond = cov(ret_stock_bond)      # Covariance matrix of 2 assets

# Risk contribution with 60/40 portfolio
RC_stcok_bond = get_RC(c(0.6, 0.4), cov_stock_bond)
RC_stock_bond = round(RC_stcok_bond, 4)
print(RC_stock_bond)

# Risk parity optimization (equal risk contribution)
opt = rp(x0 = rep(0.1, 10),    # Initial guess: 10% per asset
         P = covmat,           # Covariance matrix
         mrc = rep(0.1, 10))   # Target marginal risk contributions = equal

w = getx(opt) %>% drop()       # Extract optimized weights
w_1 = (w / sum(w)) %>% round(., 4) %>% setNames(colnames(rets)) # Normalize
print(w_1)

# Compute risk contributions for these weights
get_RC(w_1, covmat)
