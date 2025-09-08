# ===============================
# Libraries Used in This Project
# ===============================
library(quantmod)             # Downloading and handling financial market data (Yahoo Finance API)
library(PerformanceAnalytics) # Portfolio performance and return calculations
library(magrittr)             # Pipe operator (%>%) for cleaner workflows
library(tidyr)                # Data tidying and reshaping
library(dplyr)                # Data wrangling (filter, mutate, summarize)
library(corrplot)             # Visualization of correlation matrices
library(nloptr)               # Nonlinear optimization (SLSQP solver)
library(quadprog)             # Quadratic programming solver for portfolio optimization
library(RiskPortfolios)       # Ready-to-use portfolio optimization tools (e.g., min-vol, max-div)
library(ggplot2)              # Data visualization (portfolio weights, charts)

# ===============================
# Data Collection
# ===============================
symbols = c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT', 'IEF', 'IYR', 'RWX', 'GLD', 'DBC') 
# SPY: S&P500, IEV: Europe, EWJ: Japan, EEM: Emerging Markets,
# TLT/IEF: US Treasuries, IYR/RWX: Real Estate, GLD: Gold, DBC: Commodities

getSymbols(symbols, src = 'yahoo')  # Download daily price data from Yahoo Finance

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>% setNames(symbols)
# Extract adjusted closing prices and bind into one xts object

rets <- Return.calculate(prices) %>% na.omit()
# Calculate daily returns and remove missing values

covmat = cov(rets) 
# Compute covariance matrix of returns (used in optimization)

# ===============================
# Optimization 1: Basic Max-Diversification
# ===============================
Dmat = covmat
dvec = rep(0,10)
Amat = t(rbind(sqrt(diag(covmat)), diag(10)))   # Constraints: diversification ratio & weights ≥ 0
bvec = c(1, rep(0,10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution %>% round(., 4) %>% setNames(colnames(rets)) 
# Raw weights

w_1 = (w / sum(w)) %>% round(., 4)   # Normalize so weights sum to 1

# Plot portfolio weights
data.frame(w_1) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w)) +
  geom_col() + xlab(NULL) + ylab(NULL)

# Alternative max-div portfolio using RiskPortfolios package
w_2 = optimalPortfolio(covmat, control = list(type = 'maxdiv', constraint = 'lo')) %>% round(., 4)

print(w)

# ===============================
# Optimization 2: With Uniform Bounds (5%–20%)
# ===============================
Dmat = covmat
dvec = rep(0,10)
Alb = -rep(0.05, 10) %*% matrix(1,1,10) + diag(10)   # Lower bound 5%
Aub = rep(0.20, 10) %*% matrix(1,1,10) - diag(10)    # Upper bound 20%
Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0,10), rep(0,10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)
w = result$solution
w_3 = (w / sum(w)) %>% round(., 4) %>% setNames(colnames(rets))

print(w_3)

# Plot portfolio with bounds visualized
data.frame(w_3) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

# Alternative bounded max-div portfolio using RiskPortfolios
w_4 = optimalPortfolio(covmat, control = list(type = 'maxdiv', constraint = 'user', 
                                              LB = rep(0.05,10), UB = rep(0.20, 10))) %>% round(., 4)

print(w_4)

# ===============================
# Optimization 3: Custom Bounds per Asset
# ===============================
Dmat = covmat
dvec = rep(0,10)
Alb = -c(0.10,0.10,0.05,0.05,0.10,0.10,0.05,0.05,0.03,0.03) %*% matrix(1,1,10) + diag(10)
# Lower bounds differ by asset
Aub = c(0.25,0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.08,0.08) %*% matrix(1,1,10) - diag(10)
# Upper bounds differ by asset
Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0,10), rep(0,10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)
w = result$solution
w_5 = (w / sum(w)) %>% round(., 4) %>% setNames(colnames(rets))

print(w_5)

# Plot with custom bounds highlighted
data.frame(w_5) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

# Alternative custom-bounded max-div portfolio
w_6 = optimalPortfolio(covmat, control = list(type = 'maxdiv', constraint = 'user', 
                                              LB = c(0.10,0.10,0.05,0.05,0.10,0.10,0.05,0.05,0.03,0.03), 
                                              UB = c(0.25,0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.08,0.08))) %>% round(., 4)

print(w_6)
