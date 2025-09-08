
# ===============================
# Libraries Used in This Project
# ===============================

library(quantmod)            # Downloading and handling financial market data (Yahoo Finance API)
library(PerformanceAnalytics)# Portfolio performance and return calculations
library(magrittr)            # Pipe operator (%>%) for cleaner workflows
library(tidyr)               # Data tidying and reshaping
library(dplyr)               # Data wrangling (filter, mutate, summarize)
library(corrplot)            # Visualization of correlation matrices
library(nloptr)              # Nonlinear optimization (Sequential Least Squares Quadratic Programming)
library(quadprog)            # Quadratic programming solver for portfolio optimization
library(RiskPortfolios)      # Ready-to-use portfolio optimization tools (e.g., min-vol portfolio)
library(ggplot2)             # Data visualization (portfolio weights, charts)

# ----------------------
# Data Preparation
# ----------------------
symbols <- c('SPY','IEV','EWJ','EEM','TLT','IEF','IYR','RWX','GLD','DBC')

# Download adjusted prices from Yahoo Finance
getSymbols(symbols, src = 'yahoo')

# Combine into one price matrix
prices <- do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>% 
  setNames(symbols)

# Convert to returns and remove NA
rets <- Return.calculate(prices) %>% na.omit()

# ----------------------
# Correlation Structure
# ----------------------
cor(rets) %>% 
  corrplot(method = 'color', 
           type = 'upper', 
           addCoef.col = 'black', 
           number.cex = 0.7, 
           tl.cex = 0.6, 
           tl.srt = 45, 
           tl.col = 'black', 
           col = colorRampPalette(c('blue','white','red'))(200),
           mar = c(0,0,0.5,0))

# Covariance matrix (used in optimization)
covmat <- cov(rets)

# ----------------------
# Objective Function
# ----------------------
# Portfolio variance = w' Σ w
objective <- function(w) {
  obj <- t(w) %*% covmat %*% w 
  return(as.numeric(obj))
}

# Inequality constraint: weights >= 0
hin.objective <- function(w) { return(w) }

# Equality constraint: sum(weights) = 1
heq.objective <- function(w) { sum(w) - 1 }

# ----------------------
# 1. Unconstrained Min-Variance Portfolio
# ----------------------
result <- slsqp(x0 = rep(0.1,10), fn = objective, hin = hin.objective, heq = heq.objective)
w_1 <- result$par %>% round(4) %>% setNames(colnames(rets))
print(w_1)

# Quadratic Programming approach
Dmat <- covmat
dvec <- rep(0,10)
Amat <- t(rbind(rep(1,10), diag(10), -diag(10)))
bvec <- c(1, rep(0,10), -rep(1,10))
meq  <- 1

result <- solve.QP(Dmat, dvec, Amat, bvec, meq)
w_2 <- result$solution %>% round(4) %>% setNames(colnames(rets))
print(w_2)

# RiskPortfolios built-in solver
w_3 <- optimalPortfolio(covmat, control = list(type = 'minvol', constraint = 'lo')) %>% 
  round(4) %>% setNames(colnames(rets))
print(w_3)

# Visualize weights
data.frame(w_1) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w_1)) + 
  geom_col() + xlab(NULL) + ylab("Weight")

# ----------------------
# 2. Min-Variance with Bounds (0.05 ≤ w ≤ 0.20)
# ----------------------
result <- slsqp(x0 = rep(0.1,10), fn = objective, hin = hin.objective, heq = heq.objective,
                lower = rep(0.05,10), upper = rep(0.20,10))
w_4 <- result$par %>% round(4) %>% setNames(colnames(rets))
print(w_4)

# Quadratic Programming with bounds
Dmat <- covmat
dvec <- rep(0,10)
Amat <- t(rbind(rep(1,10), diag(10), -diag(10)))
bvec <- c(1, rep(0.05,10), -rep(0.20,10))
meq  <- 1

result <- solve.QP(Dmat, dvec, Amat, bvec, meq)
w_5 <- result$solution %>% round(4) %>% setNames(colnames(rets))
print(w_5)

# RiskPortfolios with user constraints
w_6 <- optimalPortfolio(covmat, control = list(type = 'minvol', constraint = 'user', 
                                               LB = rep(0.05,10), UB = rep(0.20,10))) %>% 
  round(4) %>% setNames(colnames(rets))
print(w_6)

# Visualization with bounds
data.frame(w_6) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w_6)) + 
  geom_col() + 
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab("Weight")

# ----------------------
# 3. Min-Variance with Custom Bounds
# ----------------------
# Asset-specific lower/upper bounds
Dmat <- covmat
dvec <- rep(0,10)
Amat <- t(rbind(rep(1,10), diag(10), -diag(10)))
bvec <- c(1,
          c(0.10,0.10,0.05,0.05,0.10,0.10,0.05,0.05,0.03,0.03), 
          -c(0.25,0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.08,0.08))
meq <- 1

result <- solve.QP(Dmat, dvec, Amat, bvec, meq)
w_7 <- result$solution %>% round(4) %>% setNames(colnames(rets))
print(w_7)

# Visualization with custom bounds
data.frame(w_7) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)), y = w_7)) + 
  geom_col() + 
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab("Weight")
