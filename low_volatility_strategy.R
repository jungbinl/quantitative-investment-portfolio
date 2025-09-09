# -----------------------------
# Libraries
# -----------------------------
library(quantmod)             # For downloading financial data
library(PerformanceAnalytics) # For return calculation and performance analysis
library(magrittr)             # For piping (%>%)

# -----------------------------
# Example: Beta calculation (CAPM)
# -----------------------------
symbols = c('102110.KS', '039490.KS')   # Example stock (ri) and market index (rm: KOSPI 200)
getSymbols(symbols)                     # Download price data from Yahoo Finance
prices = do.call(cbind, lapply(symbols, function(x)Cl(get(x))))  # Extract closing prices
ret = Return.calculate(prices)                                   # Calculate returns
ret = ret['2020-01::2025-07']                                    # Filter date range
rm = ret[, 1]   # Market return (KOSPI 200)
ri = ret[, 2]   # Individual stock return

# Linear regression: ri = α + β * rm
reg <- lm(ri ~ rm)
summary(reg)

# Scatter plot: Market return vs Stock return
plot(as.numeric(rm), as.numeric(ri), 
     pch = 1, cex = 1,
     xlab = "KOSPI 200", ylab = "Individual Stock",
     xlim = c(-0.02, 0.02), ylim = c(-0.02, 0.02))
abline(a = 0, b = 1, lty = 2)   # 45-degree line (β = 1 reference)
abline(reg, col = 'red')        # Regression line

# -----------------------------
# Libraries for low-volatility strategy
# -----------------------------
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

# -----------------------------
# Daily low-volatility strategy
# -----------------------------
kor_price <- read.csv('final_price.csv', row.names = 1, stringsAsFactors = F) %>% as.xts()
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

ret = Return.calculate(kor_price)  
# Calculate 12-month daily volatility (annualized)
std_12m_daily <- xts::last(ret, 252) %>% apply(., 2, sd) %>% multiply_by(sqrt(252)) %>% data.frame()

# Plot volatility distribution
ggplot(std_12m_daily, aes(x = .)) + 
  geom_histogram(binwidth = 0.01) + 
  xlim(-0.1, 2) + 
  annotate("rect", xmin = -0.02, xmax = 0.02, 
           ymin = 0, ymax = sum(std_12m_daily == 0, na.rm = T) * 1.1, 
           alpha = 0.3, fill = "red") +
  xlab("Annualized Volatility")

# Remove zero volatility
std_12m_daily[std_12m_daily == 0] = NA

# Rank stocks by volatility (lowest first) and select top 30
std_12m_daily %>% mutate(rank = rank(.)) %>% filter(rank <= 30)

# Extract top 30 low-volatility stocks
std_12m_daily_rank <- std_12m_daily[rank(std_12m_daily) <= 30, ] %>% data.frame()
ggplot(std_12m_daily_rank, aes(x = rep(1:30), y = .)) + geom_col()

invest_lowvol = rank(std_12m_daily) <= 30
kor_ticker[invest_lowvol, ] %>% 
  select('종목코드', '종목명') %>% 
  mutate('변동성' = round(std_12m_daily[rank(std_12m_daily) <= 30, ], 4))

# -----------------------------
# Weekly low-volatility strategy
# -----------------------------
# Convert to weekly returns, calculate 12-month volatility (annualized)
std_12m_weekly <- xts::last(ret, 252) %>% 
  apply.weekly(Return.cumulative) %>% 
  apply(., 2, sd) %>% 
  multiply_by(sqrt(52)) %>% 
  data.frame()

std_12m_weekly[std_12m_weekly == 0] = NA
std_12m_weekly <- std_12m_weekly %>% mutate(rank = round(rank(.), 0)) 
std_12m_weekly_top30 <- std_12m_weekly %>% filter(rank <= 30)

invest_lowvol_week <- rank(std_12m_weekly) <= 30
kor_ticker[invest_lowvol_week, ] %>% 
  select('종목코드', '종목명') %>% 
  mutate('변동성' = round(std_12m_daily[rank(std_12m_daily) <= 30, ], 4))

# -----------------------------
# Intersection: stocks selected by both daily and weekly low-volatility
# -----------------------------
intersect(kor_ticker[invest_lowvol, '종목명'], 
          kor_ticker[invest_lowvol_week, '종목명'])
