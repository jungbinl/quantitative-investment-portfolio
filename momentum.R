library(xts)                  # as.xts(), last() for time-series handling
library(PerformanceAnalytics) # Return.calculate() for return calculation
library(magrittr)             # %>%, multiply_by() for piping and arithmetic
library(dplyr)                # select(), mutate() for data wrangling
library(tidyr)                # gather() for data reshaping (wide → long)
library(ggplot2)              # ggplot(), geom_line(), facet_wrap() for visualization

# ------------------------------------------------
# Load price and ticker data
# ------------------------------------------------
kor_price <- read.csv('kor_price.csv', row.names = 1, stringsAsFactors = F) %>% as.xts()
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")
names(kor_ticker) <- c(
  'name', 'end_price', 'compare', 'fluctuation rate', 'code',
  'EPS', 'pre_PER', 'pre_EPS', 'PER', 'BPS', 'PBR',
  'dividend', 'dividend_rate', 'market', 'industry', 'company_price'
)

# ------------------------------------------------
# 12-Month Momentum Strategy
# ------------------------------------------------
ret = Return.calculate(kor_price) %>% xts::last(360)   # calculate daily returns, last 360 days (~12 months)
ret_12m <- ret %>% sapply(., function(x){prod(1+x) - 1})   # compound return over 12 months

invest_12mom <- order(ret_12m, decreasing = TRUE)[1:30]     # top 30 stocks by 12M return
kor_ticker[invest_12mom, ] %>%
  select(code, name) %>%
  mutate(profit_rate = round(ret_12m[invest_12mom], 4))

# ------------------------------------------------
# 6-Month Momentum Strategy
# ------------------------------------------------
ret = Return.calculate(kor_price) %>% xts::last(180)   # last 180 days (~6 months)
ret_6m = ret %>% sapply(., function(x){prod(1+x) - 1})

invest_6mom <- order(ret_6m, decreasing = TRUE)[1:30]
kor_ticker[invest_6mom, ] %>%
  select(code, name) %>%
  mutate(profit_rate = round(ret_6m[invest_6mom], 4))

# ------------------------------------------------
# 3-Month Momentum Strategy
# ------------------------------------------------
ret = Return.calculate(kor_price) %>% xts::last(90)   # last 90 days (~3 months)
ret_3m = ret %>% sapply(., function(x){prod(1+x) - 1})

invest_3mom <- order(ret_3m, decreasing = TRUE)[1:30]
kor_ticker[invest_3mom, ] %>%
  select(code, name) %>%
  mutate(profit_rate = round(ret_3m[invest_3mom], 4))

# ------------------------------------------------
# Risk-adjusted Momentum (Sharpe Ratio approach)
# ------------------------------------------------
ret = Return.calculate(kor_price) %>% xts::last(252)  # ~1 trading year (252 trading days)
ret_12m = ret %>% sapply(., function(x){prod(1+x) - 1})   # annual return
std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))   # annualized volatility
sharpe_12m = ret_12m / std_12m   # risk-adjusted return (Sharpe-like ratio)

# Select top 30 stocks by raw 12M return (not Sharpe ratio itself)
invest_mom_sharpe = order(ret_12m, decreasing = T)[1:30]

kor_ticker[invest_mom_sharpe, ] %>%
  select('code', 'name') %>%
  mutate('adjust_profit_rate' = round(ret_12m[invest_mom_sharpe], 4))

# ------------------------------------------------
# Visualization: Price charts for selected portfolios
# ------------------------------------------------
kor_price_month = kor_price[, invest_mom_sharpe] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) 

kor_price_day = kor_price[, invest_12mom] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index)

ggplot(kor_price_day, aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap( ~ ticker, scales = 'free') +
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# ------------------------------------------------
# Compare overlap between momentum strategies
# ------------------------------------------------
intersect(
  kor_ticker[invest_mom, 'name'],
  kor_ticker[invest_mom_sharpe, 'name']
)
