##############################################
# Libraries
##############################################

library(stringr)              # String manipulation (useful for handling tickers, codes, etc.)
library(xts)                  # Time series data handling (especially for financial data)
library(PerformanceAnalytics) # Performance and return calculations for financial assets
library(dplyr)                # Data wrangling (filter, mutate, group_by, summarise, etc.)
library(ggplot2)              # Visualization (used to plot sector allocation)

##############################################
# 1. Load Data
##############################################

# Load daily price data (rows = dates, columns = stock tickers)
kor_price <- read.csv('kor_price.csv', row.names = 1, stringsAsFactors = FALSE) %>% as.xts()

# Load ticker information (fundamental and sector data)
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

# Rename columns for clarity
names(kor_ticker) <- c('name', 'end_price', 'compare', 'fluctuation_rate', 
                       'code', 'EPS', 'pre_PER', 'pre_EPS', 'PER', 'BPS', 
                       'PBR', 'dividend', 'dividend_rate', 'market', 
                       'industry', 'company_price')

##############################################
# 2. Calculate Returns (12-Month Momentum)
##############################################

# Compute daily returns, keep last 252 trading days (~12 months)
ret = Return.calculate(kor_price) %>% xts::last(252)

# Aggregate into 12-month cumulative returns
ret_12m <- ret %>% sapply(., function(x){ prod(1 + x) - 1 })

# Select top 30 stocks by 12-month momentum
invest_12mom <- order(ret_12m, decreasing = TRUE)[1:500]

##############################################
# 3. Sector Exposure of Momentum Portfolio
##############################################

# Extract sector information for top momentum stocks
kor_sector <- kor_ticker %>% select('industry', 'code', 'name')

# Count number of selected stocks by sector
a = table(kor_sector[invest_12mom, ] %>% select('industry')) %>% data.frame()

# Plot sector distribution
ggplot(a, aes(x = reorder(industry, Freq), y = Freq, label = Freq)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) + ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0,0.1,0)) +
  theme_classic()

##############################################
# 4. Data Cleaning
##############################################

# Remove suspended/delisted stocks from the ticker dataset
kor_sector <- kor_sector[kor_sector$code %in% intersect(kor_sector$code, str_sub(names(ret_12m), 2, 7))&(!is.na(kor_sector$code)), ]

##############################################
# 5. Sector-Neutral Momentum Portfolio
##############################################

# Standardize momentum within each sector (z-score)
sector_neutral = kor_sector %>% 
  select('code', 'industry') %>%
  mutate('ret' = ret_12m) %>%
  group_by(industry) %>%
  mutate(scale_per_sector = scale(ret),
         scale_per_sector = ifelse(is.na(industry), NA, scale_per_sector))

# Select top 30 stocks across sectors (sector-neutral momentum strategy)
invest_mom_neutral = rank(-sector_neutral$scale_per_sector) <= 30

# Count selected stocks by sector
kor_ticker[invest_mom_neutral, ] %>%
  select('industry') %>%
  group_by(industry) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(industry, n), y = n, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) + ylab(NULL) +
  coord_flip()

