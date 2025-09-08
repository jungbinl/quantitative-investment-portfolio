# ===============================
# Libraries Used in This Project
# ===============================

library(xts)                # Time-series data structures & methods (used for stock prices and returns)
library(stringr)            # String manipulation (standardizing stock codes to 6 digits)
library(PerformanceAnalytics)# Financial analysis toolkit (cumulative returns, rolling returns)
library(dplyr)              # Data wrangling (filter, mutate, rank, aggregate)
library(corrplot)           # Correlation matrix visualization (Quality, Value, Momentum factor relationships)
library(tidyr)              # Data tidying & reshaping (convert wide to long format for visualization)


# -------------------------------
# 1. Data Import & Preprocessing
# -------------------------------

# Stock price data (converted to xts object for time-series operations)
kor_price <- read.csv('kor_price.csv', row.names = 1, stringsAsFactors = F) %>% as.xts()

# Ticker metadata (company code, company name, etc.)
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

# Value indicators (e.g., PER, PBR, dividend yield)
kor_value <- read.csv('kor_value_indicator.csv', row.names = 1, stringsAsFactors = F) 
kor_value <- kor_value %>% mutate(code = str_pad(kor_value$code , 6, "left", 0)) %>% arrange(code)

# Financial statement data (list format: assets, liabilities, sales, etc.)
kor_fs <- readRDS('kor_fs.Rds')

# Utility function: clean financial statements by normalizing stock codes (6-digit) and sorting
clean_fs <- function(df) {
  df %>%
    mutate(code = str_pad(code, 6, "left", "0")) %>%
    arrange(code)
}

# Extract and clean key financial statement items
assets              <- clean_fs(kor_fs$자산)
net_income_attr     <- clean_fs(kor_fs$지배주주순이익)
money               <- clean_fs(kor_fs$자본)
net_income          <- clean_fs(kor_fs$당기순이익)
cash_flow           <- clean_fs(kor_fs$영업활동으로인한현금흐름)
long_term_debt      <- clean_fs(kor_fs$장기차입금)
current_assets      <- clean_fs(kor_fs$유동자산)
current_debt        <- clean_fs(kor_fs$유동부채)
paid_in_capital     <- clean_fs(kor_fs$유상증자)
gross_profit        <- clean_fs(kor_fs$매출총이익)
sales               <- clean_fs(kor_fs$매출액)
debt                <- clean_fs(kor_fs$부채)
company_tax         <- clean_fs(kor_fs$법인세비용)
interest            <- clean_fs(kor_fs$이자비용)
money_asset         <- clean_fs(kor_fs$현금및현금성자산)
depreciation_expense<- clean_fs(kor_fs$감가상각비)
real_asset          <- clean_fs(kor_fs$유형자산)
unreal_asset        <- clean_fs(kor_fs$무형자산)
act_asset           <- clean_fs(kor_fs$기타비유동자산)
invest_asset        <- clean_fs(kor_fs$장기금융자산)

# -------------------------------
# 2. Momentum Factor Construction
# -------------------------------

# Calculate 3M / 6M / 12M returns using rolling cumulative product of daily returns
ret_3m <- Return.calculate(kor_price) %>% xts::last(90) %>% sapply(., function(x){prod(1+x) -1})
ret_6m <- Return.calculate(kor_price) %>% xts::last(180) %>% sapply(., function(x){prod(1+x) -1})
ret_12m <- Return.calculate(kor_price) %>% xts::last(360) %>% sapply(., function(x){prod(1+x) -1})

# Bind returns into a single data frame
ret_bind <- cbind(ret_3m, ret_6m, ret_12m) %>% data.frame()
rownames(ret_bind) = str_sub(rownames(ret_bind), 2)
ret_bind <- ret_bind %>% mutate(code = rownames(ret_bind))
rownames(ret_bind) = NULL

# -------------------------------
# 3. Common Universe Filtering
# -------------------------------

# Identify codes that are present in all relevant datasets
common_codes <- Reduce(intersect, list(
  net_income_attributable$code, assets$code, net_income$code, cash_flow$code, 
  long_term_debt$code, current_assets$code, current_debt$code, paid_in_capital$code, 
  gross_profit$code, sales$code, money$code, debt$code, company_tax$code, 
  interest$code, money_asset$code, depreciation_expense$code, real_asset$code, 
  unreal_asset$code, act_asset$code, invest_asset$code, kor_value$code, ret_bind$code
))

# Extract aligned code metadata
code_data <- assets[which(assets$code %in% common_codes),  4]
kor_value = kor_value[which(kor_value$code %in% common_codes),  ]

# -------------------------------
# 4. Financial Ratios
# -------------------------------

# Generic ratio constructor
make_ratio <- function(numerator_df, denominator_df = NULL, common_codes, code_data, drop_col = 4) {
  num <- numerator_df[which(numerator_df$code %in% common_codes), -drop_col]
  
  if (!is.null(denominator_df)) {
    den <- denominator_df[which(denominator_df$code %in% common_codes), -drop_col]
    result <- num / den
  } else {
    result <- num
  }
  
  result <- result %>% mutate(code = code_data)
  return(result)
}

# Profitability, leverage, liquidity, efficiency ratios
ROA     <- make_ratio(net_income_attributable, assets, common_codes, code_data)
CFO     <- make_ratio(cash_flow, assets, common_codes, code_data)
ACCRUAL <- (CFO[ ,-4] - ROA[ ,-4]) %>% mutate(code = code_data)
LEV     <- make_ratio(long_term_debt, assets, common_codes, code_data)
LIQ     <- make_ratio(current_assets, current_debt, common_codes, code_data)
OFFER   <- make_ratio(paid_in_capital, NULL, common_codes, code_data) # equity issuance (no denominator)
MARGIN  <- make_ratio(gross_profit, sales, common_codes, code_data)
TURN    <- make_ratio(sales, assets, common_codes, code_data)
ROE     <- make_ratio(net_income_attributable, money, common_codes, code_data)
GPA     <- make_ratio(gross_profit, assets, common_codes, code_data)

# -------------------------------
# 5. Factor Construction (Quality, Value, Momentum)
# -------------------------------

# Quality: ROE, Gross Profitability, CFO
ROE <- ROE[2]; GPA <- GPA[2]; CFO <- CFO[2]
quality_profit <- cbind(ROE, GPA, CFO) %>% setNames(., c('ROE','GPA','CFO'))

factor_quality <- quality_profit %>% 
  mutate_all(list(~min_rank(desc(.)))) %>%  # rank each factor (higher is better)
  mutate_all(list(~scale(.))) %>%           # standardize (z-score)
  rowSums()                                 # combine into composite score

# Value: based on valuation metrics (e.g., PBR, PER from kor_value)
factor_value <- kor_value %>% 
  mutate_all(list(~min_rank(.))) %>%        # lower is better (cheap stocks)
  mutate_all(list(~scale(.))) %>% 
  rowSums()

# Momentum: based on 3M, 6M, 12M returns
ret_bind <- ret_bind[which(ret_bind$code %in% common_codes),  -4]
factor_mom <- ret_bind %>% 
  mutate_all(list(~min_rank(desc(.)))) %>% 
  mutate_all(list(~scale(.))) %>% 
  rowSums()

# -------------------------------
# 6. Factor Correlation & Composite Score (QVM)
# -------------------------------

# Correlation between Quality, Value, Momentum
cbind(factor_quality, factor_value, factor_mom) %>% 
  data.frame() %>% 
  setNames(c('Quality','Value','Momentum')) %>% 
  cor(use = 'complete.obs') %>% 
  round(., 2) %>% 
  corrplot(method = 'color', type = 'upper', addCoef.col = 'black', 
           number.cex = 1, tl.cex = 0.6, tl.srt = 45, tl.col = 'black', 
           col = colorRampPalette(c('blue', 'white', 'red'))(200), mar = c(0,0,0.5,0))

# Equal-weighted composite factor (QVM score)
factor_qvm <- cbind(factor_quality, factor_value, factor_mom) %>% 
  data.frame() %>% 
  mutate_all(list(~scale(.))) %>% 
  mutate(factor_quality = factor_quality * 0.33, 
         factor_value   = factor_value * 0.33, 
         factor_mom     = factor_mom * 0.33) %>% 
  rowSums()

# -------------------------------
# 7. Portfolio Construction
# -------------------------------

# Select top 30 stocks by QVM rank
invest_qvm <- rank(factor_qvm) <= 30

# Diagnostics: factor distributions of selected stocks
quality_profit[invest_qvm, ] %>% gather() %>% ggplot(aes(x = value)) + 
  geom_histogram() + facet_wrap(. ~key, scale = 'free', ncol = 1) + xlab(NULL)

kor_value[invest_qvm, -5] %>% gather() %>% ggplot(aes(x = value)) + 
  geom_histogram() + facet_wrap(. ~key, scale = 'free', ncol = 1) + xlab(NULL)

ret_bind[invest_qvm, ] %>% gather() %>% ggplot(aes(x = value)) + 
  geom_histogram() + facet_wrap(. ~key, scale = 'free', ncol = 1) + xlab(NULL)

# -------------------------------
# 8. Portfolio Output
# -------------------------------

# Join ticker info with factors (for reporting)
kor_ticker <- kor_ticker[which(kor_ticker$종목코드 %in% common_codes), ] %>% arrange(종목코드)
kor_ticker[invest_qvm, ] %>% 
  select('종목코드', '종목명') %>% 
  cbind(round(ROE[invest_qvm, ], 2)) %>% 
  cbind(round(kor_value$pbr[invest_qvm] ,2)) %>% 
  cbind(round(ret_bind[invest_qvm, 3], 2)) %>% 
  setNames(c('Code', 'Company', 'ROE', 'PBR', '12M Return'))

# Portfolio average characteristics
cbind(quality_profit, kor_value, ret_bind)[invest_qvm, -8] %>% 
  apply(., 2, mean) %>% 
  round(3) %>% 
  t()
