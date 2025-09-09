# ============================================
# Korean Stock Market: Magic Formula Strategy
# Author: Jungbin Lee
# Description:
#   - Load preprocessed financial statement (FS) data and market value indicators
#   - Calculate key ratios: profitability, leverage, liquidity, efficiency
#   - Implement Joel Greenblatt's "Magic Formula" (EY & ROC ranking)
#   - Output top 30 candidate companies
# ============================================

library(stringr)
library(dplyr)

# -----------------------------
# Load datasets
# -----------------------------
kor_fs     <- readRDS('kor_fs.Rds')  # Financial statement data
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949") %>% select('종목명', '종목코드')
kor_value  <- read.csv('kor_value_indicator.csv', row.names = 1, stringsAsFactors = F)

# Standardize code format
kor_value <- kor_value %>%
  mutate(code = str_pad(code, 6, "left", 0)) %>%
  arrange(code)

names(kor_ticker) <- c('names', 'code')

# -----------------------------
# Helper function for FS cleanup
# -----------------------------
clean_fs <- function(df) {
  df %>%
    mutate(code = str_pad(code, 6, "left", "0")) %>%
    arrange(code)
}

# -----------------------------
# Extract and clean financial statement items
# -----------------------------
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

# -----------------------------
# Identify common stock codes across datasets
# -----------------------------
common_codes <- Reduce(intersect, list(
  net_income_attr$code, assets$code, net_income$code, cash_flow$code,
  long_term_debt$code, current_assets$code, current_debt$code,
  paid_in_capital$code, gross_profit$code, sales$code, money$code,
  debt$code, company_tax$code, interest$code, money_asset$code,
  depreciation_expense$code, real_asset$code, unreal_asset$code,
  act_asset$code, invest_asset$code
))

code_data <- assets[which(assets$code %in% common_codes),  4]

# -----------------------------
# Ratio calculation function
# -----------------------------
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

# -----------------------------
# Financial ratios
# -----------------------------
ROA     <- make_ratio(net_income_attr, assets, common_codes, code_data)              # Return on Assets
CFO     <- make_ratio(cash_flow, assets, common_codes, code_data)                   # Cash Flow / Assets
ACCRUAL <- (CFO[ ,-4] - ROA[ ,-4]) %>% mutate(code = code_data)                     # Accruals
LEV     <- make_ratio(long_term_debt, assets, common_codes, code_data)              # Leverage
LIQ     <- make_ratio(current_assets, current_debt, common_codes, code_data)        # Liquidity
OFFER   <- make_ratio(paid_in_capital, NULL, common_codes, code_data)               # Equity issuance
MARGIN  <- make_ratio(gross_profit, sales, common_codes, code_data)                 # Profit Margin
TURN    <- make_ratio(sales, assets, common_codes, code_data)                       # Asset Turnover
ROE     <- make_ratio(net_income_attr, money, common_codes, code_data)              # Return on Equity
GPA     <- make_ratio(gross_profit, assets, common_codes, code_data)                # Gross Profit / Assets

kor_value <- kor_value %>% arrange(code) %>%
  filter(code %in% c(intersect(code_data, kor_value$code)))

# -----------------------------
# Magic Formula calculation
# -----------------------------
# EBIT = Net Income + Tax + Interest
magic_ebit <- (
  net_income_attr[which(net_income_attr$code %in% common_codes),  -4] +
  company_tax[which(company_tax$code %in% common_codes),  -4] +
  interest[which(interest$code %in% common_codes),  -4]
) %>% mutate(code = code_data)

# Market Capitalization (approximation)
magic_cap <- merge(
  kor_value,
  net_income_attr[which(net_income_attr$code %in% common_codes),  ],
  by = 'code', all = TRUE
) %>% mutate(cap = per * 2022) %>% select('cap')

# Enterprise Value = Market Cap + Debt - Excess Cash
magic_debt <- debt[which(debt$code %in% common_codes),  2]
magic_excess_cash_1 <- (
  current_debt[which(current_debt$code %in% common_codes),  -4] -
  current_assets[which(current_assets$code %in% common_codes),  -4] +
  money_asset[which(money_asset$code %in% common_codes),  -4]
) %>% mutate(code = code_data)

magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 <- (money_asset[which(money_asset$code %in% common_codes),  -4] - magic_excess_cash_1[ ,-4])[2]

magic_ev  <- magic_cap + magic_debt[2] - magic_excess_cash_2
magic_ey  <- magic_ebit[2] / magic_ev   # Earnings Yield

# Invested Capital = Net Working Capital + Net Fixed Assets
magic_ic <- (
  (current_assets[which(current_assets$code %in% common_codes),  -4] -
   current_debt[which(current_debt$code %in% common_codes),  -4])
) + (
  (real_asset[which(real_asset$code %in% common_codes),  -4] +
   unreal_asset[which(unreal_asset$code %in% common_codes),  -4] +
   invest_asset[which(invest_asset$code %in% common_codes),  -4] +
   act_asset[which(act_asset$code %in% common_codes),  -4]) -
   depreciation_expense[which(depreciation_expense$code %in% common_codes),  -4]
)

magic_ic  <- magic_ic[2]
magic_roc <- magic_ebit[2] / magic_ic   # Return on Capital

# -----------------------------
# Select top 30 companies (Magic Formula)
# -----------------------------
invest_magic <- rank(rank(-magic_ey) + rank(-magic_roc)) <= 30

magic_table <- merge(kor_value[invest_magic, ], kor_ticker, by = 'code') %>%
  arrange(code) %>%
  select('names', 'code')

# Final table with EY & ROC
magic_table %>%
  mutate('이익수익률' = round(magic_ey[invest_magic, ], 4),
         '투하자본수익률' = round(magic_roc[invest_magic, ], 4))
