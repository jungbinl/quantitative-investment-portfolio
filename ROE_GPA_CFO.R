# ========================================================================
# Libraries
# ========================================================================
library(stringr)   # String manipulation (e.g., padding codes)
library(ggplot2)   # Visualization
library(dplyr)     # Data wrangling

# ========================================================================
# Load datasets
# ========================================================================
kor_fs <- readRDS('kor_fs.Rds')   # Financial statements
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949") %>%
  select('종목명', '종목코드')
names(kor_ticker) = c('names', 'code')   # Rename columns

# ========================================================================
# Helper function: Clean financial statement data
# - Pads stock codes to 6 digits
# - Sorts by stock code
# ========================================================================
clean_fs <- function(df) {
  df %>%
    mutate(code = str_pad(code, 6, "left", "0")) %>%
    arrange(code)
}

# ========================================================================
# Extract and clean financial statement variables
# ========================================================================
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

# ========================================================================
# Find common stock codes across all datasets
# Ensures alignment between different financial statement items
# ========================================================================
common_codes <- Reduce(intersect, list(
  net_income_attributable$code, assets$code, net_income$code,
  cash_flow$code, long_term_debt$code, current_assets$code,
  current_debt$code, paid_in_capital$code, gross_profit$code,
  sales$code, money$code, debt$code, company_tax$code,
  interest$code, money_asset$code, depreciation_expense$code,
  real_asset$code, unreal_asset$code, act_asset$code,
  invest_asset$code
))

# Extract aligned stock codes
code_data <- assets[which(assets$code %in% common_codes),  4]

# ========================================================================
# Helper function: Calculate financial ratios
# - If denominator_df is provided, compute ratio
# - If denominator_df is NULL, just return numerator
# - Always re-attach stock codes for consistency
# ========================================================================
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

# ========================================================================
# Calculate core financial ratios
# These ratios are used for F-Score and quality factor analysis
# ========================================================================
ROA     <- make_ratio(net_income_attributable, assets, common_codes, code_data)
CFO     <- make_ratio(cash_flow, assets, common_codes, code_data)
ACCRUAL <- (CFO[ ,-4] - ROA[ ,-4]) %>% mutate(code = code_data)
LEV     <- make_ratio(long_term_debt, assets, common_codes, code_data)
LIQ     <- make_ratio(current_assets, current_debt, common_codes, code_data)
OFFER   <- make_ratio(paid_in_capital, NULL, common_codes, code_data) # Equity issuance
MARGIN  <- make_ratio(gross_profit, sales, common_codes, code_data)
TURN    <- make_ratio(sales, assets, common_codes, code_data)
ROE     <- make_ratio(net_income_attributable, money, common_codes, code_data)
GPA     <- make_ratio(gross_profit, assets, common_codes, code_data)

# ========================================================================
# Quality Factor Construction
# Select profitability-related ratios: ROE, GPA, CFO
# Rank companies by each metric (higher = better)
# ========================================================================
quality_profit <- cbind(ROE[2], GPA[2], CFO[2]) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))
rank_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.))))

# ========================================================================
# Correlation Analysis of Quality Factors
# Visualize correlation between ROE, GPA, CFO
# ========================================================================
cor(rank_quality, use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(
    method = 'color', type = 'upper',
    addCoef.col = 'black', number.cex = 1,
    tl.cex = 0.3, tl.srt = 45, tl.col = 'black',
    col = colorRampPalette(c('blue', 'white', 'red'))(200),
    mar = c(0,0,0.5,0)
  )

# ========================================================================
# Aggregate Quality Scores
# - Compute sum of ranks across factors
# - Lower score = higher quality
# ========================================================================
rank_sum <- rank_quality %>%
  rowSums() %>%
  as.data.frame() %>%
  mutate(code = code_data)
names(rank_sum) = c('rank', 'code')

# ========================================================================
# Select Top 30 Quality Companies
# ========================================================================
invest_quality = rank_sum %>% arrange(rank)
final_code <- invest_quality[1:30, 2]

# ========================================================================
# Merge results with ticker data
# Final output: Top 30 companies by quality score
# ========================================================================
stock_name = cbind(
  rank_sum[rank_sum$code %in% final_code, ],
  kor_ticker[kor_ticker$code %in% final_code, 1],
  round(quality_profit[rank_sum$code %in% final_code, ], 4)
)
