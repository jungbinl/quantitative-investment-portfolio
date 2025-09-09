library(stringr)   # String manipulation (e.g., str_pad)
library(ggplot2)   # Visualization of distributions
library(dplyr)     # Data wrangling and transformation

# ----------------------------------------------------------
# Load datasets
# ----------------------------------------------------------
kor_fs <- readRDS('kor_fs.Rds')   # Financial statements (balance sheet, income statement, cash flow)
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949") %>%
  select('종목명', '종목코드')
names(kor_ticker) = c('names', 'code')   # Rename columns for consistency

# ----------------------------------------------------------
# Helper function to clean financial statement items
# - Pad stock codes to 6 digits
# - Sort by stock code
# ----------------------------------------------------------
clean_fs <- function(df) {
  df %>%
    mutate(code = str_pad(code, 6, "left", "0")) %>%
    arrange(code)
}

# ----------------------------------------------------------
# Extract and clean key financial statement variables
# ----------------------------------------------------------
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

# ----------------------------------------------------------
# Find common stock codes across all data frames
# ----------------------------------------------------------
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
code_data <- assets[which(assets$code %in% common_codes), 4]

# ----------------------------------------------------------
# Helper function to calculate financial ratios
# ----------------------------------------------------------
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

# ----------------------------------------------------------
# Construct core financial ratios for F-Score components
# ----------------------------------------------------------
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

# ----------------------------------------------------------
# Piotroski F-Score: 9 binary signals
# ----------------------------------------------------------
F_1 = as.integer(ROA[ ,2] > 0)                          # Positive ROA
F_2 = as.integer(CFO[ ,2] > 0)                          # Positive CFO
F_3 = as.integer(ROA[ ,2] - ROA[ ,1] > 0)               # ROA improvement
F_4 = as.integer(ACCRUAL[ ,1] > 0)                      # CFO > ROA (earnings quality)
F_5 = as.integer(LEV[ ,2] - LEV[ ,1] <= 0)              # Lower leverage
F_6 = as.integer(LIQ[ ,2] - LIQ[ ,1] > 0)               # Better liquidity
F_7 = as.integer(is.na(OFFER[ ,2]) | OFFER[ ,2] <= 0)   # No equity issuance
F_8 = as.integer(MARGIN[ ,2] - MARGIN[ ,1] > 0)         # Margin improvement
F_9 = as.integer(TURN[ ,2] - TURN[ ,1] > 0)             # Asset turnover improvement

# Combine all F-Score signals into a matrix
F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9)

# Final F-Score per company (sum of all signals)
F_Score = F_Table %>% apply(., 1, sum, na.rm = T) %>% setNames(code_data)

# ----------------------------------------------------------
# Distribution of F-Scores
# ----------------------------------------------------------
F_dist = prop.table(table(F_Score)) %>% round(3) %>% data.frame()

# Visualize F-Score distribution
ggplot(F_dist, aes(x = F_Score, y = Freq, label = paste0(Freq * 100, '%'))) +
  geom_bar(stat = 'identity') +
  geom_text(color = 'black', size = 3, vjust = -0.4) +
  scale_y_continuous(expand = c(0,0,0,0.05), label = scales::percent) +
  ylab(NULL) +
  theme_classic()

# ----------------------------------------------------------
# Select companies with perfect F-Score = 9
# ----------------------------------------------------------
invest_F_Score = F_Score %in% c(9)
table(invest_F_Score)
kor_ticker[invest_F_Score, ] %>% mutate('F_Score' = 9)
