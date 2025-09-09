library(stringr)  # String manipulation (e.g., padding stock codes)
library(dplyr)    # Data wrangling (mutate, arrange, filter, summarise, etc.)
library(ggplot2)  # Data visualization (bar plot of GPA by PBR quantile)

# Load datasets: financial statements (RDS), ticker info, and value indicators (CSV)
kor_fs <- readRDS('kor_fs.Rds')
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949") %>% select('종목명', '종목코드')
names(kor_ticker) = c('names', 'code')
kor_value <- read.csv('kor_value_indicator.csv', row.names = 1, stringsAsFactors = F) 
kor_value <- kor_value %>% mutate(code = str_pad(kor_value$code , 6, "left", 0)) %>% arrange(code)

# Function to clean financial statement data: format stock codes and sort
clean_fs <- function(df) {
  df %>%
    mutate(code = str_pad(code, 6, "left", "0")) %>%
    arrange(code)
}

# Extract and clean key financial statement items
assets               <- clean_fs(kor_fs$자산)                  # Total Assets
net_income_attributable <- clean_fs(kor_fs$지배주주순이익)      # Net Income Attributable to Controlling Shareholders
money                <- clean_fs(kor_fs$자본)                  # Equity
net_income           <- clean_fs(kor_fs$당기순이익)            # Net Income
cash_flow            <- clean_fs(kor_fs$영업활동으로인한현금흐름) # Operating Cash Flow
long_term_debt       <- clean_fs(kor_fs$장기차입금)             # Long-Term Debt
current_assets       <- clean_fs(kor_fs$유동자산)              # Current Assets
current_debt         <- clean_fs(kor_fs$유동부채)              # Current Liabilities
paid_in_capital      <- clean_fs(kor_fs$유상증자)              # Equity Issuance
gross_profit         <- clean_fs(kor_fs$매출총이익)            # Gross Profit
sales                <- clean_fs(kor_fs$매출액)                # Sales (Revenue)
debt                 <- clean_fs(kor_fs$부채)                  # Total Liabilities
company_tax          <- clean_fs(kor_fs$법인세비용)            # Corporate Tax Expense
interest             <- clean_fs(kor_fs$이자비용)              # Interest Expense
money_asset          <- clean_fs(kor_fs$현금및현금성자산)       # Cash & Cash Equivalents
depreciation_expense <- clean_fs(kor_fs$감가상각비)            # Depreciation Expense
real_asset           <- clean_fs(kor_fs$유형자산)              # Tangible Assets
unreal_asset         <- clean_fs(kor_fs$무형자산)              # Intangible Assets
act_asset            <- clean_fs(kor_fs$기타비유동자산)        # Other Non-current Assets
invest_asset         <- clean_fs(kor_fs$장기금융자산)          # Long-term Financial Assets

# Find common stock codes across all financial statement datasets
common_codes <- Reduce(intersect, list(
  net_income_attributable$code, assets$code, net_income$code, cash_flow$code,
  long_term_debt$code, current_assets$code, current_debt$code, paid_in_capital$code,
  gross_profit$code, sales$code, money$code, debt$code, company_tax$code, interest$code,
  money_asset$code, depreciation_expense$code, real_asset$code, unreal_asset$code,
  act_asset$code, invest_asset$code
))

# Extract representative code column
code_data <- assets[which(assets$code %in% common_codes), 4]

# Generic function to calculate financial ratios
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

# Calculate key financial ratios
ROA     <- make_ratio(net_income_attributable, assets, common_codes, code_data)    # Return on Assets
CFO     <- make_ratio(cash_flow, assets, common_codes, code_data)                  # Cash Flow / Assets
ACCRUAL <- (CFO[ ,-4] - ROA[ ,-4]) %>% mutate(code = code_data)                    # Accruals
LEV     <- make_ratio(long_term_debt, assets, common_codes, code_data)             # Leverage
LIQ     <- make_ratio(current_assets, current_debt, common_codes, code_data)       # Liquidity (Current Ratio)
OFFER   <- make_ratio(paid_in_capital, NULL, common_codes, code_data)              # Equity Issuance
MARGIN  <- make_ratio(gross_profit, sales, common_codes, code_data)                # Profit Margin
TURN    <- make_ratio(sales, assets, common_codes, code_data)                      # Asset Turnover
ROE     <- make_ratio(net_income_attributable, money, common_codes, code_data)     # Return on Equity
GPA     <- make_ratio(gross_profit, assets, common_codes, code_data)               # Gross Profitability

# Match value indicator dataset with financial ratios dataset
setdiff(kor_value$code, code_data)
kor_value = kor_value[kor_value$code %in% intersect(kor_value$code, code_data), ] %>% arrange(code)

# Gross Profitability (GPA) data preparation
data_gpa <- (gross_profit[which(gross_profit$code %in% common_codes),  -4] / 
             assets[which(assets$code %in% common_codes),  -4]) %>%
             mutate(code = code_data) %>% data.frame()
data_gpa = data_gpa[data_gpa$code %in% intersect(data_gpa$code, code_data), 2]

# Price-to-Book Ratio (PBR) from value indicator data
data_pbr <- kor_value$pbr

# Spearman correlation between GPA and PBR
cbind(data_pbr, -data_gpa) %>% 
  cor(method = 'spearman', use = 'complete.obs') %>% 
  round(4) 

# Relationship between PBR quintiles and mean GPA
as.data.frame(cbind(data_gpa, data_pbr)) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>%
  dplyr::filter(!is.na(quantile_pbr)) %>%
  group_by(quantile_pbr) %>%
  summarise(mean_gpa = mean(data_gpa, na.rm = TRUE)) %>%
  ggplot(aes(x = quantile_pbr, y = mean_gpa)) + 
  geom_col() + 
  xlab('PBR') + 
  ylab('GPA')
