library(stringr)   # String formatting (e.g., padding stock codes)
library(dplyr)     # Data manipulation (filter, mutate, select, etc.)
library(corrplot)  # Correlation matrix visualization

# Load valuation indicators (PBR, PER, PCR, PSR) and filter out invalid values (negative numbers)
kor_value <- read.csv('kor_value_indicator.csv', row.names = 1, stringsAsFactors = F)
kor_value <- kor_value %>% filter(pbr >= 0 & per >=0 & pcr >= 0 & psr >=0)

# Load ticker dataset (company names and codes)
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = 'CP949')

# --------------------------------------------
# Step 1: Identify Low PBR (Price-to-Book Ratio) Stocks
# --------------------------------------------
kor_value$code = str_pad(kor_value$code, 6, 'left', 0)   # format stock codes (6 digits with leading zeros)

# Select top 30 stocks ranked by lowest PBR
invest_pbr = rank(kor_value$pbr) <= 30
low_pbr <- merge(
  kor_value[invest_pbr, ],
  kor_ticker,
  by.x = 'code', by.y = '종목코드'
) %>% select('code', '종목명', 'pbr')

# --------------------------------------------
# Step 2: Create ranking for each valuation metric
# --------------------------------------------
# min_rank() is applied to each column (PBR, PER, PCR, PSR) to get relative ranks
rank_value <- kor_value %>% mutate_all(list(~min_rank(.)))

# --------------------------------------------
# Step 3: Correlation analysis of valuation indicators
# --------------------------------------------
# Compute correlation matrix and visualize using corrplot
cor(rank_value %>% select(!code), use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(
    method = 'color', type = 'upper',
    addCoef.col = 'black', number.cex = 1,
    tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
    col = colorRampPalette(c('blue', 'white', 'red'))(200),
    mar = c(0,0,0.5,0)
  )

# --------------------------------------------
# Step 4: Composite Ranking
# --------------------------------------------
# Calculate sum of ranks across all metrics (PBR, PER, PCR, PSR)
rank_sum = rank_value %>% mutate(rank_sum = rank_value %>% select(!code) %>% rowSums())

# Select top 30 stocks with the lowest overall rank sum
top_index <- order(rank_sum$rank_sum, decreasing = F)[1 :30]

# Merge with ticker info to display company names
merge_indicator <- merge(
  kor_value[top_index, ],
  kor_ticker,
  by.x = 'code', by.y = '종목코드'
) %>% select('code', '종목명', 'pbr', 'pcr', 'psr', 'per')

# --------------------------------------------
# Step 5: Compare overlap between Low-PBR strategy and Composite Ranking strategy
# --------------------------------------------
intersect(merge_indicator$종목명, low_pbr$종목명)
