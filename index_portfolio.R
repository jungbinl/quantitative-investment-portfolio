library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# (1) Load datasets
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")
names(kor_ticker) <- c('name', 'end_price', 'compare', 'fluctuation rate', 'code', 'EPS', 'pre_PER', 'pre_EPS', 'PER',  'BPS', 'PBR', 'dividend', 'dividend_rate', 'market', 'industry', 'company_price')
kor_value <- read.csv('kor_value_indicator.csv', row.names = 1, stringsAsFactors = F) 

# Standardize stock codes to 6 digits and sort
kor_value <- kor_value %>% 
  mutate(code = str_pad(kor_value$code , 6, "left", 0)) %>% 
  arrange(code)

# (2) Select KOSPI200 stocks and calculate market-cap weights
kospi_200 = kor_ticker[kor_ticker$market== 'KOSPI', ] %>% 
  slice(1:200) %>% 
  mutate(company_price_rate = company_price / sum(company_price))

# (3) Visualize market-cap weights
kospi_200 %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = company_price_rate)) + 
  geom_point() + 
  xlab('Stock') + 
  ylab('Market Cap Weight') + 
  scale_y_continuous(labels = scales::percent)

# Visualize with log scale + display only every 5th label
kospi_200 %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = company_price_rate)) + 
  geom_point() + 
  xlab('Stock') + 
  ylab('Market Cap Weight (Log Scale)') + 
  scale_y_log10() + 
  scale_x_discrete(breaks = kospi_200[seq(1,200,by = 5), 'name']) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (4) Calculate investment amount and shares purchased
kospi_200 = kospi_200 %>% 
  mutate(buy_price = 100000000*company_price_rate,   # Assume 100M KRW invested
         buy_price = buy_price / end_price) %>% 
  mutate(buy_price = floor(buy_price))          # Round down to integer shares

# Check the actual invested amount
inv_money = kospi_200 %>% 
  mutate(buy_price_real = end_price * buy_price) %>% 
  summarise(sum(buy_price_real))
print(inv_money)

# (5) Adjust weights based on PBR (Price-to-Book Ratio)
kospi_200 <- kospi_200 %>% mutate(PBR = as.numeric(PBR))

kospi_200 <- kospi_200 %>% 
  mutate(rank = rank(PBR), 
         control_ratio = ifelse(rank <= 100, company_price_rate + 0.0005, company_price_rate - 0.0005),
         control_ratio = ifelse(control_ratio < 0, 0, control_ratio), 
         control_ratio = control_ratio / sum(control_ratio), 
         diff = control_ratio - company_price_rate)

# Compare original weights (black) vs adjusted weights (red)
kospi_200 %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = company_price_rate)) + 
  geom_point() + 
  geom_point(data = kospi_200, aes(x = reorder(name, -company_price_rate), y = control_ratio), 
             color = 'red', shape = 4) + 
  xlab('Stock') + 
  ylab('Weight (%)') + 
  coord_cartesian(ylim = c(0, 0.03)) + 
  scale_x_discrete(breaks = kospi_200[seq(1, 200, by = 5), 'name']) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (6) Visualize diff vs PBR
kospi_200_mod <- kospi_200 %>% arrange(code)

kospi_200_mod %>% 
  ggplot(aes(x = reorder(name, PBR), y = diff)) + 
  geom_point() + 
  geom_col(aes(x = reorder(name, PBR), y = PBR/10000), fill = 'blue', alpha = 0.2) + 
  xlab('Stock') + 
  ylab('Difference (%)') + 
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 10000, name = 'PBR')) + 
  scale_x_discrete(breaks = kospi_200_mod[seq(1,200,by=5),'name']) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (7) Tilt strategy: Adjust weights using Z-score and CDF
kospi_200_tilt = kospi_200 %>% 
  mutate(zscore = -scale(rank), 
         cdf = pnorm(zscore), 
         invest_ratio = company_price_rate * cdf, 
         invest_ratio = invest_ratio / sum(invest_ratio), 
         diff = invest_ratio - company_price_rate)

# Compare original vs tilted weights
kospi_200_tilt %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = company_price_rate)) + 
  geom_point() + 
  geom_point(data = kospi_200_tilt, aes(x = reorder(name, -company_price_rate), y = invest_ratio), 
             color = 'red', shape = 4) + 
  xlab('Stock') + 
  ylab('Difference (%)') + 
  coord_cartesian(ylim = c(0, 0.03)) + 
  scale_x_discrete(breaks = kospi_200[seq(1, 200, by = 5), 'name']) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (8) Check deviation vs ±0.5% tolerance
kospi_200_tilt %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = diff)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') + 
  xlab('Stock') + 
  ylab('Difference (%)')  + 
  scale_x_discrete(breaks = kospi_200[seq(1, 200, by = 5), 'name']) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (9) Iteratively adjust until all diffs are within ±0.5%
while(max(abs(kospi_200_tilt$diff)) > (0.005+0.00001)) {
  kospi_200_tilt = kospi_200_tilt %>% 
    mutate_at(vars(invest_ratio), list(~ifelse(diff < -0.005, company_price_rate - 0.005, diff))) %>% 
    mutate_at(vars(invest_ratio), list(~ifelse(diff < -0.005, company_price_rate + 0.005, diff))) %>% 
    mutate(invest_ratio = invest_ratio / sum(invest_ratio), 
           diff = invest_ratio - company_price_rate)
}

# (10) Final diff visualization
kospi_200_tilt %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = diff)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') + 
  xlab('Stock') + 
  ylab('Difference (%)')  + 
  scale_x_discrete(breaks = kospi_200[seq(1, 200, by = 5), 'name']) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (11) Compare original vs tilt-adjusted differences
kospi_200 %>% 
  ggplot(aes(x = reorder(name, -company_price_rate), y = company_price_rate)) + 
  geom_point() + 
  geom_point(data = kospi_200_tilt, aes(x = reorder(name, -company_price_rate), y = diff), 
             color = 'red', shape = 4) + 
  xlab('Stock') + 
  ylab('Weight (%)') + 
  coord_cartesian(ylim = c(0, 0.03)) + 
  scale_x_discrete(breaks = kospi_200[seq(1, 200, by = 5), 'name']) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# (12) Sort by PBR and compare diff vs PBR
kospi_200_tilt_mod <- kospi_200_tilt %>% arrange(PBR)

kospi_200_tilt_mod %>% 
  ggplot(aes(x = reorder(name, PBR), y = diff)) + 
  geom_point() + 
  geom_col(aes(x = reorder(name, PBR), y = PBR/2000), fill = 'blue', alpha = 0.2) + 
  xlab('Stock') + 
  ylab('Difference (%)') + 
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 2000, name = 'PBR')) + 
  scale_x_discrete(breaks = kospi_200_mod[seq(1,200,by=5),'name']) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
