###套件們###
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(readr)
library(rugarch)
library(data.table)

setwd("D:/NSYSU FIN/加權")

price_data <- fread("2024adj.txt")          # 上市股票數據
index_data <- fread("y9997.txt")     # 加權指數數據
fin_data <- fread("指數.txt") # 指數因子
industry_data <- fread("industry.txt") 

colnames(price_data) <- c("Stock_Code", "Date", "Close_Price", "Volume", "Outstanding_Shares","PE","turnover")
colnames(index_data) <- c("Stock_Code", "Date", "Close_Price", "Volume", "Outstanding_Shares")
colnames(fin_data) <- c("index", "Date", "Stock_Code","Float_Coefficient")
colnames(industry_data) <- c("Stock_Code", "Industry") 

Encoding(price_data$Stock_Code)

# 將資料轉換為 UTF-8 編碼
price_data$Stock_Code <- iconv(price_data$Stock_Code, from = "BIG5", to = "UTF-8")
index_data$Stock_Code <- iconv(index_data$Stock_Code, from = "BIG5", to = "UTF-8")
fin_data$index <- iconv(fin_data$index, from = "BIG5", to = "UTF-8")
fin_data$Stock_Code <- iconv(fin_data$Stock_Code, from = "BIG5", to = "UTF-8")
industry_data$Industry<-iconv(industry_data$Industry, from = "BIG5", to = "UTF-8")

# ########刪除中文############

price_data$Stock_Code <- gsub("[^0-9]", "", price_data$Stock_Code)
index_data$Stock_Code <- gsub("[^0-9]", "", index_data$Stock_Code)
fin_data$index <- gsub("[^0-9]", "", fin_data$index)
fin_data$Stock_Code <- gsub("[^0-9]", "", fin_data$Stock_Code)

#######確定日期格式###########

price_data$Date <- as.Date(as.character(price_data$Date), format = "%Y%m%d")
index_data$Date <- as.Date(as.character(index_data$Date), format = "%Y%m%d")
fin_data$Date <- as.Date(as.character(fin_data$Date), format = "%Y%m%d")

print(head(price_data$turnover))

print(head(price_data))

# 計算 Amihud 流動性指標，越大流動性越差
price_data <- price_data %>%
  group_by(Stock_Code) %>%
  arrange(Stock_Code, Date) %>%
  mutate(
    Stock_Return = (Close_Price - lag(Close_Price)) / lag(Close_Price),
    Amihud_Liquidity = abs(Stock_Return) / Volume  
  ) %>%
  ungroup()

# 删除包含任何 NaN 或 NA 的行
price_data <- price_data %>%
  filter(!is.na(Stock_Return) & !is.na(Amihud_Liquidity))
# 合併 tr_data 與 price_data
price_data <- price_data %>%
  left_join(fin_data, by = c("Stock_Code", "Date"))


# 檢查合併後的結果
print(head(price_data))

#### 計算波動因子、動能因子與移動平均線因子 ####

price_data <- price_data %>%
  group_by(Stock_Code) %>%
  filter(n() >= 63) %>%
  filter(n() >= 126)%>%
  filter(n() >= 252)%>%
  ungroup()

# 計算波動率、動能與移動平均線
price_data <- price_data %>%
  group_by(Stock_Code) %>%
  mutate(
    # 計算波動率
    Volatility_63 = rollapply(Stock_Return, width = 63, FUN = sd, fill = NA, align = "right"),
    Volatility_126 = rollapply(Stock_Return, width = 126, FUN = sd, fill = NA, align = "right"),
    Volatility_252 = rollapply(Stock_Return, width = 252, FUN = sd, fill = NA, align = "right"),
    
    # 計算動能
    Momentum_63 = rollapply(Stock_Return + 1, width = 63, FUN = prod, fill = NA, align = "right") - 1,
    Momentum_126 = rollapply(Stock_Return + 1, width = 126, FUN = prod, fill = NA, align = "right") - 1,
    Momentum_252 = rollapply(Stock_Return + 1, width = 252, FUN = prod, fill = NA, align = "right") - 1,
    
    # 計算移動平均線
    MA_63 = SMA(Close_Price, n = 63),
    MA_126 = SMA(Close_Price, n = 126),
    MA_252 = SMA(Close_Price, n = 252)
  ) %>%
  ungroup()

####計算大盤報酬率##########
index_data <- index_data %>% arrange(Date)
unique_index_data <- index_data %>%
  select(Date, Close_Price) %>%  
  distinct() %>%               
  arrange(Date)
# 計算 Lag_Close_Price 和 Index_Return
unique_index_data$Lag_Close_Price <- c(NA, unique_index_data$Close_Price[-nrow(unique_index_data)])
# 計算大盤報酬率
unique_index_data <- unique_index_data %>%
  mutate(Index_Return = (Close_Price - Lag_Close_Price) / Lag_Close_Price)
# 大盤收益率數據
index_returns <- unique_index_data %>%
  select(Date, Index_Return) %>%
  filter(!is.na(Index_Return)) 


########獲取每季的再平衡日期###########
get_quarterly_rebalance_dates <- function(price_data) {
  price_data %>%
    mutate(Quarter = paste0(year(Date), "-", quarter(Date))) %>%
    group_by(Quarter) %>%
    summarise(Rebalance_Date = min(Date)) %>%
    pull(Rebalance_Date)
}
quarterly_rebalance_dates <- get_quarterly_rebalance_dates(price_data)


####權重調整######
calculate_quarterly_portfolio <- function(price_data, rebalance_dates, stock_count, industry_data) {
  # 初始化投資組合數據
  portfolio <- list()
  for (i in seq_along(rebalance_dates)) {
    rebalance_date <- rebalance_dates[i]
    
    # 在再平衡日期選取市值排名前 stock_count 的股票
    current_portfolio <- price_data %>%
      filter(Date == rebalance_date) %>%
      mutate(Market_Value = Close_Price * Outstanding_Shares) %>%
      arrange(desc(Market_Value)) %>%
      slice(1:stock_count) %>%
      left_join(industry_data, by = "Stock_Code") %>%  
      group_by(Industry) %>%
      mutate(
        Industry_Total_Market_Value = sum(Market_Value, na.rm = TRUE),  # 計算總市值
        Industry_Proportion = Industry_Total_Market_Value / sum(Market_Value, na.rm = TRUE)  # 計算產業市值比例
      ) %>%
      ungroup() %>%
      mutate(
        # 減少半導體產業比例，並將減少的部分重新分配給其他產業
        Industry_Adjustment = ifelse(
          Industry == "M2324 半導體",
          Industry_Proportion * 0.9,  
          Industry_Proportion + sum(Industry_Proportion[Industry == "M2324 半導體"] * 0.1, na.rm = TRUE) / 
            (n_distinct(Industry) - 1)  
        ),
        Adjusted_Proportion = Industry_Adjustment / sum(Industry_Adjustment, na.rm = TRUE),  # 調整後的比例
        Fixed_Shares = Outstanding_Shares
      ) %>%
      select(Stock_Code, Fixed_Shares, Industry, Adjusted_Proportion)
    
    portfolio[[as.character(rebalance_date)]] <- current_portfolio
  }
  
  # 計算投資組合的每日收益率
  portfolio_returns <- data.frame(Date = unique(price_data$Date), Portfolio_Return = NA)
  
  for (i in seq_along(portfolio_returns$Date)) {
    current_date <- portfolio_returns$Date[i]
    
    # 確定所屬的再平衡期間
    rebalance_period <- max(rebalance_dates[rebalance_dates <= current_date], na.rm = TRUE)
    
    # 獲取當前的投資組合
    current_portfolio <- portfolio[[as.character(rebalance_period)]]
    
    # 計算當日權重和收益率
    daily_data <- price_data %>%
      filter(Date == current_date) %>%
      select(Stock_Code, Close_Price, Stock_Return)
    
    if (nrow(daily_data) > 0) {
      combined_portfolio <- current_portfolio %>%
        left_join(daily_data, by = "Stock_Code") %>%
        mutate(
          Value = Fixed_Shares * Close_Price,
          Adjusted_Value = Value * Adjusted_Proportion,  # 根據產業調整比例進行調整
          Weight = Adjusted_Value / sum(Adjusted_Value, na.rm = TRUE)
        )
      
      
      portfolio_returns$Portfolio_Return[i] <- sum(combined_portfolio$Weight * combined_portfolio$Stock_Return, na.rm = TRUE)
    }
  }
  
  return(portfolio_returns)
}
##PE + INDUSTRY + MOM + tr
rebalance_dates <- quarterly_rebalance_dates
calculate_quarterly_portfolio_with_factors <- function(price_data, rebalance_dates, stock_count, industry_data) {
  portfolio <- list()
  
  for (i in seq_along(rebalance_dates)) {
    rebalance_date <- rebalance_dates[i]
    
    # 選取市值排名前 stock_count 的股票
    current_portfolio <- price_data %>%
      filter(Date == rebalance_date) %>%
      mutate(Market_Value = Close_Price * Outstanding_Shares) %>%
      arrange(desc(Market_Value)) %>%
      slice(1:stock_count) %>%
      left_join(industry_data, by = "Stock_Code") %>%
      group_by(Industry) %>%
      mutate(
        Industry_Total_Market_Value = sum(Market_Value, na.rm = TRUE),
        Industry_Proportion = Industry_Total_Market_Value / sum(Market_Value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # 增加半導體比例，並等比例調整其他產業
    adjustment_needed <- 0.05  
    current_portfolio <- current_portfolio %>%
      mutate(
        Adjusted_Proportion = case_when(
          Industry == "M2324 半導體" ~ Industry_Proportion + adjustment_needed,
          TRUE ~ Industry_Proportion - (adjustment_needed / (n_distinct(Industry) - 1))
        ),
        Adjusted_Proportion = pmax(Adjusted_Proportion, 0),  
        Adjusted_Proportion = Adjusted_Proportion / sum(Adjusted_Proportion, na.rm = TRUE)  
      )
    
    # 添加因子調整
    current_portfolio <- current_portfolio %>%
      mutate(
        PE_Rank = rank(desc(PE), ties.method = "average"),
        MOM_Rank = rank(desc(Momentum_63), ties.method = "average"),
        Turnover_Rank = rank(desc(turnover), ties.method = "average")
      ) %>%
      mutate(
        PE_Adjustment = case_when(
          PE_Rank <= stock_count / 5 ~ 0.001,  # 前 20%（高 PE）
          PE_Rank > stock_count * 4 / 5 ~ -0.001,  # 後 20%（低 PE）
          TRUE ~ 0
        ),
        MOM_Adjustment = case_when(
          MOM_Rank <= stock_count / 5 ~ 0.001,  # 前 20%（高 MOM）
          MOM_Rank > stock_count * 4 / 5 ~ -0.001,  # 後 20%（低 MOM）
          TRUE ~ 0
        ),
        Turnover_Adjustment = case_when(
          Turnover_Rank <= stock_count / 5 ~ 0.001,  # 前 20%（高 Turnover）
          Turnover_Rank > stock_count * 4 / 5 ~ -0.001,  # 後 20%（低 Turnover）
          TRUE ~ 0
        )
      ) %>%
      mutate(
        Adjusted_Proportion = Adjusted_Proportion + PE_Adjustment + MOM_Adjustment + Turnover_Adjustment,
        Adjusted_Proportion = pmax(Adjusted_Proportion, 0), 
        Adjusted_Proportion = Adjusted_Proportion / sum(Adjusted_Proportion, na.rm = TRUE)  
      ) %>%
      select(Stock_Code, Fixed_Shares = Outstanding_Shares, Industry, Adjusted_Proportion)
    
    portfolio[[as.character(rebalance_date)]] <- current_portfolio
  }
  
  # 計算投資組合的每日收益率
  portfolio_returns <- data.frame(Date = unique(price_data$Date), Portfolio_Return = NA)
  
  for (i in seq_along(portfolio_returns$Date)) {
    current_date <- portfolio_returns$Date[i]
    rebalance_period <- max(rebalance_dates[rebalance_dates <= current_date], na.rm = TRUE)
    current_portfolio <- portfolio[[as.character(rebalance_period)]]
    
    daily_data <- price_data %>%
      filter(Date == current_date) %>%
      select(Stock_Code, Close_Price, Stock_Return)
    
    if (!is.null(current_portfolio) && nrow(daily_data) > 0) {
      combined_portfolio <- current_portfolio %>%
        left_join(daily_data, by = "Stock_Code") %>%
        mutate(
          Value = Fixed_Shares * Close_Price,
          Adjusted_Value = Value * Adjusted_Proportion,
          Weight = Adjusted_Value / sum(Adjusted_Value, na.rm = TRUE)
        )
      
      portfolio_returns$Portfolio_Return[i] <- sum(combined_portfolio$Weight * combined_portfolio$Stock_Return, na.rm = TRUE)
    }
  }
  
  return(portfolio_returns)
}
result <- calculate_quarterly_portfolio_with_factors(
  price_data = price_data,
  rebalance_dates = rebalance_dates,
  stock_count = 400,
  industry_data = industry_data
)

######大盤報酬#########
filter_date_range <- function(data, start_date, end_date) {
  data %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
}
# 篩選2020-01-01~2022-12-31

index_returns_2020_2022 <- filter_date_range(index_returns, "2020-01-01", "2022-12-31"
)%>%rename(Portfolio_Return = Index_Return)

# 篩選2021-01-01~2023-12-31

index_returns_2021_2023 <- filter_date_range(index_returns, "2021-01-01", "2023-12-31"
)%>%rename(Portfolio_Return = Index_Return)

# 確保 Stock_Code 為相同類型

price_data$Stock_Code <- as.character(price_data$Stock_Code)
industry_data$Stock_Code <- as.character(industry_data$Stock_Code)

#####實際#######

# 定義再平衡日期和股票數量
rebalance_dates <- quarterly_rebalance_dates
stock_count <- 400


price_data$Stock_Code <- as.character(price_data$Stock_Code)
industry_data$Stock_Code <- as.character(industry_data$Stock_Code)

###函式######
calculate_cumulative_return <- function(portfolio_returns) {
  portfolio_returns %>%
    arrange(Date) %>%
    mutate(Cumulative_Return = cumprod(1 + Portfolio_Return) - 1) %>%
    filter(!is.na(Cumulative_Return))
}
calculate_diff <- function(portfolio_1, portfolio_2) {
  
  merged_data <- portfolio_1 %>%
    rename(Portfolio_Return_1 = Portfolio_Return) %>%
    left_join(
      portfolio_2 %>% rename(Portfolio_Return_2 = Portfolio_Return),
      by = "Date"
    )
  
  # 計算每日報酬的差異
  merged_data <- merged_data %>%
    mutate(Diff_Return = Portfolio_Return_1 - Portfolio_Return_2) %>%
    filter(!is.na(Diff_Return))
  
  return(merged_data)
}
calculate_annualized_return <- function(cumulative_return, years) {
  if (length(cumulative_return) == 0 || is.na(last(cumulative_return))) {
    stop("累積報酬資料缺失或不正確")
  }
  
  # 計算年化報酬率
  annualized_return <- (last(cumulative_return) + 1)^(1 / years) - 1
  return(annualized_return)
}
calculate_tracking_error <- function(diff_return, trading_days) {
  if (length(diff_return) == 0 || all(is.na(diff_return))) {
    stop("差異回報資料缺失或不正確")
  }
  
  # 計算追蹤誤差 (TE)
  tracking_error <- sd(diff_return, na.rm = TRUE) * sqrt(trading_days)
  return(tracking_error)
}
calculate_information_ratio <- function(active_return, tracking_error) {
  if (tracking_error == 0) {
    stop("追蹤誤差為零，無法計算資訊比率")
  }
  
  # 計算資訊比率 (IR)
  information_ratio <- active_return / tracking_error
  return(information_ratio)
}
calculate_portfolio_for_stock_count <- function(stock_count, start_date, end_date) {
  # 計算因子投組
  factor_returns <- calculate_quarterly_portfolio_with_factors(
    price_data = price_data,
    rebalance_dates = quarterly_rebalance_dates,
    stock_count = stock_count,
    industry_data = industry_data
  ) %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  
  # 計算基準投組
  baseline_returns <- calculate_quarterly_portfolio(
    price_data = price_data,
    rebalance_dates = quarterly_rebalance_dates,
    stock_count = stock_count,
    industry_data = industry_data
  ) %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  
  list(
    factor_returns = factor_returns,
    baseline_returns = baseline_returns
  )
}

####計算每家公司#######
# 計算400家公司投組
results_400 <- calculate_portfolio_for_stock_count(
  stock_count = 400,
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)

factor_returns_400 <- results_400$factor_returns
baseline_returns_400 <- results_400$baseline_returns

# 計算350家公司投組
results_350 <- calculate_portfolio_for_stock_count(
  stock_count = 350,
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)

factor_returns_350 <- results_350$factor_returns
baseline_returns_350 <- results_350$baseline_returns

# 計算250家公司投組
results_250 <- calculate_portfolio_for_stock_count(
  stock_count = 250,
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)

factor_returns_250 <- results_250$factor_returns
baseline_returns_250 <- results_250$baseline_returns


######大盤報酬#########
filter_date_range <- function(data, start_date, end_date) {
  data %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
}

index_returns_2020_2022 <- filter_date_range(index_returns, "2020-01-01", "2022-12-31"
)%>%rename(Portfolio_Return = Index_Return)

index_returns_2021_2023 <- filter_date_range(index_returns, "2021-01-01", "2023-12-31"
)%>%rename(Portfolio_Return = Index_Return)

####函式##########
calculate_metrics <- function(new_factor_returns, old_factor_returns, index_returns, start_date, end_date) {
  
  new_factor_returns <- new_factor_returns %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  old_factor_returns <- old_factor_returns %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  index_returns <- index_returns %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  
  
  new_factor_cumulative <- calculate_cumulative_return(new_factor_returns)
  old_factor_cumulative <- calculate_cumulative_return(old_factor_returns)
  index_cumulative <- calculate_cumulative_return(index_returns)
  
  
  years <- as.numeric(difftime(as.Date(end_date), as.Date(start_date), units = "days")) / 365
  new_factor_annualized <- calculate_annualized_return(new_factor_cumulative$Cumulative_Return, years)
  old_factor_annualized <- calculate_annualized_return(old_factor_cumulative$Cumulative_Return, years)
  index_annualized <- calculate_annualized_return(index_cumulative$Cumulative_Return, years)
  
  # 主動報酬 (新的 - 舊的)
  active_error <- new_factor_annualized - old_factor_annualized
  
  # 計算每日報酬差異 (新的 vs 大盤)
  new_diff_returns <- calculate_diff(new_factor_returns, index_returns)
  
  # 計算追蹤誤差 (新的與大盤)
  trading_days <- nrow(index_returns) / years
  new_vs_index_tracking_error <- calculate_tracking_error(new_diff_returns$Diff_Return, trading_days)
  
  # 計算 IR
  ir <- calculate_information_ratio(active_error, new_vs_index_tracking_error)
  
  list(
    New_Factor_Annualized = new_factor_annualized,
    Old_Factor_Annualized = old_factor_annualized,
    Index_Annualized = index_annualized,
    Active_Error = active_error,
    New_vs_Index_Tracking_Error = new_vs_index_tracking_error,
    IR = ir
  )
}

# 範例計算
metrics_400 <- calculate_metrics(
  new_factor_returns = factor_returns_400,  # 新的因子
  old_factor_returns = baseline_returns_400,  # 舊的因子
  index_returns = index_returns_2021_2023,  # 大盤
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)


metrics_350 <- calculate_metrics(
  new_factor_returns = factor_returns_350,  
  old_factor_returns = baseline_returns_350,  
  index_returns = index_returns_2021_2023,  
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)

#250

metrics_250 <- calculate_metrics(
  new_factor_returns = factor_returns_250,  
  old_factor_returns = baseline_returns_250,  
  index_returns = index_returns_2021_2023,  
  start_date = "2021-01-01",
  end_date = "2023-12-31"
)

# 打印結果
print(metrics_400)
print(metrics_350)
print(metrics_250)

####400家畫圖###########
# 計算因子累積報酬率並加 1
factor_cumulative <- calculate_cumulative_return(factor_returns_400) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
baseline_cumulative <- calculate_cumulative_return(baseline_returns_400) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
index_cumulative <- calculate_cumulative_return(index_returns_2021_2023) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)

# 整合資料
cumulative_data <- rbind(
  factor_cumulative %>% mutate(Type = "Factor Portfolio"),
  baseline_cumulative %>% mutate(Type = "Baseline Portfolio"),
  index_cumulative %>% mutate(Type = "Market Index") # 加入大盤
)

# 繪製累積報酬率圖
ggplot(cumulative_data, aes(x = Date, y = Cumulative_Return, color = Type)) +
  geom_line(size = 1) +
  labs(
    title = "2021-2023累積報酬率 400家 (因子：產業 & MOM & PE & turnover)",
    x = "日期",
    y = "累積報酬率",
    color = "策略類型"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  annotate(
    "text",
    x = max(cumulative_data$Date) - 900,  
    y = max(cumulative_data$Cumulative_Return, na.rm = TRUE) * 1,  
    label = paste0(
      "年化主動報酬 (Alpha): ", round(metrics_400$Active_Error * 100, 2), "%\n",
      "年化追蹤誤差 (Factor vs Index): ", round(metrics_400$New_vs_Index_Tracking_Error * 100, 2), "%\n",
      "資訊比率 (IR): ", round(metrics_400$IR, 2)
    ),
    hjust = 0, vjust = 1, size = 4, color = "black"
  )

#### 350 家畫圖 ###########

factor_cumulative <- calculate_cumulative_return(factor_returns_350) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
baseline_cumulative <- calculate_cumulative_return(baseline_returns_350) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
index_cumulative <- calculate_cumulative_return(index_returns_2021_2023) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)


cumulative_data <- rbind(
  factor_cumulative %>% mutate(Type = "Factor Portfolio"),
  baseline_cumulative %>% mutate(Type = "Baseline Portfolio"),
  index_cumulative %>% mutate(Type = "Market Index")
)


ggplot(cumulative_data, aes(x = Date, y = Cumulative_Return, color = Type)) +
  geom_line(size = 1) +
  labs(
    title = "2021-2023累積報酬率 350家 (因子：產業 & MOM & PE & turnover)",
    x = "日期",
    y = "累積報酬率",
    color = "策略類型"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  annotate(
    "text",
    x = max(cumulative_data$Date) - 900,
    y = max(cumulative_data$Cumulative_Return, na.rm = TRUE) * 1,
    label = paste0(
      "年化主動報酬 (Alpha): ", round(metrics_350$Active_Error * 100, 2), "%\n",
      "年化追蹤誤差 (Factor vs Index): ", round(metrics_350$New_vs_Index_Tracking_Error * 100, 2), "%\n",
      "資訊比率 (IR): ", round(metrics_350$IR, 2)
    ),
    hjust = 0, vjust = 1, size = 4, color = "black"
  )

#### 250 家畫圖 ###########

factor_cumulative <- calculate_cumulative_return(factor_returns_250) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
baseline_cumulative <- calculate_cumulative_return(baseline_returns_250) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)
index_cumulative <- calculate_cumulative_return(index_returns_2021_2023) %>% 
  mutate(Cumulative_Return = Cumulative_Return + 1)

cumulative_data <- rbind(
  factor_cumulative %>% mutate(Type = "Factor Portfolio"),
  baseline_cumulative %>% mutate(Type = "Baseline Portfolio"),
  index_cumulative %>% mutate(Type = "Market Index")
)

ggplot(cumulative_data, aes(x = Date, y = Cumulative_Return, color = Type)) +
  geom_line(size = 1) +
  labs(
    title = "2021-2023累積報酬率 250家 (因子：產業 & MOM & PE & turnover)",
    x = "日期",
    y = "累積報酬率",
    color = "策略類型"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  annotate(
    "text",
    x = max(cumulative_data$Date) - 900,
    y = max(cumulative_data$Cumulative_Return, na.rm = TRUE) * 1,
    label = paste0(
      "年化主動報酬 (Alpha): ", round(metrics_250$Active_Error * 100, 2), "%\n",
      "年化追蹤誤差 (Factor vs Index): ", round(metrics_250$New_vs_Index_Tracking_Error * 100, 2), "%\n",
      "資訊比率 (IR): ", round(metrics_250$IR, 2)
    ),
    hjust = 0, vjust = 1, size = 4, color = "black"
  )




#########TOTAL#############
# 計算累積報酬率 
calculate_cumulative_return <- function(portfolio_returns) {
  if (!"Portfolio_Return" %in% colnames(portfolio_returns)) {
    stop("資料框中沒有欄位 'Portfolio_Return'")
  }
  portfolio_returns %>%
    arrange(Date) %>%
    mutate(Cumulative_Return = cumprod(1 + Portfolio_Return)) 
}

# 計算各投資組合和大盤的累積報酬率
factor_cumulative_400 <- calculate_cumulative_return(factor_returns_400) %>%
  mutate(Type = "Factor Portfolio (400)")
baseline_cumulative_400 <- calculate_cumulative_return(baseline_returns_400) %>%
  mutate(Type = "Baseline Portfolio (400)")
index_cumulative <- calculate_cumulative_return(index_returns_2021_2023) %>%
  mutate(Type = "Market Index")

factor_cumulative_350 <- calculate_cumulative_return(factor_returns_350) %>%
  mutate(Type = "Factor Portfolio (350)")
baseline_cumulative_350 <- calculate_cumulative_return(baseline_returns_350) %>%
  mutate(Type = "Baseline Portfolio (350)")

factor_cumulative_250 <- calculate_cumulative_return(factor_returns_250) %>%
  mutate(Type = "Factor Portfolio (250)")
baseline_cumulative_250 <- calculate_cumulative_return(baseline_returns_250) %>%
  mutate(Type = "Baseline Portfolio (250)")


cumulative_data <- bind_rows(
  factor_cumulative_400,
  factor_cumulative_350,
  factor_cumulative_250,
  index_cumulative
)

# 繪製累積報酬率圖
ggplot(cumulative_data, aes(x = Date, y = Cumulative_Return, color = Type)) +
  geom_line(size = 0.8) +
  labs(
    title = "2021-2023累積報酬率比較 (400, 350, 250 投資組合與大盤)",
    x = "日期",
    y = "累積報酬率",
    color = "策略類型"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )


