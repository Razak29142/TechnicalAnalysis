library(quantmod)

load_stock_data <- function(portfolio_file = "portfolio.txt") {
  symbols <- readLines(portfolio_file, warn = FALSE)
  stock_data <- list()
  
  for (symbol in symbols) {
    tryCatch({
      data <- getSymbols(symbol, src = "yahoo",
                         from = "2025-01-01",
                         auto.assign = FALSE)
      stock_data[[symbol]] <- data.frame(
        Date = as.Date(index(data)),
        coredata(data),
        row.names = NULL,
        check.names = FALSE
      )
      cat("Successfully loaded:", symbol, "\n")
    }, error = function(e) {
      cat("Error loading:", symbol, "-", conditionMessage(e), "\n")
    })
  }
  
  return(stock_data)
}

stocks_data <- load_stock_data()

lapply(stocks_data, head)

# Calculate basic stock statistics

calculate_statistics <- function(stock_df) {
  close_col <- grep("\\.Close$", names(stock_df), value = TRUE)
  prices <- as.numeric(stock_df[[close_col[1]]])
  prices <- prices[!is.na(prices)]
  
  frequency <- table(prices)
  mode_price <- as.numeric(names(frequency)[
    which.max(frequency)
  ])
  
  moving_avg <- TTR::SMA(prices, n = 20)
  
  data.frame(
    Mean = mean(prices),
    Median = median(prices),
    Mode = mode_price,
    Standard_Deviation = sd(prices),
    Moving_Average_20 = tail(na.omit(moving_avg), 1)
  )
}

stock_statistics <- lapply(stocks_data, calculate_statistics)

print(stock_statistics)

library(ggplot2)

# Display stock prices with 20-day moving averages
for (symbol in names(stocks_data)) {
  stock_df <- stocks_data[[symbol]]
  close_col <- grep("\\.Close$", names(stock_df), value = TRUE)[1]
  
  stock_df$Close <- as.numeric(stock_df[[close_col]])
  stock_df$MA20 <- TTR::SMA(stock_df$Close, n = 20)
  
  chart <- ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Closing Price")) +
    geom_line(aes(y = MA20, color = "20-Day Moving Average"),
              na.rm = TRUE) +
    labs(
      title = paste(symbol, "Stock Price Analysis"),
      x = "Date",
      y = "Stock Price (USD)",
      color = "Indicator"
    ) +
    theme_minimal()
  
  print(chart)
  ggsave(paste0(symbol, "_stock_chart.png"),
         plot = chart, width = 10, height = 6)
}