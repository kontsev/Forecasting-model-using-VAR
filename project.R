library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(BVAR)
library(seasonal)
library(imputeTS)
library(openxlsx)
library(quantmod)
library(xts)
library(gridExtra)
library(lmtest)

# Get data
symbols <- c("GC=F",  # Gold
             "SI=F",  # Silver
             "CL=F",  # Oil (Crude)
             "DX-Y.NYB", # US Dollar Index
             "SPY")    # S&P 500 ETF

from_date <- as.Date("2020-01-01")
to_date <- Sys.Date()

data_list <- lapply(symbols, function(sym) {
  getSymbols(sym, src = "yahoo", from = from_date, to = to_date, auto.assign = FALSE)
})

prices <- do.call(merge, lapply(data_list, Cl))
colnames(prices) <- c("Gold", "Silver", "Oil", "Dollar", "S&P500")

prices$Date <- index(prices)

prices_df <- data.frame(Date=index(prices), coredata(prices))

prices_df
############################

# Deleted Nan's and get graphs
prices_df_clean <- na.omit(prices_df)

ggplot(prices_df_clean, aes(x = Date, y = Gold)) + 
  geom_line() + 
  ggtitle("Gold Prices Over Time") + 
  xlab("Date") + 
  ylab("Gold Price")

ggplot(prices_df_clean, aes(x = Date, y = Silver)) + 
  geom_line() + 
  ggtitle("Silver Prices Over Time") + 
  xlab("Date") + 
  ylab("Silver Price")

ggplot(prices_df_clean, aes(x = Date, y = Oil)) + 
  geom_line() + 
  ggtitle("Oil Prices Over Time") + 
  xlab("Date") + 
  ylab("Oil Price")

ggplot(prices_df_clean, aes(x = Date, y = Dollar)) + 
  geom_line() + 
  ggtitle("Dollar Index Over Time") + 
  xlab("Date") + 
  ylab("Dollar Index")

ggplot(prices_df_clean, aes(x = Date, y = `Date.1`)) + 
  geom_line() + 
  ggtitle("S&P 500 Over Time") + 
  xlab("Date") + 
  ylab("S&P 500 Price")

plot_list <- list(
  ggplot(prices_df_clean, aes(x = Date, y = Gold)) + geom_line() + ggtitle("Gold"),
  ggplot(prices_df_clean, aes(x = Date, y = Silver)) + geom_line() + ggtitle("Silver"),
  ggplot(prices_df_clean, aes(x = Date, y = Oil)) + geom_line() + ggtitle("Oil"),
  ggplot(prices_df_clean, aes(x = Date, y = Dollar)) + geom_line() + ggtitle("Dollar Index"),
  ggplot(prices_df_clean, aes(x = Date, y = `Date.1`)) + geom_line() + ggtitle("S&P 500")
)

grid.arrange(grobs = plot_list, ncol = 2)

#######################################

# ACF, PACF
acf(prices_df_clean['Gold'])
pacf(prices_df_clean['Gold'])

acf(prices_df_clean['Silver'])
pacf(prices_df_clean['Silver'])

acf(prices_df_clean['Oil'])
pacf(prices_df_clean['Oil'])

acf(prices_df_clean['Dollar'])
pacf(prices_df_clean['Dollar'])

acf(prices_df_clean['Date.1'])
pacf(prices_df_clean['Date.1'])

# Current value highly correlated with previous values
# Let's look at stationarity
# From PACF we get that AR model is applicable for data
#####################

# Divide data into train and test
split_point <- floor(0.8 * nrow(prices_df_clean)) # 80% train, 20% test

train_set <- prices_df_clean[1:split_point, ]

test_set <- prices_df_clean[(split_point + 1):nrow(prices_df_clean), ]

head(test_set)
#####################################################


# Checking stationarity, apply differencies
gold_series <- train_set$Gold
silver_series <- train_set$Silver
oil_series <- train_set$Oil
dollar_series <- train_set$Dollar
sp500_series <- train_set$Date.1

# Non-stationary
adf.test(gold_series)
kpss.test(gold_series)

# Non-stationary
adf.test(silver_series)
kpss.test(silver_series)

# Non-stationary
adf.test(oil_series)
kpss.test(oil_series)

# Non-stationary
adf.test(dollar_series)
kpss.test(dollar_series)

# Non-stationary
adf.test(sp500_series)
kpss.test(sp500_series)
##########################################################

# Get first difference and check for stationarity
gold_diff <- diff(train_set$Gold, differences = 1)
silver_diff <- diff(train_set$Silver, differences = 1)
oil_diff <- diff(train_set$Oil, differences = 1)
dollar_diff <- diff(train_set$Dollar, differences = 1)
sp500_diff <- diff(train_set$Date.1, differences = 1)

# Stationary
adf.test(gold_diff)
kpss.test(gold_diff)

# Stationary
adf.test(silver_diff)
kpss.test(silver_diff)

# Stationary
adf.test(oil_diff)
kpss.test(oil_diff)

# Stationary
adf.test(dollar_diff)
kpss.test(dollar_diff)

# Stationary
adf.test(sp500_diff)
kpss.test(sp500_diff)
###############################################

# Granger Casuality test

p_value_matrix <- matrix(NA, nrow = 5, ncol = 5, 
                         dimnames = list(c("gold_y", "silver_y", "oil_y", "dollar_y", "sp500_y"),
                                         c("gold_x", "silver_x", "oil_x", "dollar_x", "sp500_x")))
p_value_matrix

time_series_names <- c("gold", "silver", "oil", "dollar", "sp500")

for (i in 1:5) {
  for (j in 1:5) {
    if (i != j) {
      x <- paste0(time_series_names[i], "_diff")
      y <- paste0(time_series_names[j], "_diff")
      
      x_val <- get(x)
      y_val <- get(y)
      
      granger_test <- grangertest(x_val ~ y_val, order = 5)
      p_value_matrix[i, j] <- granger_test$'Pr(>F)'[2]
    }
  }
}
p_value_matrix
#########################################################################


# Cointegration test using Johansen test
data_matrix <- cbind(gold_series, silver_series, oil_series, 
                     dollar_series, sp500_series)

cointegration_test <- function(transform_data, alpha = 0.05) {
  test_result <- ca.jo(transform_data, type="trace", ecdet="none", K=5)
  summary_result <- summary(test_result)
  
  traces <- summary_result@teststat
  critical_values <- summary_result@cval
  
  cat("Name   :: Test Stat > C(", 100 * (1 - alpha), "%)       => Signif\n", rep("-", 40), "\n", sep="")
  
  for (i in 1:length(traces)) {
    signif <- ifelse(traces[i] > critical_values[i, 2], "True", "False")
    cat("r <= ", i-1, "::", formatC(traces[i], format="f", digits=2), 
        ">", formatC(critical_values[i, 2], format="f", digits=4), "    =>  ", signif, "\n")
  }
}

cointegration_test(data_matrix)
#####################################

# VaR
diff_data <- cbind(gold_diff, silver_diff, oil_diff, 
                     dollar_diff, sp500_diff)
lag.selection <- VARselect(diff_data, lag.max = 15, type = "both")
lag.selection
opt.lag <- lag.selection$selection["AIC(n)"]
var.model <- VAR(diff_data, p = opt.lag, type = "both")

summary(var.model)
################

# Forecast for 45 steps
forecasts <- predict(var.model, n.ahead=45)
point_forecasts <- forecasts$fcst

fcst_list <- list()

for (i in seq_along(point_forecasts)) {
  fcst_list[[names(point_forecasts)[i]]] <- point_forecasts[[i]][, "fcst"]
}
fcst_df <- as.data.frame(fcst_list)
fcst_df
####################################

# Compare forecast with real data
reversed_df <- fcst_df
test_set_first_15 <- head(test_set, 45)
test_set_first_15_no_date <- subset(test_set_first_15, select = -Date)
test_set_first_15_no_date

for (i in seq_along(reversed_df)) {
  reversed_df[, i] <- test_set_first_15_no_date[i] + cumsum(reversed_df[, i])
}

reversed_df

dates <- test_set$Date[1:45]
reversed_df$Date <- dates
reversed_df <- reversed_df[c("Date", setdiff(names(reversed_df), "Date"))]
reversed_df
########################################################

# Get graphs
actual_df <- data.frame(Date = test_set_first_15$Date, Gold = test_set_first_15$Gold, Silver = test_set_first_15$Silver, Type = 'Actual')
forecast_df <- data.frame(Date = reversed_df$Date, Gold = reversed_df$gold_diff, Silver = reversed_df$silver_diff, Type = 'Forecast')

combined_df <- rbind(actual_df, forecast_df)

ggplot(combined_df, aes(x = Date, y = Gold, color = Type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Actual vs Forecast Prices", x = "Date", y = "Price") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red"))

ggplot(combined_df, aes(x = Date, y = Silver, color = Type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Actual vs Forecast Prices", x = "Date", y = "Price") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red"))
#########################

# Calculate errors
library(Metrics)
actual <- combined_df[combined_df$Type == 'Actual', 'Gold']
forecast <- combined_df[combined_df$Type == 'Forecast', 'Gold']

bias <- mean(forecast - actual)
mae <- mae(actual, forecast)
mse <- mse(actual, forecast)
rmse <- sqrt(mse)

cat('Bias:', bias, '\n')
cat('MAE:', mae, '\n')
cat('MSE:', mse, '\n')
cat('RMSE:', rmse, '\n')
###################

# Naive forecast
naive_forecast <- actual[-length(actual)]
actual_values_for_naive <- actual[-1]

naive_mae <- mean(abs(naive_forecast - actual_values_for_naive))
naive_mse <- mean((naive_forecast - actual_values_for_naive)^2)
naive_rmse <- sqrt(naive_mse)

cat('MAE:', naive_mae, '\n')
cat('MSE:', naive_mse, '\n')
cat('RMSE:', naive_rmse, '\n')


improvement_mae <- (naive_mae - mae) / naive_mae * 100

improvement_rmse <- (naive_rmse - rmse) / naive_rmse * 100

cat("MAE improvement:", improvement_mae, "%\n")
cat("RMSE improvement:", improvement_rmse, "%\n")

# Diebold-Mariano test
error1 <- actual[-1] - forecast[-1]
error2 <- actual_values_for_naive - naive_forecast
dm_test <- dm.test(error1, error2, alternative = "two.sided")

dm_test
######################
