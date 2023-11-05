# Forecasting Model using VAR

Welcome to the repository for the Vector Autoregression (VAR) forecasting model project. This project is a focused effort to construct and apply a VAR model in the R programming language to predict the price of an asset.

## Project Overview

The VAR model implemented in this project is developed using R. This model aims to forecast the future prices of a selected set of assets by examining their historical prices.

### Data Description

The dataset used in this project consists of daily price records from 2020 to 2023 for the following assets:

- Gold
- Silver
- Crude Oil
- US Dollar Index (DXY)
- S&P 500 Index

## Model Construction

The construction of the VAR model was undertaken meticulously:

1. **Autocorrelation Analysis**: The initial phase involved examining the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF) for each asset. The analysis revealed that each value within the time series was "highly correlated with the previous value," which set the foundation for subsequent analysis.

2. **Stationarity Tests**: Subsequent application of the Dickey-Fuller and KPSS tests indicated that the time series of each asset were non-stationary. By differencing the data once (first differences), we established that each asset time series was integrated of order 1, denoted I(1).

3. **Granger Causality Tests**: The Granger Causality test was then conducted to examine the predictability of the time series. The p-values from the test suggested that most of the variables (time series) in the system are interchangeably causing each other.

4. **Cointegration Test**: Finally, a cointegration test was carried out, which found that there is no cointegration in any of our variables. With this result, it was appropriate to proceed with the VAR model.

## Results and Usage

After constructing the VAR model, various error metrics were computed to assess the model's performance, including:

- Mean Absolute Error (MAE)
- Mean Squared Error (MSE)
- Root Mean Squared Error (RMSE)

In addition to error metrics, the forecasting ability of the VAR model was benchmarked against a naive forecast. The naive forecast simply assumes that the value of the next time point is equal to the last observed value of the time series, which serves as a rudimentary baseline to gauge the sophistication of our model.

A Diebold-Mariano test was conducted to statistically compare the predictive accuracy of our VAR model against the naive forecast. The test results demonstrated that the VAR model has a significantly higher predictive ability.


