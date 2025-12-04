# NVIDIA Stock Volatility Forecasting (Time-Series ML + EGARCH + XGBoost)

This project forecasts next-day volatility and price movement for NVIDIA stock (NVDA) using a hybrid time-series ML pipeline.
It combines:
- **EGARCH(1,1)** for econometric volatility modeling
- **XGBoost** for predicting:
    - Next-day return
    - Direction (up/down)
    - Volatility (absolute return)
- A simple **rule-based backtest** to compare a strategy vs. buy-and-hold.

The objective of this project is show how traditional volatility models and ML can be combined for **risk analysis and forecasting.** (this is nottttt useful to make next day trading decisons, alone pls use mindfully thanks ily) 

## Dataset
The data is pulled directly from Yahoo Finance using R.
Ticker: NVDA
Frequency: Daily OHLCV
Period: 1999–present
I initially used a Kaggle dataset for offline testing and also just to see if the model was working lol.
Raw data will be fetched automatically when you run the pipeline.

## Getting Started
### Prerequisites
Install these R packages:
`install.packages(c(
  "quantmod","rugarch","xgboost","dplyr","ggplot2",
  "timetk","PerformanceAnalytics","zoo","TTR"
))`

### Running the Full Pipeline
All steps can be executed by running:
`Rscript scripts/run_all.R`

This script:
1. Fetches NVDA data
2. Builds features
3. Fits EGARCH
4. Trains XGBoost models
5. Runs the backtest
6. Outputs appear in the outputs/ and plots/ folders.


***The model is better at forecasting volatility than price direction — which is expected for financial time-series.***

## Customizing the Model
To experiment with different ideas, you can:
- Add more features (macro indicators, VIX, options data)
- Swap XGBoost for:
- Random Forest
- CatBoost
- LSTM / GRU
- Modify the backtest thresholds

Happy to recieve feedback!

## License
This project is released under the MIT License.
