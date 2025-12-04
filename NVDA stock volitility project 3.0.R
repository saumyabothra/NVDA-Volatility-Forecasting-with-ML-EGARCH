
#SETUP USING YAHOO STOCK DATA

library(quantmod)

# Download full NVDA history from Yahoo
getSymbols("NVDA", src = "yahoo", from = "1999-01-01", auto.assign = TRUE)
tryCatch({
  getSymbols("NVDA", src="yahoo", auto.assign=FALSE)
}, error=function(e) {
  cat("Yahoo download failed. Using last saved data.\n")
  readRDS("nvda_backup.rds")
})

nvda_raw <- data.frame(
  Date     = as.Date(index(NVDA)),
  AdjClose = as.numeric(NVDA$NVDA.Adjusted),
  Close    = as.numeric(NVDA$NVDA.Close),
  High     = as.numeric(NVDA$NVDA.High),
  Low      = as.numeric(NVDA$NVDA.Low),
  Open     = as.numeric(NVDA$NVDA.Open),
  Volume   = as.numeric(NVDA$NVDA.Volume)
) %>%
  arrange(Date)


#CORE FEATURE ENGINEERING

nvda_data <- nvda_raw %>%
  mutate(
    #Returns based on AdjClose
    Daily_Return  = log(AdjClose / lag(AdjClose)),
    Simple_Return = (AdjClose - lag(AdjClose)) / lag(AdjClose),
    
    #Volatility features
    Daily_Range     = High - Low,
    Daily_Range_Pct = (High - Low) / AdjClose,
    Gap_Open        = (Open - lag(Close)) / lag(Close),
    
    #Volume features
    Volume_Log   = log(Volume),
    Volume_Ratio = Volume / lag(Volume),
    
    #Price position in daily range
    Price_Position = ifelse(High != Low, (Close - Low) / (High - Low), 0.5),
    
    Weekday = weekdays(Date),
    Month   = month(Date),
    
    #Lags
    Return_Lag1 = lag(Daily_Return, 1),
    Return_Lag2 = lag(Daily_Return, 2),
    Return_Lag5 = lag(Daily_Return, 5),
    
    #Rolling stats on returns
    Rolling_Mean_5  = zoo::rollmean(Daily_Return, 5,  fill = NA, align = "right"),
    Rolling_Mean_20 = zoo::rollmean(Daily_Return, 20, fill = NA, align = "right"),
    Rolling_Std_5   = zoo::rollapply(Daily_Return, 5,  sd, fill = NA, align = "right"),
    Rolling_Std_20  = zoo::rollapply(Daily_Return, 20, sd, fill = NA, align = "right"),
    
    #Targets
    Next_Day_Return      = lead(Daily_Return, 1),
    Next_Day_Abs_Return  = abs(lead(Daily_Return, 1)),         # volatility proxy
    Next_Day_Direction   = ifelse(lead(Daily_Return, 1) > 0, 1, 0)
  )

#Remove missing variables
nvda_clean <- nvda_data %>% na.omit()

#Add technical + volatility regime info
nvda_clean <- nvda_clean %>%
  mutate(
    RSI          = TTR::RSI(AdjClose, n = 14),
    Momentum_5   = AdjClose / lag(AdjClose, 5)  - 1,
    Momentum_20  = AdjClose / lag(AdjClose, 20) - 1,
    Vol_Ratio_5_20 = Rolling_Std_5 / Rolling_Std_20,
    Volume_Price_Corr = zoo::rollapply(
      data      = cbind(Volume_Log, Daily_Return),
      width     = 20,
      FUN       = function(m) stats::cor(m[,1], m[,2]),
      by.column = FALSE,
      fill      = NA,
      align     = "right"
    ),
    Vol_Regime = case_when(
      Rolling_Std_20 > quantile(Rolling_Std_20, 0.8, na.rm = TRUE) ~ "High",
      Rolling_Std_20 < quantile(Rolling_Std_20, 0.2, na.rm = TRUE) ~ "Low",
      TRUE ~ "Medium"
    )
  ) %>%
  na.omit()


nvda_clean$EGARCH_Cond_Vol  <- nvda_clean$Rolling_Std_20
nvda_clean$EGARCH_Cond_Mean <- nvda_clean$Rolling_Mean_20


cat(sprintf("Clean dataset: %d rows, %d columns\n", nrow(nvda_clean), ncol(nvda_clean)))


#DATA EXPLORATION 

cat("Returns summary:\n")
print(summary(nvda_clean$Daily_Return))

cat("\n20-day rolling annualized volatility summary:\n")
print(summary(nvda_clean$Rolling_Std_20 * sqrt(252)))

#Price
p_price <- ggplot(nvda_clean, aes(Date, AdjClose)) +
  geom_line(color = "blue", alpha = 0.7) +
  labs(title = "NVDA Adjusted Close Price", x = NULL, y = "Price ($)") +
  theme_minimal()

#Daily returns
p_ret <- ggplot(nvda_clean, aes(Date, Daily_Return)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "NVDA Daily Log Returns", x = NULL, y = "Return") +
  theme_minimal()

#Rolling vol
p_vol <- ggplot(nvda_clean, aes(Date)) +
  geom_line(aes(y = Rolling_Std_20 * sqrt(252)), color = "darkred") +
  geom_ribbon(aes(ymin = 0, ymax = Rolling_Std_20 * sqrt(252)),
              fill = "red", alpha = 0.1) +
  labs(title = "20-Day Rolling Annualized Volatility",
       x = NULL, y = "Annualized σ") +
  theme_minimal()

print(p_price)
print(p_ret)
print(p_vol)


#EGARCH MODEL

#numeric xts returns
ret_xts <- xts::xts(nvda_clean$Daily_Return, order.by = nvda_clean$Date)
ret_vec <- as.numeric(ret_xts)
ret_vec <- ret_vec[is.finite(ret_vec)]

egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "std"
)

egarch_fit_try <- try(
  ugarchfit(spec = egarch_spec, data = ret_vec, solver = "hybrid"),
  silent = TRUE
)

if (inherits(egarch_fit_try, "try-error")) {
  cat("EGARCH(1,1) fitting failed. Using Rolling_Std_20 as volatility proxy.\n")
  
  nvda_clean$EGARCH_Cond_Vol  <- nvda_clean$Rolling_Std_20
  nvda_clean$EGARCH_Cond_Mean <- nvda_clean$Rolling_Mean_20
  
  p_egarch <- ggplot(
    data.frame(
      Date = nvda_clean$Date,
      Vol  = nvda_clean$EGARCH_Cond_Vol * sqrt(252)
    ),
    aes(Date, Vol)
  ) +
    geom_line(color = "orange") +
    labs(
      title = "Rolling 20-day Volatility (fallback)",
      x = NULL, y = "σ (annualized)"
    ) +
    theme_minimal()
  
  print(p_egarch)
  
} else {
  cat("EGARCH(1,1) fitted successfully.\n")
  egarch_fit <- egarch_fit_try
  print(egarch_fit)
  
  cond_vol  <- sigma(egarch_fit)
  cond_mean <- fitted(egarch_fit)
  
  n_fit <- length(cond_vol)
  n_dat <- nrow(nvda_clean)
  if (n_fit != n_dat) {
    cond_vol  <- tail(cond_vol,  n_dat)
    cond_mean <- tail(cond_mean, n_dat)
  }
  
  nvda_clean$EGARCH_Cond_Vol  <- as.numeric(cond_vol)
  nvda_clean$EGARCH_Cond_Mean <- as.numeric(cond_mean)
  
  p_egarch <- ggplot(
    data.frame(
      Date = nvda_clean$Date,
      Vol  = nvda_clean$EGARCH_Cond_Vol * sqrt(252)
    ),
    aes(Date, Vol)
  ) +
    geom_line(color = "purple") +
    labs(
      title = "EGARCH Conditional Volatility (Annualized)",
      x = NULL, y = "σ_t (annualized)"
    ) +
    theme_minimal()
  
  print(p_egarch)
}


#MORE FEATURE ENGINEERING

#Residual from the EGARCH_Cond_Mean we have as of now
nvda_clean$EGARCH_Residual <- nvda_clean$Daily_Return - nvda_clean$EGARCH_Cond_Mean

nvda_clean <- nvda_clean %>%
  mutate(
    Vol_Persistence   = lag(EGARCH_Cond_Vol, 1) / EGARCH_Cond_Vol,
    Is_Jump           = as.numeric(abs(Daily_Return) > 3 * Rolling_Std_20),
    Volume_Surge      = as.numeric(Volume > 2 * lag(Volume, 1)),
    Price_Accel       = Daily_Return - lag(Daily_Return, 1),
    Vol_Regime_Change = Vol_Regime != lag(Vol_Regime, 1),
    Is_Monday         = as.numeric(Weekday == "Monday"),
    Is_Friday         = as.numeric(Weekday == "Friday"),
    Is_January        = as.numeric(Month == 1),
    Is_October        = as.numeric(Month == 10),
    Is_December       = as.numeric(Month == 12)
  ) %>%
  na.omit()

cat(sprintf("Final feature count: %d (excluding Date)\n", ncol(nvda_clean) - 1))


#XGBOOST MODEL

feature_columns <- c(
#Price / return
  "Return_Lag1","Return_Lag2","Return_Lag5",
  "Rolling_Mean_5","Rolling_Mean_20",
  "Momentum_5","Momentum_20",
  
#Volatility
  "Rolling_Std_5","Rolling_Std_20",
  "EGARCH_Cond_Vol","Vol_Ratio_5_20",
  "Daily_Range_Pct","Gap_Open",
  
#Volume
  "Volume_Log","Volume_Ratio","Volume_Surge",
  
#Technical
  "RSI","Price_Position",
  
#EGARCH / residual
  "EGARCH_Residual","Vol_Persistence",
  
#Event / dynamics
  "Is_Jump","Price_Accel",
  
#Time
  "Is_Monday","Is_Friday",
  "Is_January","Is_October","Is_December"
)

X <- nvda_clean[, feature_columns]
y_return     <- nvda_clean$Next_Day_Return
y_direction  <- nvda_clean$Next_Day_Direction
y_volatility <- nvda_clean$Next_Day_Abs_Return

n <- nrow(X)
train_size <- floor(0.8 * n)
train_idx  <- 1:train_size
test_idx   <- (train_size + 1):n

X_train <- X[train_idx, ]
X_test  <- X[test_idx, ]

y_train_return <- y_return[train_idx]
y_test_return  <- y_return[test_idx]

y_train_dir <- y_direction[train_idx]
y_test_dir  <- y_direction[test_idx]

y_train_vol <- y_volatility[train_idx]
y_test_vol  <- y_volatility[test_idx]

#XGBoost: Return (regression)
cat("\nTraining XGBoost for next-day return...\n")
dtrain_ret <- xgb.DMatrix(as.matrix(X_train), label = y_train_return)
dtest_ret  <- xgb.DMatrix(as.matrix(X_test),  label = y_test_return)

params_ret <- list(
  objective        = "reg:squarederror",
  eta              = 0.01,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  lambda           = 1,
  alpha            = 0.1,
  eval_metric      = "rmse"
)

xgb_return <- xgb.train(
  params = params_ret,
  data   = dtrain_ret,
  nrounds = 1000,
  early_stopping_rounds = 50,
  watchlist = list(train = dtrain_ret, test = dtest_ret),
  verbose = 0
)

#XGBoost: Direction (classification)
cat("Training XGBoost for direction (up/down)...\n")
dtrain_dir <- xgb.DMatrix(as.matrix(X_train), label = y_train_dir)
dtest_dir  <- xgb.DMatrix(as.matrix(X_test),  label = y_test_dir)

params_dir <- list(
  objective        = "binary:logistic",
  eta              = 0.01,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "logloss"
)

xgb_dir <- xgb.train(
  params = params_dir,
  data   = dtrain_dir,
  nrounds = 1000,
  early_stopping_rounds = 50,
  watchlist = list(train = dtrain_dir, test = dtest_dir),
  verbose = 0
)

#XGBoost: Volatility (abs-return regression)
cat("Training XGBoost for volatility (abs next-day return)...\n")
dtrain_vol <- xgb.DMatrix(as.matrix(X_train), label = y_train_vol)
dtest_vol  <- xgb.DMatrix(as.matrix(X_test),  label = y_test_vol)

params_vol <- list(
  objective        = "reg:squarederror",
  eta              = 0.01,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "rmse"
)

xgb_vol <- xgb.train(
  params = params_vol,
  data   = dtrain_vol,
  nrounds = 1000,
  early_stopping_rounds = 50,
  watchlist = list(train = dtrain_vol, test = dtest_vol),
  verbose = 0
)


#EVALUATION

#Predictions
pred_return   <- predict(xgb_return, as.matrix(X_test))
pred_dir_prob <- predict(xgb_dir,    as.matrix(X_test))
pred_dir      <- as.numeric(pred_dir_prob > 0.5)
pred_vol      <- predict(xgb_vol,    as.matrix(X_test))  # forecast abs return

#Return prediction metrics
ret_rmse <- sqrt(mean((pred_return - y_test_return)^2))
ret_mae  <- mean(abs(pred_return - y_test_return))
ret_cor  <- cor(pred_return, y_test_return)

cat("\nReturn prediction:\n")
cat(sprintf("  RMSE: %.4f\n", ret_rmse))
cat(sprintf("  MAE : %.4f\n", ret_mae))
cat(sprintf("  Corr: %.4f\n", ret_cor))

#Direction classification metrics
tp <- sum(pred_dir == 1 & y_test_dir == 1)
fp <- sum(pred_dir == 1 & y_test_dir == 0)
fn <- sum(pred_dir == 0 & y_test_dir == 1)

accuracy  <- mean(pred_dir == y_test_dir)
precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
recall    <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
f1        <- ifelse(is.na(precision) || is.na(recall) || (precision + recall) == 0,
                    NA_real_,
                    2 * (precision * recall) / (precision + recall))

cat("\nDirection prediction:\n")
cat(sprintf("  Accuracy : %.2f%%\n", accuracy * 100))
cat(sprintf("  Precision: %.2f%%\n", precision * 100))
cat(sprintf("  Recall   : %.2f%%\n", recall * 100))
cat(sprintf("  F1-score : %.4f\n", f1))

#Volatility (abs return)
vol_rmse <- sqrt(mean((pred_vol - y_test_vol)^2))
vol_mae  <- mean(abs(pred_vol - y_test_vol))
vol_cor  <- cor(pred_vol, y_test_vol)

cat("\nVolatility prediction (abs next-day return):\n")
cat(sprintf("  RMSE: %.4f\n", vol_rmse))
cat(sprintf("  MAE : %.4f\n", vol_mae))
cat(sprintf("  Corr: %.4f\n", vol_cor))

#Feature importance (focus on volatility model)
imp_vol <- xgb.importance(feature_names = colnames(X_train), model = xgb_vol)
cat("\nTop 10 features for volatility prediction:\n")
print(imp_vol[1:10, ])

#Simple ensemble: XGBoost vol forecast and EGARCH current volatility combined (daily sigma)
egarch_test_vol <- nvda_clean$EGARCH_Cond_Vol[test_idx] 

#Normalize scale roughly
vol_ens <- 0.5 * pred_vol + 0.5 * egarch_test_vol

ens_rmse <- sqrt(mean((vol_ens - y_test_vol)^2))
ens_mae  <- mean(abs(vol_ens - y_test_vol))
ens_cor  <- cor(vol_ens, y_test_vol)

cat("\nEnsemble volatility (0.5 * XGB + 0.5 * EGARCH):\n")
cat(sprintf("  RMSE: %.4f\n", ens_rmse))
cat(sprintf("  MAE : %.4f\n", ens_mae))
cat(sprintf("  Corr: %.4f\n", ens_cor))


#BACKTESTING STRATEGY vs BUY & HOLD

backtest_data <- nvda_clean[test_idx, ]
backtest_data$Pred_Return    <- pred_return
backtest_data$Pred_Direction <- pred_dir
backtest_data$Pred_Vol       <- pred_vol

# Simple rules:
#  - Buy if predicted return > 1% and predicted vol < 70th percentile of predicted vol
#  - Sell/short if predicted return < -1%
#  - Otherwise hold cash
vol_thresh <- quantile(backtest_data$Pred_Vol, 0.7, na.rm = TRUE)

backtest_data <- backtest_data %>%
  mutate(
    Signal = case_when(
      Pred_Return > 0.01 & Pred_Vol < vol_thresh ~ "Buy",
      Pred_Return < -0.01                        ~ "Sell",
      TRUE                                       ~ "Hold"
    ),
    Strategy_Return = case_when(
      Signal == "Buy"  ~ Next_Day_Return,
      Signal == "Sell" ~ -Next_Day_Return,
      TRUE             ~ 0
    ),
    Benchmark_Return = Next_Day_Return
  )

strategy_cum  <- cumsum(backtest_data$Strategy_Return)
benchmark_cum <- cumsum(backtest_data$Benchmark_Return)

strategy_sharpe  <- mean(backtest_data$Strategy_Return) /
  sd(backtest_data$Strategy_Return) * sqrt(252)
benchmark_sharpe <- mean(backtest_data$Benchmark_Return) /
  sd(backtest_data$Benchmark_Return) * sqrt(252)

strategy_max_dd  <- maxDrawdown(backtest_data$Strategy_Return)
benchmark_max_dd <- maxDrawdown(backtest_data$Benchmark_Return)

strategy_win_rate  <- mean(backtest_data$Strategy_Return  > 0)
benchmark_win_rate <- mean(backtest_data$Benchmark_Return > 0)

cat("\nStrategy performance:\n")
cat(sprintf("  Cumulative return: %.2f%%\n", (exp(tail(strategy_cum, 1)) - 1) * 100))
cat(sprintf("  Sharpe ratio    : %.2f\n", strategy_sharpe))
cat(sprintf("  Max drawdown    : %.2f%%\n", strategy_max_dd * 100))
cat(sprintf("  Win rate        : %.2f%%\n", strategy_win_rate * 100))

cat("\nBuy & Hold benchmark:\n")
cat(sprintf("  Cumulative return: %.2f%%\n", (exp(tail(benchmark_cum, 1)) - 1) * 100))
cat(sprintf("  Sharpe ratio    : %.2f\n", benchmark_sharpe))
cat(sprintf("  Max drawdown    : %.2f%%\n", benchmark_max_dd * 100))
cat(sprintf("  Win rate        : %.2f%%\n", benchmark_win_rate * 100))

#cumulative log returns plot
ret_df <- data.frame(
  Date      = backtest_data$Date,
  Strategy  = strategy_cum,
  Benchmark = benchmark_cum
)

p_bt <- ggplot(ret_df, aes(Date)) +
  geom_line(aes(y = Strategy,  color = "Strategy"), linewidth = 1) +
  geom_line(aes(y = Benchmark, color = "Buy & Hold"), linewidth = 1, alpha = 0.7) +
  scale_color_manual(values = c("Strategy" = "darkgreen", "Buy & Hold" = "blue")) +
  labs(title = "Backtest: Strategy vs Buy & Hold",
       x = NULL, y = "Cumulative log return", color = NULL) +
  theme_minimal()
print(p_bt)


#VOLATILITY SUMMARY


cat("\n", strrep("=", 70), "\n", sep = "")
cat("VOLATILITY INSIGHTS SUMMARY\n")
cat(strrep("=", 70), "\n", sep = "")

vol_stats <- nvda_clean %>%
  summarise(
    Annual_Volatility      = sd(Daily_Return) * sqrt(252),
    Avg_Daily_Abs_Return   = mean(abs(Daily_Return)),
    Max_Daily_Gain         = max(Daily_Return),
    Max_Daily_Loss         = min(Daily_Return),
    Volatility_Clustering  = Box.test(Daily_Return^2, lag = 10)$p.value < 0.05,
    Days_Over_5pct         = sum(abs(Daily_Return) > 0.05) / n() * 100,
    Avg_Daily_Range        = mean(Daily_Range_Pct) * 100
  )

cat("\n1. VOLATILITY CHARACTERISTICS:\n")
cat(sprintf("   • Annualized volatility           : %.1f%%\n", vol_stats$Annual_Volatility * 100))
cat(sprintf("   • Avg daily absolute return       : %.2f%%\n", vol_stats$Avg_Daily_Abs_Return * 100))
cat(sprintf("   • Maximum daily gain              : %.1f%%\n", vol_stats$Max_Daily_Gain * 100))
cat(sprintf("   • Maximum daily loss              : %.1f%%\n", vol_stats$Max_Daily_Loss * 100))
cat(sprintf("   • Days with >5%% moves            : %.1f%% of days\n", vol_stats$Days_Over_5pct))
cat(sprintf("   • Avg daily range (High–Low)      : %.2f%% of price\n", vol_stats$Avg_Daily_Range))

cat("\n2. VOLATILITY PATTERNS:\n")
if (vol_stats$Volatility_Clustering) {
  cat("   • Strong volatility clustering detected (GARCH effects present)\n")
} else {
  cat("   • No strong volatility clustering detected at lag 10\n")
}

cat("\n3. KEY PREDICTORS OF VOLATILITY (XGBoost):\n")
for (i in seq_len(min(5, nrow(imp_vol)))) {
  cat(sprintf("   %d. %s\n", i, imp_vol$Feature[i]))
}

cat("\n4. TRADING IMPLICATIONS:\n")
cat(sprintf("   • Strategy Sharpe ratio: %.2f\n", strategy_sharpe))
cat("   • Volatility-based filters can improve risk-adjusted returns vs naive buy & hold.\n")

cat("\n", strrep("=", 70), "\n", sep = "")

daily_nvda_risk_report <- function() {
  #Last available obs
  last_row   <- tail(nvda_clean, 1)
  x_today    <- tail(X, 1)
  
  #Price as of last close
  last_price <- last_row$AdjClose
  
  #XGBoost volatility forecast (abs next-day return)
  pred_vol   <- as.numeric(predict(xgb_vol, as.matrix(x_today)))
  
  #EGARCH one-step-ahead forecast
  egarch_fc  <- ugarchforecast(egarch_fit, n.ahead = 1)
  egarch_sig <- as.numeric(sigma(egarch_fc))       # daily sigma
  
  #Simple ensemble
  vol_ens    <- 0.5 * pred_vol + 0.5 * egarch_sig
  
  #Regime based on ensemble volatility
  #Use historical distribution to define quantiles
  hist_vol   <- nvda_clean$Next_Day_Abs_Return  # or Rolling_Std_20, up to you
  q_low      <- quantile(hist_vol, 0.2, na.rm = TRUE)
  q_high     <- quantile(hist_vol, 0.8, na.rm = TRUE)
  
  regime <- dplyr::case_when(
    vol_ens < q_low  ~ "LOW",
    vol_ens > q_high ~ "HIGH",
    TRUE             ~ "MEDIUM"
  )
  
  #Expected dollar move (approx)
  exp_move_pct <- vol_ens    # since this is abs-return proxy
  exp_move_abs <- last_price * exp_move_pct
  
  cat("\n=== NVDA DAILY RISK REPORT ===\n")
  cat(sprintf("As of date           : %s\n", as.character(last_row$Date)))
  cat(sprintf("Last close price     : $%.2f\n", last_price))
  cat(sprintf("XGB predicted |r|    : %.2f%%\n", pred_vol * 100))
  cat(sprintf("EGARCH sigma (1d)    : %.2f%%\n", egarch_sig * 100))
  cat(sprintf("Ensemble vol (proxy) : %.2f%%\n", vol_ens * 100))
  cat(sprintf("Volatility regime    : %s\n", regime))
  cat(sprintf("Expected move (±)    : ~$%.2f (%.2f%%)\n",
              exp_move_abs, exp_move_pct * 100))
  cat("================================\n")
}

daily_nvda_risk_report()


# Save augmented dataset
out_file <- file.path(data_dir, "NVDA_volatility_analysis_with_predictions.csv")
write.csv(nvda_clean, out_file, row.names = FALSE)
cat("\nSaved enriched dataset to:\n", out_file, "\n", sep = "")
