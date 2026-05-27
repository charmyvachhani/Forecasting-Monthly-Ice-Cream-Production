## USE FORECAST LIBRARY.

library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/hp/Downloads")

# Create data frame.
icecream.data <- read.csv("ice_cream.csv")
names(icecream.data) <- c("DATE", "Sales")

icecream.ts <- ts(icecream.data$Sales,
                  start = c(1972, 1), end = c(2020, 1), freq = 12)

## ============================================================
## PLOT HISTORICAL ICE CREAM DATA (TRAIN / VALID / FORECAST SPANS)
## ============================================================

plot(icecream.ts,
     xlab = "Time", ylab = "Ice Cream Production Index",
     bty = "l", xaxt = "n", lwd = 2,
     main = "Monthly Ice Cream Production Index (1972–2020)")

# x-axis ticks every 4 years (like your other plots)
axis(1, at = seq(1972, 2022, 4), labels = format(seq(1972, 2022, 4)))

# Pick a top y position slightly above max for labels/arrows
y_top <- max(icecream.ts, na.rm = TRUE) * 1.05

# Define split times (same as your project: last 36 months as validation)
nValid <- 36
nTrain <- length(icecream.ts) - nValid
train.ts <- window(icecream.ts, start = c(1972, 1), end = c(1972, nTrain))
valid.ts <- window(icecream.ts, start = c(1972, nTrain + 1), end = c(1972, nTrain + nValid))

train.end.time  <- tail(time(train.ts), 1)    # end of training
valid.end.time  <- tail(time(valid.ts), 1)    # end of validation
future.end.time <- valid.end.time + 1.0       # 12 months ahead (future horizon)

# Vertical separators (start, validation start, forecast start)
abline(v = 1972, lwd = 1)
abline(v = train.end.time, lwd = 1)
abline(v = valid.end.time, lwd = 1)

# Labels
text((1972 + train.end.time) / 2, y_top, "Training")
text((train.end.time + valid.end.time) / 2, y_top, "Validation")
text(valid.end.time + 0.6, y_top, "Forecast")

# Span arrows
arrows(1972, y_top, train.end.time, y_top, code = 3, angle = 30, length = 0.10)
arrows(train.end.time, y_top, valid.end.time, y_top, code = 3, angle = 30, length = 0.10)
arrows(valid.end.time, y_top, future.end.time, y_top, code = 3, angle = 30, length = 0.10)

## ============================================================
## IDENTIFY TIME SERIES COMPONENTS USING ACF
## ============================================================

Acf(icecream.ts, lag.max = 48,
    main = "Autocorrelation (ACF) - Monthly Ice Cream Production Index")

## ==========================================
## 2) DEFINE TRAINING + VALIDATION PARTITIONS
## ==========================================

# Validation = last 36 months (Feb 2017 - Jan 2020)
nValid <- 36
nTrain <- length(icecream.ts) - nValid

train.ts <- window(icecream.ts, start = c(1972, 1), end = c(1972, nTrain))
valid.ts <- window(icecream.ts, start = c(1972, nTrain + 1), end = c(1972, nTrain + nValid))

## Compute split times for drawing vertical lines/arrows
train.end.time <- tail(time(train.ts), 1)
valid.end.time <- tail(time(valid.ts), 1)
future.end.time <- valid.end.time + 1.0

## ============================================================
## (A) HOLT-WINTER'S (HW) ETS(M,Ad,M) WITH PARTITIONED DATA
##     OPTIMAL PARAMETERS FOR ALPHA, BETA, GAMMA, PHI
## ============================================================

# Fit ETS(M,Ad,M) over the training period.
hw.MAdM <- ets(train.ts, model = "MAM")
hw.MAdM

# Forecast for validation period (nValid). Show table.
hw.MAdM.pred <- forecast(hw.MAdM, h = nValid, level = 0)
hw.MAdM.pred

# Plot predictions (validation forecast + fitted + original series)
plot(hw.MAdM.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index", ylim = c(40, 210),
     bty = "l", xlim = c(1972, future.end.time + 0.25), xaxt = "n",
     main = "Holt-Winter's ETS(M,Ad,M) with Optimal Smoothing Parameters",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1972, floor(future.end.time) + 1, 4),
     labels = format(seq(1972, floor(future.end.time) + 1, 4)))
lines(hw.MAdM.pred$fitted, col = "blue", lwd = 2)
lines(icecream.ts, col = "black", lwd = 2)
legend("topleft",
       legend = c("Ice Cream Index",
                  "HW ETS(M,Ad,M) for Training Partition",
                  "HW ETS(M,Ad,M) for Validation Partition"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

# Vertical lines + arrows for Training / Validation / Future intervals.
lines(c(train.end.time, train.end.time), c(0, 210))
lines(c(valid.end.time, valid.end.time), c(0, 210))
text(1985, 205, "Training")
text((train.end.time + valid.end.time)/2, 205, "Validation")
text(valid.end.time + 0.6, 205, "Future")
arrows(1972, 200, train.end.time - 0.05, 200, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(train.end.time + 0.05, 200, valid.end.time - 0.05, 200, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(valid.end.time + 0.05, 200, future.end.time + 0.25, 200, code = 3, length = 0.1, lwd = 1, angle = 30)

## ============================================================
## (C) FORECAST WITH ETS(M,Ad,M) USING ENTIRE DATA SET
##     INTO THE FUTURE FOR 12 PERIODS
## ============================================================

# Fit ETS(M,Ad,M) to entire data set.
HW.MAdM <- ets(icecream.ts, model = "MAM")
HW.MAdM

# Forecast 12 months into the future.
HW.MAdM.pred <- forecast(HW.MAdM, h = 12, level = 0)
HW.MAdM.pred

# Plot forecast for entire data set + future 12 months.
plot(HW.MAdM.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index", ylim = c(40, 210),
     bty = "l", xlim = c(1972, tail(time(HW.MAdM.pred$mean), 1) + 0.25), xaxt = "n",
     main = "Holt-Winter's ETS(M,Ad,M) for Entire Data Set and Forecast for Future 12 Periods",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1972, floor(tail(time(HW.MAdM.pred$mean), 1)) + 1, 4),
     labels = format(seq(1972, floor(tail(time(HW.MAdM.pred$mean), 1)) + 1, 4)))
lines(HW.MAdM.pred$fitted, col = "blue", lwd = 2)
lines(icecream.ts, col = "black", lwd = 2)
legend("topleft",
       legend = c("Ice Cream Index",
                  "HW ETS(M,Ad,M) for Entire Data Set",
                  "HW ETS(M,Ad,M) Forecast, Future 12 Periods"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

# Vertical line + arrows for Data Set / Future intervals.
data.end.time <- tail(time(icecream.ts), 1)
future.end2.time <- tail(time(HW.MAdM.pred$mean), 1)
lines(c(data.end.time, data.end.time), c(0, 210))
text(1995, 205, "Data Set")
text(data.end.time + 0.6, 205, "Future")
arrows(1972, 200, data.end.time - 0.05, 200, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(data.end.time + 0.05, 200, future.end2.time + 0.25, 200, code = 3, length = 0.1, lwd = 1, angle = 30)

## ============================================================
## (D) PERFORMANCE MEASURES (ENTIRE DATA SET FIT) + BENCHMARKS
## ============================================================

round(accuracy(HW.MAdM.pred$fitted, icecream.ts), 3)
round(accuracy((naive(icecream.ts))$fitted, icecream.ts), 3)
round(accuracy((snaive(icecream.ts))$fitted, icecream.ts), 3)



## ============================================================
## FIT REGRESSION MODEL WITH LINEAR TREND: (needed for accuracy comparisons)
## ============================================================
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred
## ============================================================
## FIT REGRESSION MODEL WITH QUADRATIC TREND: (needed for accuracy comparisons)
## ============================================================
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred
## ============================================================
## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 4
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.
## ============================================================

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data

# Apply forecast() function for validation set.
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

# x-axis range helpers (define if missing)
xStart <- floor(time(icecream.ts)[1])
xEnd   <- floor(future.end.time) + 1

# y-axis range helpers (define if missing)
yMin <- min(icecream.ts, na.rm = TRUE) - 5
yMax <- max(icecream.ts, na.rm = TRUE) + 5

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(yMin, yMax), bty = "l",
     xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Regression Model with Seasonality",
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend("topleft",
       legend = c("Ice Cream Time Series", "Seasonality Model for Training Data",
                  "Seasonality Forecast for Validation Data"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

# Vertical lines and arrows describing training, validation, future.
lines(c(train.end.time, train.end.time), c(yMin, yMax))
lines(c(valid.end.time, valid.end.time), c(yMin, yMax))
text(xStart + 10, yMax - 5, "Training")
text((train.end.time + valid.end.time)/2, yMax - 5, "Validation")
text(valid.end.time + 0.6, yMax - 5, "Future")
arrows(xStart, yMax - 10, train.end.time - 0.05, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(train.end.time + 0.05, yMax - 10, valid.end.time - 0.05, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(valid.end.time + 0.05, yMax - 10, future.end.time + 0.25, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the model with seasonality.
plot(train.season.pred$residuals,
     xlab = "Time", ylab = "Residuals",
     bty = "l", xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Residuals for the Seasonality Model",
     col = "brown", lwd = 2)
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(valid.ts - train.season.pred$mean, col = "brown", lty = 1, lwd = 2)

lines(c(train.end.time, train.end.time), c(min(train.season.pred$residuals, na.rm=TRUE) - 5,
                                           max(train.season.pred$residuals, na.rm=TRUE) + 5))
lines(c(valid.end.time, valid.end.time), c(min(train.season.pred$residuals, na.rm=TRUE) - 5,
                                           max(train.season.pred$residuals, na.rm=TRUE) + 5))

## ============================================================
## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 5
## ============================================================

train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

plot(train.lin.season.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(yMin, yMax), bty = "l",
     xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality",
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend("topleft",
       legend = c("Ice Cream Time Series",
                  "Linear Trend and Seasonality Model for Training Data",
                  "Linear Trend and Seasonality Forecast for Validation Data"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

lines(c(train.end.time, train.end.time), c(yMin, yMax))
lines(c(valid.end.time, valid.end.time), c(yMin, yMax))
text(xStart + 10, yMax - 5, "Training")
text((train.end.time + valid.end.time)/2, yMax - 5, "Validation")
text(valid.end.time + 0.6, yMax - 5, "Future")
arrows(xStart, yMax - 10, train.end.time - 0.05, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(train.end.time + 0.05, yMax - 10, valid.end.time - 0.05, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(valid.end.time + 0.05, yMax - 10, future.end.time + 0.25, yMax - 10, code = 3, length = 0.1,
       lwd = 1, angle = 30)

plot(train.lin.season.pred$residuals,
     xlab = "Time", ylab = "Residuals",
     bty = "l", xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Residuals for Linear Trend and Seasonality Model",
     col = "brown", lwd = 2)
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lty = 1, lwd = 2)

## ============================================================
## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 6
## ============================================================

train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.season)

train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

plot(train.quad.season.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(yMin, yMax), bty = "l",
     xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality",
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend("topleft",
       legend = c("Ice Cream Time Series",
                  "Quadratic Trend and Seasonality Model for Training Data",
                  "Quadratic Trend and Seasonality Forecast for Validation Data"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

lines(c(train.end.time, train.end.time), c(yMin, yMax))
lines(c(valid.end.time, valid.end.time), c(yMin, yMax))

plot(train.quad.season.pred$residuals,
     xlab = "Time", ylab = "Residuals",
     bty = "l", xlim = c(xStart, future.end.time + 0.25), xaxt = "n",
     main = "Residuals for Quadratic Trend and Seasonality Model",
     col = "brown", lwd = 2)
axis(1, at = seq(xStart, xEnd, 4), labels = format(seq(xStart, xEnd, 4)))
lines(valid.ts - train.quad.season.pred$mean, col = "brown", lty = 1, lwd = 2)

## ============================================================
## ACCURACY COMPARISONS (VALIDATION)
## ============================================================
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)
round(accuracy(train.quad.season.pred$mean, valid.ts), 3)

## ============================================================
## REGRESSION MODELS (ENTIRE DATA): ICE CREAM DATASET
## MODEL A: REGRESSION WITH SEASONALITY ONLY
## MODEL B: REGRESSION WITH QUADRATIC TREND AND SEASONALITY
## ============================================================

# Number of periods to forecast into the future (12 months)
h <- 12

## ============================================================
## MODEL A: REGRESSION WITH SEASONALITY (ENTIRE DATA)
## ============================================================

# Use tslm() function to create regression model with seasonality.
season.model <- tslm(icecream.ts ~ season)

# See summary of seasonality equation and associated parameters.
summary(season.model)

# Apply forecast() function to make predictions for future periods.
season.model.pred <- forecast(season.model, h = h, level = 0)
season.model.pred

## ============================================================
## MODEL B: REGRESSION WITH QUADRATIC TREND AND SEASONALITY (ENTIRE DATA)
## ============================================================

# Use tslm() function to create regression model with quadratic trend and seasonality.
quad.season.model <- tslm(icecream.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(quad.season.model)

# Apply forecast() function to make predictions for future periods.
quad.season.model.pred <- forecast(quad.season.model, h = h, level = 0)
quad.season.model.pred



## ============================================================
## SECTION: ARIMA / SARIMA MODELS – ICE CREAM DATASET
## ACF/PACF checks, auto.arima, manual SARIMA,
## validation accuracy, entire data fit + 12-month forecast)
## ============================================================

## ============================================================
## ACF/PACF CHECKS (TRAINING DATA)
## ============================================================

# ACF and PACF of original training series.
par(mfrow = c(1, 2))
Acf(train.ts,  lag.max = 48, main = "ACF - Original Training Series")
Pacf(train.ts, lag.max = 48, main = "PACF - Original Training Series")
par(mfrow = c(1, 1))

# First difference (d = 1) to remove trend.
train.diff1 <- diff(train.ts, lag = 1)
par(mfrow = c(1, 2))
Acf(train.diff1,  lag.max = 48, main = "ACF - First Difference (d=1)")
Pacf(train.diff1, lag.max = 48, main = "PACF - First Difference (d=1)")
par(mfrow = c(1, 1))

# Seasonal difference (D = 1, lag = 12) applied on top of first difference.
train.diff1.seas <- diff(train.diff1, lag = 12)
par(mfrow = c(1, 2))
Acf(train.diff1.seas,  lag.max = 48,
    main = "ACF - First + Seasonal Difference (d=1, D=1)")
Pacf(train.diff1.seas, lag.max = 48,
     main = "PACF - First + Seasonal Difference (d=1, D=1)")
par(mfrow = c(1, 1))

## ============================================================
## 8b. FIT AUTO ARIMA MODEL (TRAINING) + VALIDATION FORECAST
## ============================================================

train.auto.arima <- auto.arima(train.ts, seasonal = TRUE,
                               stepwise = FALSE, approximation = FALSE)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# ACF of residuals.
Acf(train.auto.arima$residuals, lag.max = 48,
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot auto ARIMA forecast vs validation.
plot(train.auto.arima.pred,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(40, 210), xaxt = "n",
     bty = "l", xlim = c(1972, future.end.time + 0.25),
     main = "Auto ARIMA Model (Training + Validation)", lwd = 2, flty = 5)
axis(1, at = seq(1972, floor(future.end.time) + 1, 4),
     labels = format(seq(1972, floor(future.end.time) + 1, 4)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend("topleft", legend = c("Ice Cream Series",
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 5), lwd = c(2, 2, 2), bty = "n")

# Vertical lines + arrows for Training / Validation / Future.
lines(c(train.end.time, train.end.time), c(0, 210))
lines(c(valid.end.time, valid.end.time), c(0, 210))
text(1985, 205, "Training")
text((train.end.time + valid.end.time)/2, 205, "Validation")
text(valid.end.time + 0.6, 205, "Future")
arrows(1972, 200, train.end.time - 0.05, 200, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(train.end.time + 0.05, 200, valid.end.time - 0.05, 200, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(valid.end.time + 0.05, 200, future.end.time + 0.25, 200, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Residual diagnostics (optional but professor-style)
checkresiduals(train.auto.arima)
Box.test(train.auto.arima$residuals, lag = 20, type = "Ljung-Box")

## ============================================================
## 8c. FIT MANUAL SARIMA MODEL (TRAINING) + VALIDATION FORECAST
## ============================================================

# OPTION 1 (common “starter”): SARIMA(1,1,1)(1,1,1)[12]
train.sarima <- Arima(train.ts,
                      order = c(1, 1, 1),
                      seasonal = list(order = c(1, 1, 1), period = 12),
                      method = "ML")
summary(train.sarima)

train.sarima.pred <- forecast(train.sarima, h = nValid, level = 0)
train.sarima.pred

Acf(train.sarima$residuals, lag.max = 48,
    main = "Autocorrelations of Manual SARIMA Model Residuals")

plot(train.sarima.pred,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(40, 210), xaxt = "n",
     bty = "l", xlim = c(1972, future.end.time + 0.25),
     main = "Manual SARIMA Model (Training + Validation)", lwd = 2, flty = 5)
axis(1, at = seq(1972, floor(future.end.time) + 1, 4),
     labels = format(seq(1972, floor(future.end.time) + 1, 4)))
lines(train.sarima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend("topleft", legend = c("Ice Cream Series",
                             "Manual SARIMA Forecast for Training Period",
                             "Manual SARIMA Forecast for Validation Period"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 5), lwd = c(2, 2, 2), bty = "n")

lines(c(train.end.time, train.end.time), c(0, 210))
lines(c(valid.end.time, valid.end.time), c(0, 210))

checkresiduals(train.sarima)
Box.test(train.sarima$residuals, lag = 20, type = "Ljung-Box")

## ============================================================
## 8d. VALIDATION ACCURACY COMPARISON (AUTO vs MANUAL SARIMA)
## ============================================================

round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
round(accuracy(train.sarima.pred$mean,     valid.ts), 3)

## ============================================================
## 8e. FIT BEST AUTO ARIMA/SARIMA TO ENTIRE DATA SET + 12-MONTH FORECAST
## ============================================================

# Auto ARIMA on entire data set.
arima.full <- auto.arima(icecream.ts, seasonal = TRUE,
                         stepwise = FALSE, approximation = FALSE)
summary(arima.full)

arima.full.pred <- forecast(arima.full, h = 12, level = 0)
arima.full.pred

plot(arima.full.pred,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(40, 210),
     main = "Auto ARIMA - Entire Data Set with 12-Month Forecast",
     xlim = c(1972, 2022), flty = 2, bty = "l", lwd = 2, xaxt = "n")
axis(1, at = seq(1972, 2022, 4), labels = format(seq(1972, 2022, 4)))
lines(arima.full.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(icecream.ts, col = "black", lty = 1, lwd = 2)

# Accuracy for entire data set fitted values + benchmarks.
round(accuracy(arima.full.pred$fitted, icecream.ts), 3)
round(accuracy((snaive(icecream.ts))$fitted, icecream.ts), 3)
round(accuracy((naive(icecream.ts))$fitted,  icecream.ts), 3)


## ============================================================
## FINAL ACCURACY TABLE (VALIDATION) – ALL MODELS
## Shows: ME RMSE MAE MPE MAPE ACF1 Theil's U
## ============================================================

# Collect accuracy rows (all are on VALIDATION set)
acc.hw            <- accuracy(hw.MAdM.pred$mean, valid.ts)
acc.reg.season    <- accuracy(train.season.pred$mean, valid.ts)
acc.reg.lin.seas  <- accuracy(train.lin.season.pred$mean, valid.ts)
acc.reg.quad.seas <- accuracy(train.quad.season.pred$mean, valid.ts)
acc.arima.auto    <- accuracy(train.auto.arima.pred$mean, valid.ts)
acc.sarima.manual <- accuracy(train.sarima.pred$mean, valid.ts)

# Combine into one table
acc.all <- rbind(
  "Holt-Winters ETS(MAM)"                 = acc.hw,
  "Regression (Seasonality)"              = acc.reg.season,
  "Regression (Linear Trend + Season)"    = acc.reg.lin.seas,
  "Regression (Quad Trend + Season)"      = acc.reg.quad.seas,
  "Auto ARIMA"                            = acc.arima.auto,
  "Manual SARIMA"                         = acc.sarima.manual
)

# Keep only the columns you asked for + round
acc.all <- acc.all[, c("ME","RMSE","MAE","MPE","MAPE","ACF1","Theil's U")]
round(acc.all, 3)

## ============================================================
## 8e. FIT BEST QUADRATIC TREND MODEL + AUTO ARIMA
## TO ENTIRE DATA SET + 12-MONTH FORECAST
## ============================================================

# ============================================================
# 8e(i). QUADRATIC TREND + SEASONALITY MODEL (ENTIRE DATA SET)
# ============================================================

# Number of periods to forecast into the future.
h <- 12

# Fit regression model with quadratic trend and seasonality to entire data set.
best.quad.model <- tslm(icecream.ts ~ trend + I(trend^2) + season)
summary(best.quad.model)

# Forecast next 12 months.
best.quad.model.pred <- forecast(best.quad.model, h = h, level = 0)
best.quad.model.pred

# Plot forecast for entire data set + future 12 months.
plot(best.quad.model.pred$mean,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(40, 210), bty = "l",
     xlim = c(1972, tail(time(best.quad.model.pred$mean), 1) + 0.25), xaxt = "n",
     main = "Quadratic Trend and Seasonality Model for Entire Data Set and Forecast for Future 12 Periods",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1972, floor(tail(time(best.quad.model.pred$mean), 1)) + 1, 4),
     labels = format(seq(1972, floor(tail(time(best.quad.model.pred$mean), 1)) + 1, 4)))
lines(best.quad.model.pred$fitted, col = "blue", lwd = 2)
lines(icecream.ts, col = "black", lwd = 2)
legend("topleft",
       legend = c("Ice Cream Index",
                  "Quadratic Trend and Seasonality Model for Entire Data Set",
                  "Quadratic Trend and Seasonality Forecast, Future 12 Periods"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2), lwd = c(2, 2, 2), bty = "n")

# Vertical line + arrows for Data Set / Future intervals.
data.end.time <- tail(time(icecream.ts), 1)
future.end.quad.time <- tail(time(best.quad.model.pred$mean), 1)
lines(c(data.end.time, data.end.time), c(0, 210))
text(1995, 205, "Data Set")
text(data.end.time + 0.6, 205, "Future")
arrows(1972, 200, data.end.time - 0.05, 200, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(data.end.time + 0.05, 200, future.end.quad.time + 0.25, 200, code = 3, length = 0.1, lwd = 1, angle = 30)

# Accuracy for entire data set fitted values + benchmarks.
round(accuracy(best.quad.model.pred$fitted, icecream.ts), 3)
round(accuracy((naive(icecream.ts))$fitted, icecream.ts), 3)
round(accuracy((snaive(icecream.ts))$fitted, icecream.ts), 3)

# ============================================================
# 8e(ii). AUTO ARIMA MODEL (ENTIRE DATA SET)
# ============================================================

# Auto ARIMA on entire data set.
arima.full <- auto.arima(icecream.ts, seasonal = TRUE,
                         stepwise = FALSE, approximation = FALSE)
summary(arima.full)

# Forecast 12 months into the future.
arima.full.pred <- forecast(arima.full, h = 12, level = 0)
arima.full.pred

# Plot forecast for entire data set + future 12 months.
plot(arima.full.pred,
     xlab = "Time", ylab = "Ice Cream Production Index",
     ylim = c(40, 210),
     main = "Auto ARIMA - Entire Data Set with 12-Month Forecast",
     xlim = c(1972, 2022), flty = 2, bty = "l", lwd = 2, xaxt = "n")
axis(1, at = seq(1972, 2022, 4), labels = format(seq(1972, 2022, 4)))
lines(arima.full.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(icecream.ts, col = "black", lty = 1, lwd = 2)

# Accuracy for entire data set fitted values + benchmarks.
round(accuracy(arima.full.pred$fitted, icecream.ts), 3)
round(accuracy((snaive(icecream.ts))$fitted, icecream.ts), 3)
round(accuracy((naive(icecream.ts))$fitted,  icecream.ts), 3)