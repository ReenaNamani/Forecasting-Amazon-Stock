##Using Forecast and ZOO library:

library(forecast)
library(zoo)

# Set working directory for locating files.
setwd("C:/Users/o/Desktop/My subjects/Third Semester/Capstone/Dataset ")

##Create data frame for Amazon Stock Prices:
Amazon.Data <- read.csv("AMAZON_monthly.csv")
Amazon.Data

# See the first and last 6 records of the file for Amazon data.
head(Amazon.Data)
tail(Amazon.Data)



##ts() function is used to create time series data set.
Amazon.ts <- ts(Amazon.Data$AverageStockPrice, 
                   start = c(1998, 10), end = c(2023,9) , freq = 12)
Amazon.ts


##Plotting the historical data using plot function: (Visualization)
plot(Amazon.ts, main="Data Plot for Historical data of Amazon stock prices",ylab ="Stock Price in USD")

#stl() component is used to plot time series components(seasonality, trend and level) of the original data:
#(Visualization)
Stock.stl <- stl(Amazon.ts, s.window="periodic")
autoplot(Stock.stl, main="Time Series Components of Stock price")

# Use Arima() function to fit AR(1) model.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
# ARIMA (1,0,0) model is an autoregressive model of order 1 without differencing and without average moving component.
Stock.ar1<- Arima(Amazon.ts, order = c(1,0,0))
summary(Stock.ar1)

###Checking predictability using hypothesis testing method:

#Predictability test approach-1 

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9971
s.e. <- 0.0032
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

#Predictability test approach-2

# Create first difference of ClosePrice data using diff() function.
diff.stock.price <- diff(Amazon.ts, lag = 1)
diff.stock.price

# Use Acf() function to identify autocorrealtion for first differenced
# Stock Price and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.stock.price, lag.max = 12, 
    main = "Autocorrelation for Amazon Stock Prices")


## Creating Time Series Partition.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(ridership.ts) = 300.
# nTrain = 240 months, from October 1998 to September 2018.
# nvalid = 60 months, from October 2018 to September 2023.

nValid <- 60
nTrain <- length(Amazon.ts) - nValid
nTrain
train.ts <- window(Amazon.ts, start = c(1998, 10), end = c(1998, nTrain))
valid.ts <- window(Amazon.ts, start = c(1998, nTrain + 1), 
                   end = c(1998, nTrain + nValid))

####MODEL ONE:


## USE TWO-LEVEL MODEL, REGRESSION WITH LINEAR TREND AND 
## SEASONALITY AND TRAILING MA FOR RESIDUALS FOR ENTIRE 
## DATA SET. USE TWO-LEVEL (COMBINED) FORECAST TO FORECAST 
## 12 FUTURE PERIODS FROM APRIL 2023.

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(Amazon.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, 
                                level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, 
                             align = "right")
tot.ma.trail.res

# Identify two-level forecast, regression with linear trend and 
# seasonality and trailing MA for regression residuals, for 
# entire data set (training period).
train.fst.2level <- tot.trend.seas.pred$fitted+tot.ma.trail.res
train.fst.2level

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, 
                                  level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + 
  tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, 
                                tot.ma.trail.res.pred$mean, tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", 
                        "Combined.Fst")
future12.df

# Plot historical data set and two-level model's forecast for
# entire data set and future 12 monthly periods 
plot(Amazon.ts, 
     xlab = "Time", ylab = "StockPrice", ylim = c(0, 180), 
     bty = "l", xlim = c(1998, 2023.75), lwd =1, xaxt = "n",
     main = "Two-Level Model: Regression with Trend and Seasonality + Trailing MA for Residuals") 
axis(1, at = seq(1998, 2023.75, 1), labels = format(seq(1998, 2023.75, 1)))
lines(train.fst.2level, col = "blue", lwd = 2)
lines(tot.fst.2level, col = "blue", lty =5, lwd = 2)
legend(1998,170, legend = c("Original Stock Price from historical data", "Two-Level Model for Entire Data Set",
                             "Two-Level Model for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023.75, 2023.75), c(0, 150))
text(1998, 170 , "DataSet")
text(2023.75, 170, "Future")
arrows(1998, 170, 2023.75, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.75, 170, 2025, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# two-level model's residuals. 
Acf((Amazon.ts - train.fst.2level), lag.max = 12, 
    main = "Autocorrelations of Two-Level Model's Residuals")

### Model 2:


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 MONTHLY PERIODS IN Oct 2023- Sep 24.

# Create Holt-Winter's (HW) exponential smoothing for 
# entire data set. Use ets() function with model = "ZZZ" to 
# identify the best HW options and optimal alpha, beta, & gamma 
# to fit HW for the entire data set.
HW.ZZZ <- ets(Amazon.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this 
# HW model for upcoming 12 months with no confidence interval.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Plot HW model's predictions for historical data set and
# future 12 months.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "StockPrice in USD", ylim = c(0, 170), 
     bty = "l", xlim = c(1998, 2023.75),lwd=1, xaxt = "n",
     main = "Holt-Winter's Automatic Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue") 
axis(1, at = seq(1998, 2023.75, 1), labels = format(seq(1998, 2023.75, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(Amazon.ts)
legend(1998,170, 
       legend = c("Original Stock Price from historical data", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023.75, 2023.75), c(0, 170))
text(1998, 170, "Data Set")
text(2023.75, 170, "Future")
arrows(1998, 170, 2023.75, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.75, 170, 2025, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Use Acf() function to create autocorrelation chart of 
# residuals of HW model's with automated options and parameters. 
Acf(HW.ZZZ.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of Residuals of HW Model with Automatic Options")

###Model 3:

## FIT TWO-LEVEL MODEL, LINEAR TREND AND SEASONALITY REGRESSION 
## AND AR(1) FOR REGRESSION RESIDUALS, FOR ENTIRE DATA SET. 


# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(Amazon.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months of 2023-2024  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into 
# the future 12 months of 2023-2024.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)
residual.ar1.pred

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Create two-level model's forecast for the entire data set (training
# period).
train.lin.season.ar1.pred <- lin.season$fitted + residual.ar1$fitted
train.lin.season.ar1.pred

# Identify forecast for the future 12 periods of 2023-2024 as sum of 
# linear trend and seasonality model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(Amazon.ts, 
     xlab = "Time", ylab = "StockPrice in USD", 
     ylim = c(0, 170), xaxt = "n",
     bty = "l", xlim = c(1998, 2023.75), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(1998, 2023.75, 1), labels = format(seq(1998, 2023.75, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1998,170, legend = c("Historical Data for Training and Valiadaton Periods", 
                             "Two-Level Forecast for Training and Valiadtion Periods", 
                             "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023.75, 2023.75), c(0, 170))
text(1998, 170, "Data Set")
text(2023.75, 170, "Future")
arrows(1998, 170, 2023.75, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.75, 170, 2024, 140, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for residuals of
# two-level model for different lags (up to maximum of 12).
Acf((Amazon.ts - train.lin.season.ar1.pred), lag.max = 12, 
    main = "Autocorrelation for Two-Level Model's Residuals for Entire Data Set")

### Model 4:

## FIT AUTO ARIMA MODEL FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters 
# for entire data set.
auto.arima <- auto.arima(Amazon.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(Amazon.ts, 
     xlab = "Time", ylab = "StockPrice", 
     ylim = c(0, 170), xaxt = "n", 
     bty = "l", xlim = c(1998, 2023.75), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1998, 2023.75, 1), labels = format(seq(1998, 2023.75, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1998,170, legend = c("Historical Data for Stock Price", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2023.75, 2023.75), c(0, 170))
text(1998, 170, "Data Set")
text(2023.75, 170, "Future")
arrows(1998, 170, 2023.75, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.75, 170, 2025, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model's Residuals")


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET USING VARIOUS METHODS.

# Use accuracy() function to identify common accuracy measures for:
# (1) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Trailing MA for Regression Residuals,
# (2) Holt-Winter's Model with Automatic Selection of Model Options
#     and Parameters, 
# (3) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Ar(1) Model for Regression Residuals,
# (4) Auto ARIMA Model,
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, Amazon.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, Amazon.ts), 3)
round(accuracy(lin.season$fitted + residual.ar1$fitted, Amazon.ts), 3)
round(accuracy(auto.arima.pred$fitted, Amazon.ts), 3)




