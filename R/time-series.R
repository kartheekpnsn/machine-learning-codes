library(forecast)
library(TTR)

data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
print("CONVERT DATA TO A TIME SERIES DATA")
data = ts(data[, 2], start = c(2003, 1), frequency = 12)
data = tsclean(data) # cleans data outliers
plot(data, xlab = 'Years', ylab = 'Tractor Sales', main = '1. Simple Plot')
x11()
plot(diff(data), ylab = 'Differenced Tractor Sales', main = "2. PLOT THE FIRST DIFFERENCE AND CHECK THE SERIES IS STATIONARY OR NOT")
x11()
plot(log10(data), ylab = 'Log (Tractor Sales)', main = "3. STILL NOT STATIONARY - REASON: NOT CONSTANT VARIANCE; Apply log10")
x11()
plot(diff(log10(data)), ylab = 'Differenced Log (Tractor Sales)', main = "4. NOW IT IS STATIONARY")
x11()
par(mfrow = c(1, 2))
acf(ts(diff(log10(data))), main = 'ACF Tractor Sales')
pacf(ts(diff(log10(data))), main = 'PACF Tractor Sales')
title("5. OBSERVE THAT THERE ARE MANY PEAKS ABOVE THE INSIGNIFICANT LEVEL (BLUE LINE)", outer = TRUE)

# # # ARIMA METHOD # # #
fit = auto.arima(log10(data), approximation = FALSE, trace = FALSE)
summary(fit)

pred = predict(fit, n.ahead = 36) # for 3 years (3 * 12 = 36)
plot(data, type = 'l',xlim = c(2004, 2018), ylim = c(1, 1600), xlab = 'Year', ylab = 'Tractor Sales', main = '6. Forecasted (direct, direct - se, direct + se)')
lines(10 ^ (pred$pred), col = 'blue') # Since log10 to make it stationary.. apply 10 ^ to me it non-stationary
lines(10 ^ (pred$pred + 2 * pred$se), col = 'orange') # subtract standard error
lines(10 ^ (pred$pred - 2 * pred$se), col = 'orange') # add standard error


par(mfrow = c(1, 2))
acf(ts(fit$residuals), main = 'ACF Residual')
pacf(ts(fit$residuals), main = 'PACF Residual')
title("7. Check if any more info left to be extracted", outer = TRUE)


# # # Exponential Smoothing - HOLT WINTERS METHOD # # #
rain = scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries = ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts = HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
# (OR)
# rainseriesforecasts = HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start = 26)
print(rainseriesforecasts)
print(rainseriesforecasts$SSE)
print(rainseriesforecasts$fitted)
plot(rainseriesforecasts)

rainseriesforecasts2 = forecast.HoltWinters(rainseriesforecasts, h=8)
print(rainseriesforecasts2)
plot.forecast(rainseriesforecasts2)
plot.ts(rainseriesforecasts2$residuals)
acf(rainseriesforecasts2$residuals, lag.max = 20)
