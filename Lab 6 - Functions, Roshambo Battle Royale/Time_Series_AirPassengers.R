############################################################
##  Time Series Exercise
##  International air passenger data (1949-1960)
##  Data citation: Box, G. E. P., Jenkins, G. M. and Reinsel, G. C. (1976) 
##  Time Series Analysis, Forecasting and Control. Third Edition. Holden-Day. Series G.
############################################################

## Exercise Tasks:
## 1) Decompose time series by trend, seasonality, random error
## 2) Fit ARIMA model, check residuals, and forecast
## 3) Assess accuracy using cross validation
## 4) Plot forecast along with training and testing data
## 5) Save data to RData file

############################################################
##  Load Libraries
##  ggplot2, forecast
############################################################

library(ggplot2)

library(forecast)

###########################################################
##  Load data
##  Create time series
###########################################################

##  These data are included in R and so can be loaded using data()
data("AirPassengers")
AP <- AirPassengers

png("/Volumes/KINGSTON/R Class/Lectures/Time Series/AP_ts.png", 4000, 4000, res = 600)
plot(AP)
dev.off()

##########################################################
##  Explore time series
##  Decompose time series
##########################################################

## Explore TREND
abline(reg = lm(AP~time(AP)), col = "red", lwd = 2)
plot(aggregate(AP, FUN = mean))
plot(diff(AP))


## Explore SEASONALITY
AP.subset <- window(AP, start = c(1954, 1), end = c(1958, 12))
plot(AP.subset)
##  Create box plots for months
boxplot(AP~cycle(AP))
## Try season plot from ggplot2 package
ggseasonplot(diff(AP))
plot(log(AP))

##  Decompose time series
AP.decomp <- decompose(AP, type = "multiplicative")
plot(AP.decomp)
plot(diff(log(AP)))



##########################################################
##  Fit ARIMA model to time series
##  Perform model diagnostics
##  Forecast future using model
##########################################################

##  Fit ARIMA model
model <- auto.arima(AP)
checkresiduals(model)

##  Fitted ARIMA residuals failed autocorrelation Ljung-Box test (p-value<0.05)
##  HINT: Fix this by taking log to remove seasonality
model <- auto.arima(log(AP))
checkresiduals(model)

##  Forecast using ARIMA model
f <- forecast(model, h = 60)
summary(f)
##  Plot forecast, note log scale on y-axis
autoplot(f)

##  Tidy up the plot a little
autoplot(exp(f$mean), ylab = "International Air Passengers (1000s)") + 
  geom_ribbon(aes(ymin = exp(f$lower[,1]), ymax = exp(f$upper[,1])), alpha=0.25) +
  geom_ribbon(aes(ymin = exp(f$lower[,2]), ymax = exp(f$upper[,2])), alpha=0.2) + 
  autolayer(AP, series = "Observed Data") + 
  autolayer(exp(f$mean), series = "Forecast") + 
  ggtitle("Air Passengers Forecast Accuracy") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("red", "black"))

##########################################################
##  Perform accuracy assessment
##########################################################

##  Cross Validation
##  Create training and testing sets
train <- head(log(AP), round(length(log(AP)) * 0.8))
t <- length(log(AP)) - length(train)
test <- tail(log(AP), t)
##  Plot training and test series
autoplot(train) + autolayer(test)
##  Re-fit ARIMA using training data only
modeltrain <- auto.arima(train)
##  Forecast through length of test set
ftrain <- forecast(modeltrain, h = length(test))
##  Assess accuracy of ARIMA model in simulating known test set
accuracy(ftrain, test)

##  Plot
autoplot(train) + autolayer(ftrain) + autolayer(test)

##  Tidy up the plot a little
autoplot(exp(ftrain$mean), ylab = "International Air Passengers (1000s)") + 
  geom_ribbon(aes(ymin = exp(ftrain$lower[,1]), ymax = exp(ftrain$upper[,1])), alpha=0.25) +
  geom_ribbon(aes(ymin = exp(ftrain$lower[,2]), ymax = exp(ftrain$upper[,2])), alpha=0.2) + 
  autolayer(exp(test), series = "Test Data") + autolayer(exp(train), series = "Training Data") + 
  autolayer(exp(ftrain$mean), series = "Forecast") + 
  ggtitle("Air Passengers Forecast Accuracy") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("red", "blue", "black"))

#########################################################
##  Save output and time series data
#########################################################

save.image("/Volumes/KINGSTON/R Class/Lectures/Time Series/AP.RData", sep="")


