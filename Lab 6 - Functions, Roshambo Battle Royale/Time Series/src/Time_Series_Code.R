############################################################
##  Intro to time series analysis in R
##  GISS Surface Temperature Analysis (GISTEMPv4) prepared by 
##  NASA Goddard Institute for Space Studies
##  Load data, decompose time series, fit ARIMA model and forecast, assess accuracy
##  Data available here: https://data.giss.nasa.gov/gistemp/
############################################################



############################################################
##  Load Libraries
##  ggplot2, forecast
############################################################

library(ggplot2)

library(forecast)


###########################################################
##  Set working directory
##  Load data
##  Create time series
###########################################################

path <- "/Volumes/KINGSTON/R Class/Lectures/Time Series/"
setwd(paste(path, "data/", sep = ""))

##  Read in csv
gt <- read.csv("GISTEMPv4.csv")

##  Remove years column
gt <- gt[,-1]

##  Transpose columns and create vector
gt <- as.vector(t(gt))

##  Create time series object from vector
gt.ts <- ts(gt, start = c(1880, 1), end = c(2018, 12), frequency = 12)

##  Plot time series
plot(gt.ts)


##########################################################
##  Explore time series
##  Decompose time series
##########################################################


##  Time series have 3 components: trend, seasonality, random error (irregular fluctuations)

##  Explore TREND
##  Fit regression to series
abline(reg = lm(gt.ts ~ time(gt.ts)), col = "red", lwd = 2)
##  Calculate annual mean
plot(aggregate(gt.ts, FUN = mean))
abline(reg = lm(gt.ts ~ time(gt.ts)), col = "red", lwd = 2)
##  Remove trend by taking first difference (change in value across months)
plot(diff(gt.ts))

##  Explore SEASONALITY
plot(gt.ts)
gt.subset <- window(gt.ts, start = c(2000, 1), end = c(2018, 12))
plot(gt.subset)
##  Create box plots for months
boxplot(gt.ts~cycle(gt.ts))
##  Try season plot from ggplot2 package
ggseasonplot(diff(gt.ts))

##  Another way of decomposing time series
gt.subset <- window(gt.ts, start = c(1980, 1), end = c(2018, 12))
gt.ts.decomp <- decompose(gt.subset, type = "multiplicative")
plot(gt.ts.decomp)


##########################################################
##  Fit ARIMA model to time series
##  Perform model diagnostics
##  Forecast future using model
##########################################################

##  ARIMA - Auto Regressive Integrated Moving Average
##  Data must be stationary - trend and seasonality must be removed
##  Can do this manually using diff() for trend and log for seasonality
##  Here we will use an automated method that takes care of stationarity for us

model <- auto.arima(gt.ts)
model

##  Examine model diagnostics
##  Plot ACF of model residuals to see if autocorrelation removed
##  Further test autocorrelation using Ljung-Box test
##  Examine histogram of residuals for Gaussian distribution

checkresiduals(model)

##  Forecast using ARIMA model and plot

##  Forecast out # of time steps
f <- forecast(model, h = 60)
summary(f)
str(f)
f$mean


##  Plot

plot(f)
autoplot(f)
autoplot(f, include = 240)

##  Tidy up the plot
autoplot(f, include = 240) + ggtitle("Global temperatures forecast") +
  xlab("Time (year)") + ylab("Temperatures (C) relative to 1950-1980 mean") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(from = 1998, to = 2024, by = 2))

##########################################################
##  Perform accuracy assessment
##########################################################

##  Assess model accuracy
accuracy(f)

##  Cross Validation
##  Create training and testing sets
train <- head(gt.ts, round(length(gt.ts) * 0.8))
t <- length(gt.ts) - length(train)
test <- tail(gt.ts, t)

## Plot training and test series
autoplot(train) + autolayer(test)

##  Re-fit ARIMA using training data only
modeltrain <- auto.arima(train)

##  Forecast through length of test set
ftrain <- forecast(modeltrain, h = length(test))

##  Assess accuracy of ARIMA model in simulating known test set
accuracy(ftrain, test)

## Plot
autoplot(train) + autolayer(ftrain) + autolayer(test)


##  Tidy up the plot a little
autoplot(ftrain$mean, ylab = "Temperatures (C) relative to 1950-1980 mean") + 
  geom_ribbon(aes(ymin = ftrain$lower[,1], ymax = ftrain$upper[,1]), alpha=0.25) +
  geom_ribbon(aes(ymin = ftrain$lower[,2], ymax = ftrain$upper[,2]), alpha=0.2) + 
  autolayer(test, series = "Test Data") + autolayer(train, series = "Training Data") + 
  autolayer(ftrain$mean, series = "Forecast") + 
  ggtitle("Global Temperatures Forecast Accuracy") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("red", "blue", "black"))

#########################################################
##  Save output and time series data
#########################################################

save.image(paste(path, "output/Global_Temperature.RData", sep = ""))

##  How to re-load a RData file
rm(gt.ts)
prior <- load(paste(path, "output/Global_Temperature.RData", sep = ""), verbose =TRUE)








