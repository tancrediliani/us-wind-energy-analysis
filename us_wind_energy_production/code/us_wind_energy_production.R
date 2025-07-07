# TIME SERIES ANALYSIS: US Wind Energy Production
# Data source: EIA (Independent Statistics and Analysis) US Energy Information
# Administration --> Electricity Data Browser
# Variable: Electricity generation from wind
# Series type: monthly
# Unit of measure: MWh*1e3 = GWh
# Notes: Currently the USA is the 2nd country in the world for wind
# electricity production (equal to 21% of world production in 2021).
# Growing sector due to technological advancement that has significantly
# reduced costs, also thanks to funds allocated by the US government
# starting from 2009 (Obama administration year) consistently.
############################
rm(list=ls())

# Libraries and sources
library(forecast)
library(lmtest)       
library(tsoutliers)   
library(FinTS)

source("TSA-Predict-Student-Functions.R")
source("TSA-Useful-Functions.R")
source("CalendarEffects-Student-Functions.R")

# Import dataset, extract start date and create time series
data_file <- read.delim("../data/US-Wind-MWh_1000.txt")
#View(data_file)
#head(data_file)
date <- as.Date(x = data_file$date, format = "%Y-%m-%d")  # Date format transformation
y <- data_file$y  # Extract response variable
start <- as.numeric(c(format(date[1], "%Y"), format(date[1], "%m"))) 
y <- ts(data = y, start = start, frequency = 12)

# PRELIMINARY ANALYSIS

# Plot
par(mfrow = c(1,1))
plot(date, y, type = "l", main = "Original TS", ylab = "MWh*1000", xlab = "", lwd = 1)
#plot(decompose(y, type = "additive"))
plot(decompose(y, type = "multiplicative"))  # This one is better

# ACF, PACF
lag <- NROW(y)/4
par(mfrow = c(3,1), mar = c(3,4,3,1))
plot(date, y, type = "l", main = "Original TS", ylab = "MWh*1000")
Acf(y, lag.max = lag, type = "correlation", main = "Acf", ylab = "")
Acf(y, lag.max = lag, type = "partial", main = "Pacf", ylab = "")
# From the ACF plot we notice a decrease in which the linear component 
# prevails over the seasonal one.

# Unit Root Tests
adf <- ur.df(y = y, type = "none", lags = 24, selectlags = "AIC")

# Set d=1
ystar <- diff(y, 1)  # y[2:n]-y[1:n-1] subtracts the previous value
par(mfrow = c(3,1))
plot(ystar, main = "ystar", ylab = "MWh*1000")
Acf(ystar, lag.max = lag, type = "correlation", main = "Acf", ylab = "")
Acf(ystar, lag.max = lag, type = "partial", main = "Pacf", ylab = "")
adf.1 <- ur.df(y = ystar, type = "none", lags = 24, selectlags = "AIC")
# From the ACF plot we notice a seasonal component, the linear decrease
# has been removed

# Set D=1
ystar2 <- diff(ystar, 12)  # 12-month differencing
par(mfrow = c(3,1), mar = c(3,4,3,1))
plot(ystar2, main = "ystar2", ylab = "MWh*1000")
Acf(ystar2, lag.max = lag, type = "correlation", main = "Acf")
Acf(ystar2, lag.max = lag, type = "partial", main = "Pacf")
adf.2 <- ur.df(y = ystar2, type = "none", lags = 24, selectlags = "AIC")

# KPSS Test
cat("\n-----\nKPSS with tau\n")
kpss.1 <- ur.kpss(y = y, type = "mu", lags = "long", use.lag = NULL)
print(data.frame(teststat = kpss.1@teststat, kpss.1@cval, check.names = FALSE))

kpss.2 <- ur.kpss(y = ystar, type = "mu", lags = "long", use.lag = NULL)
print(data.frame(teststat = kpss.2@teststat, kpss.2@cval, check.names = FALSE))

kpss.3 <- ur.kpss(y = ystar2, type = "mu", lags = "long", use.lag = NULL)
print(data.frame(teststat = kpss.3@teststat, kpss.3@cval, check.names = FALSE))

##############################################################

# ARIMA (0, 1, 0) x (0, 1, 0)[12]
xreg <- NULL
fit00 <- Arima(log(y), order = c(0,1,0),
              seasonal = list(order = (c(0,1,0))),
              xreg = xreg, include.constant = FALSE
)
res00 <- residuals(fit00)
par(mfrow = c(3,1))
main <- "residuals"
plot(res00, type = "l", main = main, ylab = "")
Acf(x = res00, type = "correlation", lag.max = 60, na.action = na.pass, main = "Acf", ylab = "")
Acf(x = res00, type = "partial", lag.max = 60, na.action = na.pass, main = "Pacf", ylab = "")

# ARIMA (0, 1, 0) x (0, 1, 1)[12]
xreg <- NULL
fit0 <- Arima(y, order = c(0,1,0),
             seasonal = list(order = (c(0,1,1))),
             xreg = xreg, include.constant = FALSE
)
print(lmtest::coeftest(fit0))

# AIC=4542.4   AICc=4542.45   BIC=4549.45

# ARIMA (0, 1, 1) x (0, 1, 1)[12] on log(y)
fit <- Arima(log(y), order = c(0,1,1),
            seasonal = list(order = (c(0,1,1))),
            xreg = NULL, include.constant = FALSE
)
print(lmtest::coeftest(fit))

summary(fit)
# AIC=-343.43   AICc=-343.33   BIC=-332.85
#names(fit)
#fit$fitted
#par(mfrow=c(1,1))
#plot(date, y, type="l")
#lines(date, exp(fit$fitted), col="red", type="l")
#legend("topleft", legend=c("Original", "ARIMA Estimate"), col=c("black", "red"),
#       lwd=1)

# Residuals of the fit
res1 <- residuals(fit)                          

#### Time series plot, ACF, PACF of residuals
par(mfrow = c(3,1))
main <- "residuals"
x1 <- res1
plot(x1, type = "l", main = main, ylab = "MWh*1000")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = "Acf")
Acf(x = x1, type = "partial", lag.max = 60, na.action = na.pass, main = "Pacf")

#### Time series plot, ACF, PACF of |residuals|
par(mfrow = c(3,1))
main <- "|residuals|"
x1 <- abs(res1)
plot(x1, type = "l", main = main, ylab = "MWh*1000")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = "")
Acf(x = x1, type = "partial", lag.max = 60, na.action = na.pass, main = "")

#### Time series plot, ACF, PACF of residuals^2
par(mfrow = c(3,1))
main <- "residuals^2"
x1 <- res1^2
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial", lag.max = 60, na.action = na.pass, main = main)

par(mfrow = c(2,1))
plot(res1^2, type = "l", main = "residuals^2", ylab = "MWh*1000")
plot(abs(res1), type = "l", main = "|residuals|", ylab = "MWh*1000")

par(mfrow = c(1,2))
hist(x = res1, breaks = 25, freq = FALSE, main = "residuals", xlab = "")
x1 <- seq(from = min(res1), to = max(res1)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = mean(res1), sd = sd(res1)), col = "red", lwd = 2)
qqnorm(y = res1, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(res1, col = "red", lwd = 2)

# Normality test 
print(shapiro.test(x = res1))

# Homoskedasticity check
#help(ArchTest)
# H0: delta_1=...=delta_H=0 (homosked.) vs H1: exists j such that delta_j!=0
ArchTest(res1, lags = 12, demean = F)
ArchTest(res1, lags = 24, demean = F)
ArchTest(x = sqrt(abs(res1)), lags = 12, demean = F) 
ArchTest(x = sqrt(abs(res1)), lags = 24, demean = F)

# In all tests performed we do not reject the null hypothesis of homoskedasticity

# Outlier detection
#source("~/Downloads/TSA-Useful-Functions-2.R")
#settings <- .Arima.settings(fit = fit)
#xreg <- fit$xreg
#afit <- tso(log(y), xreg = xreg,
#           types = c("IO","LS", "AO"), delta = 0.7, cval = 2.9,
#           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#           tsmethod = "arima", args.tsmethod = list(order = 
#          settings$order, seasonal = settings$seasonal))

# No outliers detected

###########################################################
# FORECASTING

H <- 12  # Forecast horizon
t1 <- NROW(y)  # Last observation in data

# .predict
xreg <- NULL
pred <- .predict(object = fit, n.ahead = H, t = t1, y = log(y), xreg = xreg, fixed.n.ahead = F)
#pred_log <- .predict(object = fit, n.ahead = H, t = t1, y = log(y), xreg = xreg, fixed.n.ahead = F)
# Clarification needed: should we keep y=y or should we select y=log(y)?

# Naive prediction
pred_naive <- .predict.naive(fit = fit, J = 0, n.ahead = H, g = "log")
pred_naive

# Confidence bands
b1 <- .pred.bands(pred = pred, alpha = .05, g = "log")
b1$mean

# Plot confidence bands
par(mfrow = c(1,1))
time1 <- .extend.time(x = date, n.ahead = H, by = "month")
plot(x = time1, y = b1$mean, type = "l", ylim = range(b1$lower, b1$upper), lwd = 2, ylab = "MWh*1000",
     main = "Ex-ante Forecast")
lines(time1, b1$lower, type = "l", lty = 4, col = "red")
lines(time1, b1$upper, type = "l", lty = 4, col = "red")

############################
# Ex-post 

# Settings
J  <- 12                                             
H  <- 1                                              
t1 <- .predict.t1(nobs = NROW(y), J = J, n.ahead = H)

#### No external regressors
pred1.1 <- .predict(object = fit, n.ahead = H, t = t1, y = log(y),
                    fixed.n.ahead = TRUE)
pred1.1

# Naive prediction
predn.1 <- .predict.naive(fit = fit, J = J, n.ahead = H, g = "log")

b1.1 <- .pred.bands(pred = pred1.1, alpha = 0.05, g = "log")
b1.1

# Ex-post prediction bands
par(mfrow = c(1,1))
ind <- (NROW(y) - J + 1) : NROW(y)
ind
time2 <- date[ind]
plot(x = time2, y = y[ind], type = "l", ylim = range(b1.1$lower, b1.1$upper), lwd = 2, ylab = "MWh*1000",
     main = "Ex-post Forecast")

lines(x = time2, y = b1.1$mean, col = "blue", lwd = 2) 
lines(time2, b1.1$lower, lty = 4, col = "red")
lines(time2, b1.1$upper, lty = 4, col = "red")
lines(time2, predn.1, col = "green")  # naive
legend(x = "topright", legend = c("estimated data", "observed data", "prediction bands",
                                 "naive estimate"), col = c("blue", "black", "red", "green"),
                                 lty = c(1,1,4,1), lwd = c(2,2,1,1), bty = "n")               

# Error Measures
em  <- .ErrorMeasures(y = y, fit = b1.1$mean, naive = predn.1)
em
emn <- .ErrorMeasures(y = y, fit = predn.1, naive = predn.1)
emn

################################################################################