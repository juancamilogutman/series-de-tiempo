library(tseries)
library(mAr) 
library(forecast)
library(stats)
library(tseries)
library(urca)
library(readxl)
library(vars)
library(ggplot2)
library(dplyr)
library(openxlsx)
library (strucchange)
library (lubridate)
library (svars)
library(tsDyn)
library(corrplot)
library(patchwork)

#Code 1.1 Simulation of an AR(1) process with φ = 0.9
set.seed(123456)
y <- arima.sim(n = 100, list(ar = 0.9), innov=rnorm(100))
op <- par(no.readonly=TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(y, ylab='')
acf(y, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black")
pacf(y, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")
par(op)


#Code 1.2 Estimation of an AR(2) process with φ = 0.6 and φ = -0.28

series <-  rnorm(1000)
y.st <- arima.sim(n = 1000,list(ar = c(0.6,-0.28)))

              
ar2.st <- arima(y.st, c(2, 0, 0), include.mean=FALSE,
                transform.pars=FALSE, method="ML")
ar2.st$coef
polyroot(c(1, -ar2.st$coef))
Mod(polyroot(c(1, -ar2.st$coef)))
root.comp <- Im(polyroot(c(1, -ar2.st$coef)))
root.real <- Re(polyroot(c(1, -ar2.st$coef)))
# Plotting the roots in a unit circle
x <- seq(-1, 1, length = 1000)
y1 <- sqrt(1- x^2)
y2 <- -sqrt(1- x^2)
plot(c(x, x), c(y1, y2), xlab='Real part',
     ylab='Complex part', type='l',
     main='Unit Circle', ylim=c(-2, 2), xlim=c(-2, 2))
abline(h=0)
abline(v=0)
points(Re(polyroot(c(1, -ar2.st$coef))),
       Im(polyroot(c(1, -ar2.st$coef))), pch=19)
legend(-1.25, -1.25, legend="Roots of AR(2)", pch=19)

#Code 1.3 Box-Jenkins: U.S. unemployment rate
library(urca)
data(npext)
y <- ts(na.omit(npext$unemploy), start=1909, end=1988,
        frequency=1)
op <- par(no.readonly=TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot(y, ylab="unemployment rate (logarithm)")
acf(y, main='Autocorrelations', ylab='', ylim=c(-1, 1))
pacf(y, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1))
par(op)
## tentative ARMA(2,0)
arma20 <- arima(y, order=c(2, 0, 0))
ll20 <- logLik(arma20)
ll20
aic20 <- arma20$aic
aic20
res20 <- residuals(arma20)
Box.test(res20, lag = 20, type =  "Ljung-Box")
shapiro.test(res20)
## alternative specifications
## ARMA(3,0)
arma30 <- arima(y, order=c(3, 0, 0))
ll30 <- logLik(arma30)
ll30
aic30 <- arma30$aic
aic30
lrtest <- as.numeric(2*(ll30 - ll20))
chi.pval <- pchisq(lrtest, df = 1, lower.tail = FALSE)
chi.pval
## ARMA(1,1)
arma11 <- arima(y, order = c(1, 0, 1))
ll11 <- logLik(arma11)
ll11
aic11 <- arma11$aic
aic11
tsdiag(arma11)
res11 <- residuals(arma11)
Box.test(res11, lag = 20, type =  "Ljung-Box")
shapiro.test(res11)
tsdiag(arma11)
## Using auto.arima()
library(forecast)
auto.arima(y, max.p = 3, max.q = 3, start.p = 1,
           start.q = 1, ic = "aic")

#Box-Jenkins: Predictions of the U.S. unemployment rate
## Forecasts
arma11.pred <- predict(arma11, n.ahead = 10)
predict <- ts(c(rep(NA, length(y) - 1), y[length(y)],
                arma11.pred$pred), start = 1909,
              frequency = 1)
upper <- ts(c(rep(NA, length(y) - 1), y[length(y)],
              arma11.pred$pred + 2 * arma11.pred$se),
            start = 1909, frequency = 1)
lower <- ts(c(rep(NA, length(y) - 1), y[length(y)],
              arma11.pred$pred - 2 * arma11.pred$se),
            start = 1909, frequency = 1)
observed <- ts(c(y, rep(NA, 10)), start=1909,
               frequency = 1)
## Plot of actual and forecasted values
plot(observed, type = "l",
     ylab = "Actual and predicted values", xlab = "")
lines(predict, col = "blue", lty = 2)
lines(lower, col = "red", lty = 5)
lines(upper, col = "red", lty = 5)
abline(v = 1988, col = "gray", lty = 3)
