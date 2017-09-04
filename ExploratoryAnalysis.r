library(dplyr)
library(zoo)
library(forecast)

data <- read.csv("crypto-markets.csv")
data$date <- as.Date(data$date, format="%Y-%m-%d")

btc <- data %>% filter(coin=="Bitcoin (BTC)")
eth <- data %>% filter(coin == "Ethereum (ETH)")
dash <- data %>% filter(coin == "Dash (DASH)")

library(ggplot2)
ggplot() + geom_line(data = eth, aes(x = date, y = close), color = "red") + geom_line(data = btc, aes(x = date, y = close), color = "blue") + geom_line(data=dash, aes(x = date, y = close), color = "green") + xlab("Date") + ylab("Closing Price")


closeCorr <- cor(tail(btc$close, length(eth$close)), eth$close)
volumeCorr <- cor(tail(btc$volume, length(eth$volume)), eth$volume)
sprintf("The correlation between the closing price of btc and eth is %f", closeCorr)
sprintf("The correlation between the volume of btc and eth is %f", volumeCorr)


btcPercChange <- 100 * (btc$close[1] - btc$close[length(btc$close)])/btc$close[length(btc$close)]
ethPercChange <- 100 * (eth$close[1] - eth$close[length(eth$close)])/eth$close[length(eth$close)]

#Setup for ARIMA Model
btcCloseDf <- subset(btc, select = c("date", "close"))
btcCloseZoo <- read.zoo(btcCloseDf, format = "%Y-%m-%d")

#Simple returns
btcReturns <- diff(diff(log(btcCloseZoo)))/lag(btcCloseZoo, k = -1) * 100
pacf(btcReturns)
acf(btcReturns)



print(btcReturns)
# btcReturns$y <- lapply(btcReturns$y, logFunction)
#Creating Model
model <- auto.arima(btcReturns, stationary = TRUE, seasonal = TRUE, ic="aic")

print(model)
print(confint(model))

#Model diagnostic checking
tsdiag(model)

#Plotting
plot(btcReturns, lty = 1, main = "BTC Close: raw data vs fitted", ylab = "Log Returns", xlab = "Date")
lines(fitted(model), lty = 2, lwd = 2, col = "blue")

print(accuracy(model))
