library(quantmod)
#alphavantage API = Y474

BTC <- read.csv('https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=BTC&market=CNY&apikey=Y474&datatype=csv')
ETH <- read.csv('https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=ETH&market=CNY&apikey=Y474&datatype=csv')
LTC <- read.csv('https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=LTC&market=USD&apikey=Y474&datatype=csv')

BTC.c <- cbind(BTC[,c(2:5,10)])
rownames(BTC.c) <- BTC[,1]
BTC.CNY <- as.xts(BTC.c)

BTC.u <- cbind(BTC[,c(6:10)])
rownames(BTC.u) <- BTC[,1]
BTC.USD <- as.xts(BTC.u)

ETH.c <- cbind(ETH[,c(2:5,10)])
rownames(ETH.c) <- ETH[,1]
ETH.CNY <- as.xts(ETH.c)

ETH.u <- cbind(ETH[,c(6:10)])
rownames(ETH.u) <- ETH[,1]
ETH.USD <- as.xts(ETH.u)

LTC.u <- cbind(LTC[,c(6:10)])
rownames(LTC.u) <- LTC[,1]
LTC.USD <- as.xts(LTC.u)
