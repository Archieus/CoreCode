library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)
library(tseries)

#alphavantage API = Y474

####Download ADJUSTED PRICE DATA from AlphaVantage
###outputsize=c(full,compact) full= 20 years of data, compact = 100 datapoints

#### Current Bitcoin Data from Coindesk ####
BTCcd <- read_json('https://api.coindesk.com/v1/bpi/currentprice.json')

BTCpx <- BTCcd$bpi$USD$rate_float

GBTC <- read.csv('http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=GBTC&outputsize=full&apikey=Y474&datatype=csv')
BTC <- read.csv('https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=BTC&market=CNY&apikey=Y474&datatype=csv')

GBTC.new <- cbind(GBTC[,c(2:5,7,6)])
rownames(GBTC.new) <- GBTC[,1]
GBTC.xts <- as.xts(GBTC.new)

BTC.u <- cbind(BTC[,c(6:10)])
rownames(BTC.u) <- BTC[,1]
BTC.USD <- as.xts(BTC.u)

CorrData <- na.omit(cbind(GBTC.xts[,4], BTC.USD[,4]))
names(CorrData) <- c("GBTC", "BTCUSD")

GBTC.log <- as.ts(log(CorrData[,1]))
BTC.log <- as.ts(log(CorrData[,2]))

RetData <- na.omit(Return.calculate(CorrData))
RunCor <- na.omit(runCor(RetData[,1],RetData[,2], n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE))

#### ADF Mean Reversion Test of the Spread ####
Model <- lm(log(CorrData$GBTC) ~ log(CorrData$BTCUSD) + 0, data = CorrData)
beta <- coef(Model)[1]

### Compute Spread to be used for ADF Test ###
sprd <- log(CorrData$GBTC) - beta*log(CorrData$BTCUSD)

### ADF Calculation ###
BTC.ADF <- suppressWarnings(adf.test(sprd, alternative = 'stationary', k = 1))

#### HALF LIFE OF MEAN REVERSION ####
sprd.lag <- lag(sprd, -1)
delta.sprd <- diff(sprd)

df <- cbind(sprd, sprd.lag, delta.sprd)
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(delta.sprd ~ sprd.lag, data = df)

lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda

#### GBTC Premium to BTC ####
TheoValue <- .09196847*BTCpx
GBTC.theo <- as.data.frame(coredata(as.numeric(TheoValue)))
rownames(GBTC.theo) <- Date
names(GBTC.theo) <- "TheoValue"
Theo.xts <- as.xts(GBTC.theo)


summary(RunCor)
table.Correlation(RetData[,1],RetData[,2])

last(RunCor)

summary(sprd)
last(sprd)
BTC.ADF

last(GBTC.theo)
last(GBTC.xts[,4])
