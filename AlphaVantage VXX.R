library(jsonlite)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)

Sys.setenv(TZ = "EST5EDT")
#alphavantage API = Y474

####Download ADJUSTED PRICE DATA from AlphaVantage
###outputsize=c(full,compact) full= 20 years of data, compact = 100 datapoints

y <- read_json('http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=VXX&outputsize=full&apikey=Y474', simplifyVector=FALSE)

#str(dat) #View Attributes so you can extract the data portion of JSON

y.js <- y$`Time Series (Daily)` #Extracts JSON "data" element.

#### Extract Open, high, Low, Close Adjusted Close and Volume Data from JSON data element.
y.Op <- sapply(y.js, function(a) as.numeric(a[[1]]))
y.Hi <- sapply(y.js, function(a) as.numeric(a[[2]]))
y.Lo <- sapply(y.js, function(a) as.numeric(a[[3]]))
y.Cl <- sapply(y.js, function(a) as.numeric(a[[4]]))
y.Ad <- sapply(y.js, function(a) as.numeric(a[[5]]))
y.Vo <- sapply(y.js, function(a) as.numeric(a[[6]]))

y <- as.xts(cbind(y.Op, y.Hi, y.Lo, y.Cl, y.Vo, y.Ad))
VXX <- y
names(VXX) <- c("VXX.Open", "VXX.High", "VXX.Low", "VXX.Close", "VXX.Volume", "VXX.Adjusted")

#### 30-day ADF ####
VXX.rolladf <- as.data.frame(rollapply(as.ts(VXX[,6]), 30, function(u) adf.test(u, k=1)$p.value))
VXX.adf <- adf.test(tail(as.ts(VXX[,6]), 30), alternative = 'stationary', k = 1)

print(VXX.adf)
mean(tail(VXX[,6]),30)
last(VXX[,6])