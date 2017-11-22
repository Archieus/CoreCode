library(quantmod)
library(Quandl)
library(tseries)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = "UTC")

##RELATIVE MOM = Compare the share price movement of one company to the price movement of another Co.
##ABSOLUTE MOM = Compare the movement of a company's share price with the return of a short govt bond.

#### reads/loads CSV into R ####
Descr <- read.csv('ETF-Names.csv', header = FALSE)

Datalist <- read.table('MomPlus', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)

Daily.Ret <- na.omit(Return.calculate(DLxts,"discrete"))

#chart.Correlation(Daily.Ret,30)
DLxts.cor <- table.Correlation(Daily.Ret,Daily.Ret)

