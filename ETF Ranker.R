library(quantmod)
library(data.table)
Sys.setenv(TZ = "EST5EDT")

##Download Quotes for Up Gaps
TDETFs <- read.csv("tdetfs.csv", sep = ",", header = FALSE)
TDETFs.tr = t(TDETFs)
ETFQuotes <- getQuote(TDETFs.tr, src = 'yahoo')
