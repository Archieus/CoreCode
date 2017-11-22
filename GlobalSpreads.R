library(quantmod)
library(tseries)

getSymbols('DGS10', src = 'FRED', return.class = 'xts') # DATA FROM FRED 

ECB10Y <- read.csv("data.csv", sep = ",", skip  = 4) #DATA FROM: www.ecb.europa.eu
ENG10Y <- read.csv("results.csv", sep = ",", skip = 2) # DATA FROM: http://www.bankofengland.co.uk

#https://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Macro_economic_time_series/
#its_details_value_node.html?tsId=BBK01.WT1010

BUND10 <- read.csv("BBK01.WT1010.csv", sep = ",", skip =5) 

EU10Yzoo <- read.zoo(ECB10Y, sep = ",", format = "%Y-%m-%d")
UK10Yzoo <- read.zoo(ENG10Y, sep = ",", format = "%d %B %Y")
DB10Yzoo <- read.zoo(BUND10, sep = ",", format = "%Y-%m-%d")

DB10Y.df <- suppressWarnings(as.data.frame(as.numeric(DB10Yzoo[,1])))
row.names(DB10Y.df) <- index(DB10Yzoo)

Compzoo <- suppressWarnings(na.omit(cbind(EU10Yzoo, UK10Yzoo, DB10Yzoo[,1])))
Comp.x <- as.xts(Compzoo)
Comp10 <- suppressWarnings(na.omit(cbind(DGS10,Comp.x)))
names(Comp10) <- c("UST10", "EU10", "UK10", "DB10")


Spreads <- lapply(Comp10, function(x) {x-Comp10$UST10})
Spreads <- as.data.frame(Spreads)
Sprdzoo <- as.zoo(Spreads)
Sprdxts <- na.omit(as.xts(Sprdzoo[,-1])) # Remove first Column of the Data Frame

EU.adf <- adf.test(Sprdxts$EU10['2004:/'], 'stationary', k=1)
UK.adf <- adf.test(Sprdxts$UK10['2004:/'], 'stationary', k=1)
DB.adf <- adf.test(Sprdxts$DB10['2004:/'], 'stationary', k=1)

Sprdxts[which.max(Sprdxts$UK10),]
Sprdxts[which.max(Sprdxts$EU10),]
Sprdxts[which.max(Sprdxts$DB10),]

Sprdxts[which.min(Sprdxts$EU10),]
Sprdxts[which.min(Sprdxts$UK10),]
Sprdxts[which.min(Sprdxts$DB10),]

par(mfrow=c(2,2), cex.main = .8, cex.sub = .5) #(row,col)
plot(Comp10$UST10, main = "US TSY 10")

plot(Comp10$EU10, main = "EUROBD 10")

plot(Comp10$UK10, main = "UK BD 10")

plot(Comp10$DB10, main = "BUND 10")

summary(Sprdxts$EU10)
cat('\n', "Current EUB-US Tsy Spread: ", last(Sprdxts$EU10), '\n', '\n', "ADF p-value: ", EU.adf$p.value, '\n')

summary(Sprdxts$UK10)
cat('\n', "Current UK-US Tsy Spread: ", last(Sprdxts$UK10), '\n', '\n', "ADF p-value: ", UK.adf$p.value, '\n')

summary(Sprdxts$DB10)
cat('\n', "Current Bund-US Tsy Spread: ", last(Sprdxts$DB10), '\n', '\n', "ADF p-value: ", DB.adf$p.value, '\n')

