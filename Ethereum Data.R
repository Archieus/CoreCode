library(jsonlite)
library(quantmod)
library(depmixS4)

#### Extract Data from Etherchain.org ####
#### Parse escaped Unicode and attempt to create a dataframe from JSON data ####
ETH.raw <- fromJSON('https://etherchain.org/api/statistics/price')

ETH.df <- as.data.frame(ETH.raw) 

#### Convert to XTS format ####
ETH.c <- as.data.frame(coredata(ETH.df[,3]))
rownames(ETH.c) <- ETH.df[,2]
ETH <- as.xts(ETH.c)

#### Convert Intraday Data to Daily Data ####
ETH.dly <- to.daily(ETH)

chartSeries(ETH['2017-12:/'])

#### Write data to csv for use in charting program ####
ETHDaily.df <- as.data.frame(ETH.dly)
write.csv(ETHDaily.df, file = "ETH.csv", row.names = TRUE)

#### Generate Data for Bubble Indicator ####
