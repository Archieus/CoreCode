library(Quandl)
library(quantmod)
Sys.setenv(TZ = "EST5EDT")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

COTDJI <- Quandl("CFTC/11C_FO_L_ALL", type = "xts", start_date = "2010-06-15")
COTSP5 <- Quandl("CFTC/SPC_FO_L_ALL", type = "xts", start_date = "2010-06-15")

getSymbols(c('DJIA', 'SP500'), src = 'FRED', from = "2010-06-01")

DJI.WK <- to.weekly(DJIA)[,4]
SP5.WK <- to.weekly(SP500)[,4]

### Find Net Position of Non-Commercial (Speculators) and Non-Reportable (Small Traders) ###
DJI.NC <- COTDJI[,2] - COTDJI[,3]
DJI.ST <- COTDJI[,9] - COTDJI[,10]

SP5.NC <- COTSP5[,2] - COTSP5[,3]
SP5.ST <- COTSP5[,9] - COTSP5[,10]

### Commercial Traders: Hedging Corporate Positions (Smart Money)###
DJI.CH <- COTDJI[,5] - COTDJI[,6]
SP5.CH <- COTSP5[,5] - COTSP5[,6]

layout(1:4)
plot(DJI.WK['2010-06-15:/'], main = "DJI Average (Weekly)")
plot(DJI.NC, main = "COT-DJIA Futures & Options (Net) - Non-Commercial")
plot(DJI.ST, main = "COT-DJIA Futures & Options (Net)- Small Traders")
plot(DJI.CH, main = "COT-DJIA Futures & Options (Net)- Commercial (Smart Money)")

layout(1:4)
plot(SP5.WK['2010-06-15:/'], main = "SP500 Average (Weekly)")
plot(SP5.NC, main = "COT-SP500 Futures & Options (Net) - Non-Commercial")
plot(SP5.ST, main = "COT-SP500 Futures & Options (Net)- Small Traders")
plot(SP5.CH, main = "COT-SP500 Futures & Options (Net)- Commercial (Smart Money)")

