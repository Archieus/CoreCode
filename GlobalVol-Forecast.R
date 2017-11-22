library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
library(fPortfolio)
library(fMultivar)
library(tseries)
library(CausalImpact)
library(rugarch)
Sys.setenv(TZ = "UTC")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

# #### reads/loads CSV into R ####
# fut <- read.csv("futures.csv", header = FALSE)
# FT <- t(fut)

# indexes <- read.csv("indexes.csv", header = FALSE)
# IND <- t(indexes)
# 
# ####Equity Indexes####
# suppressWarnings(getSymbols(IND, src = 'yahoo', from = '2013-12-31'))

CAC40 <- Quandl("CHRIS/LIFFE_FCE1", type = 'xts', start_date = "2013-12-31")
EUR50 <- Quandl("CHRIS/EUREX_FESX1", type = 'xts', start_date = "2013-12-31")
DAX <- Quandl("CHRIS/EUREX_FDAX1", type = 'xts', start_date = "2013-12-31")
FTSE <- Quandl("CHRIS/LIFFE_Z1", type = 'xts', start_date = "2013-12-31")
SP500 <- Quandl("CHRIS/CME_SP1", type = 'xts', start_date = "2013-12-31")
CHNFT <- Quandl("CHRIS/SGX_CN1", type = 'xts', start_date = "2013-12-31")
NIK225 <- Quandl("NIKKEI/ALL_STOCK", type = 'xts', start_date = "2013-12-31")

####Currency Futures####
AUD <- Quandl("CHRIS/CME_AD1", type = 'xts', start_date = "2013-12-31")
CAD <- Quandl("CHRIS/CME_CD1", type = 'xts', start_date = "2013-12-31")
EUR <- Quandl("CHRIS/CME_EC1", type = 'xts', start_date = "2013-12-31")
JPY <- Quandl("CHRIS/CME_JY1", type = 'xts', start_date = "2013-12-31")
USD <- Quandl("CHRIS/ICE_DX1", type = 'xts', start_date = "2013-12-31")
GBP <- Quandl("CHRIS/CME_BP1", type = 'xts', start_date = "2013-12-31")

####Energy####
WTI <- Quandl("CHRIS/ICE_T1", type = 'xts', start_date = "2013-12-31")
#CRUDE <- Quandl("CHRIS/CME_BZ1", type = 'xts', start_date = "2013-12-31")
BRENT <- Quandl("CHRIS/ICE_B1", type = 'xts', start_date = "2013-12-31")
NATGAS <- Quandl("CHRIS/CME_NG1", type = 'xts', start_date = "2013-12-31")

####Metals####
Au <- Quandl("CHRIS/CME_GC1", type = 'xts', start_date = "2013-12-31")
Cu <- Quandl("CHRIS/CME_HG1", type = 'xts', start_date = "2013-12-31")

####Fixed Income####
BUND <- Quandl("CHRIS/EUREX_FGBL1", type = 'xts', start_date = "2013-12-31")
USTSY <- Quandl("CHRIS/CME_US1", type = 'xts', start_date = "2013-12-31")
JGB <- Quandl("CHRIS/SGX_JB1", type = 'xts', start_date = "2013-12-31")
CADBND <- Quandl("CHRIS/MX_CGB1", type = 'xts', start_date = "2013-12-31")
GILT <- Quandl("CHRIS/LIFFE_H1", type = 'xts', start_date = "2013-12-31")

####Agricultural Commodities####
Corn <- Quandl("CHRIS/CME_C1", type = 'xts', start_date = "2013-12-31")
Wht <- Quandl("CHRIS/ICE_IW1", type = 'xts', start_date = "2013-12-31")
Cott <- Quandl("CHRIS/ICE_CT1", type = 'xts', start_date = "2013-12-31")
Oats <- Quandl("CHRIS/CME_O1", type = 'xts', start_date = "2013-12-31")
SoyBn <- Quandl("CHRIS/CME_S1", type = 'xts', start_date = "2013-12-31")
LvCat <- Quandl("CHRIS/CME_LC1", type = 'xts', start_date = "2013-12-31")
FdCat <- Quandl("CHRIS/CME_FC1", type = 'xts', start_date = "2013-12-31")
Hogs <- Quandl("CHRIS/CME_LN1", type = 'xts', start_date = "2013-12-31")
Lmbr <- Quandl("CHRIS/CME_LB1", type = 'xts', start_date = "2013-12-31")
Cofee <- Quandl("CHRIS/ICE_KC1", type = 'xts', start_date = "2013-12-31")
Sugar <- Quandl("CHRIS/LIFFE_W1", type = 'xts', start_date = "2013-12-31")

Settle <- na.locf(cbind(AUD[,6],CAD[,6],EUR[,6],JPY[,6],USD[,4],GBP[,6],WTI[,4],BRENT[,4],NATGAS[,6],
                Au[,6],Cu[,6],BUND[,4],USTSY[,6],JGB[,5],CADBND[,14],GILT[,4],Corn[,6], Wht[,4],
                Cott[,4],Oats[,6],SoyBn[,6],LvCat[,6],FdCat[,6],Hogs[,6],Lmbr[,6],Cofee[,4],Sugar[,4],
                SP500[,6],DAX[,4],NIK225[,1], EUR50[,4], CAC40[,4], FTSE[,4], CHNFT[,5]))

names(Settle) <- c('AUD','CAD','EUR','YEN','USD','GBP','WTI','CRUDE','NATGAS','Au','Cu','BUND',
                   'USTSY','JBd','CBd','GILT','Corn','Wht','Cott','Oats','SoyBn','LvCat',
                   'FdCat','Hogs','Lmbr','Cofee','Sugar','SP5','DAX','N225', 'EUR50', 'CAC40',
                   'FTSE', 'CNFT')

Forx <- na.locf(cbind(AUD[,6],CAD[,6],EUR[,6],JPY[,6],USD[,4],GBP[,6]))
names(Forx) <- c('AUD','CAD','EUR','YEN','USD','GBP')

Forx.df <- as.data.frame(rowMeans(Forx))
Forx.x <- as.xts(coredata(Forx.df), index(Forx))

Engy <- na.locf(cbind(WTI[,4],BRENT[,4],NATGAS[,6]))
names(Engy) <- c('WTI','CRUDE','NATGAS')

Engy.df <- as.data.frame(rowMeans(Engy))
Engy.x <- as.xts(coredata(Engy.df), index(Engy))

Metl <- na.locf(cbind(Au[,6],Cu[,6]))
names(Metl) <- c('Au','Cu')

Metl.df <- as.data.frame(rowMeans(Metl))
Metl.x <- as.xts(coredata(Metl.df), index(Metl))

# Fixd <- na.locf(cbind(BUND[,4],USTSY[,6],JPYBND[,5],CADBND[,14],GILT[,4]))
# names(Fixd) <- c('BUND','USTSY','JBd','CBd','GILT')
Fixd <- na.locf(cbind(BUND[,4],USTSY[,6],CADBND[,14],GILT[,4]))
names(Fixd) <- c('BUND','USTSY','CBd','GILT')

Fixd.df <- as.data.frame(rowMeans(Fixd))
Fixd.x <- as.xts(coredata(Fixd.df), index(Fixd))

Agri <- na.locf(cbind(Corn[,6], Wht[,4],Cott[,4],Oats[,6],SoyBn[,6],LvCat[,6],FdCat[,6],
                      Hogs[,6],Lmbr[,6],Cofee[,4],Sugar[,4]))
names(Agri) <- c('Corn','Wht','Cott','Oats','SoyBn','LvCat','FdCat','Hogs','Lmbr','Cofee','Sugar')

Agri.df <- as.data.frame(rowMeans(Agri))
Agri.x <- as.xts(coredata(Agri.df), index(Agri))

Eqty <- na.locf(cbind(SP500[,6],DAX[,4],NIK225[,1],EUR50[,4], CAC40[,4], FTSE[,4], CHNFT[,5]))
names(Eqty) <- c('SP500','DAX','N225', 'EUR50', 'CAC40', 'FTSE100', 'CNFTSE')

Eqty.df <- as.data.frame(rowMeans(Eqty))
Eqty.x <- as.xts(coredata(Eqty.df), index(Eqty))

GVI.comp <- na.omit(cbind(Forx.x, Engy.x, Metl.x, Fixd.x, Agri.x, Eqty.x))
GVI.df <- as.data.frame(rowMeans(GVI.comp))
GVI.x <- as.xts(coredata(GVI.df), index(GVI.comp))
GVI.ret <- na.omit(Return.calculate(GVI.x, method = "log"))

####CALCULATE RETURNS####

Returns <- Return.calculate(Settle, method = c("discrete"))
wklyret <- Returns[endpoints(Returns, on = 'weeks', k = 1)]
Comp.ret <- Return.portfolio(na.omit(Returns))
Comp.wk <- Comp.ret[endpoints(Comp.ret, on = 'weeks', k = 1)]

FXret <- Return.calculate(Forx, method = c('discrete'))
FXPort <- Return.portfolio(na.omit(FXret))
FXwret <- FXPort[endpoints(FXPort, on = 'weeks', k = 1)]

ENret <- Return.calculate(Engy, method = c('discrete'))
ENPort <- Return.portfolio(na.omit(ENret))
ENwret <- ENPort[endpoints(ENPort, on = 'weeks', k = 1)]

MTret <- Return.calculate(Metl, method = c('discrete'))
MTPort <- Return.portfolio(na.omit(MTret))
MTwret <- MTPort[endpoints(MTPort, on = 'weeks', k = 1)]

FIret <- Return.calculate(Fixd, method = c('discrete'))
FIPort <- Return.portfolio(na.omit(FIret))
FIwret <- FIPort[endpoints(FIPort, on = 'weeks', k = 1)]

AGret <- Return.calculate(Agri, method = c('discrete'))
AGPort <- Return.portfolio(na.omit(AGret))
AGwret <- AGPort[endpoints(AGPort, on = 'weeks', k = 1)]

EQret <- Return.calculate(Eqty, method = c('discrete'))
EQPort <- Return.portfolio(na.omit(EQret))
EQwret <- EQPort[endpoints(EQPort, on = 'weeks', k = 1)]

Cat.dret <- na.locf(cbind(FXPort, ENPort, MTPort, FIPort, AGPort, EQPort))
names(Cat.dret) <- c('Forex', 'Energy', 'Metals', 'Fixed', 'Agri', 'Equities')

Cat.wret <- na.locf(cbind(FXwret, ENwret, MTwret, FIwret, AGwret, EQwret))
names(Cat.wret) <- c('Forex', 'Energy', 'Metals', 'Fixed', 'Agri', 'Equities')

#SD.1d <- na.omit(runSD(GVI.ret))*100 ##Daily Std Deviation
#SD.5d <- na.omit(runSD(GVI.ret, 5))*100 ##Daily 5-period Std Deviation
# SD.5dc <- na.omit(runSD(Comp.ret, 5))*100 ##Daily 5-period Std Deviation

####GARCH(1,1) - Volatility Forecast###
spec <- ugarchspec()
GVI.oos <- 5 # Out of Sample selection.  Omits this no. of obs from calculation
GVI.fit <- ugarchfit(spec, data = GVI.ret, out.sample = GVI.oos)
GVI.fore <- ugarchforecast(GVI.fit, n.ahead = 1, n.roll = 5, out.sample = GVI.oos)

GVI.hd <- round(halflife(GVI.fit),0)
GVI.hv <- volatility(GVI.ret, calc = 'close', n = GVI.hd, N = 260, mean0 = TRUE)
GVI.sd <- na.omit(runSD(GVI.x, n = GVI.hd))

# #chart.Correlation(tail(wklyret,52))
# assetsCorImagePlot(tail(wklyret,52))
# 
# #chart.Correlation(tail(Returns,30))
# assetsCorImagePlot(tail(Returns,30))

#chart.Correlation(tail(Returns,10))
#assetsCorImagePlot(tail(Returns,10))

#chart.Correlation(tail(Cat.wret, 52))

chart.Correlation(tail(Cat.dret, 360))
chart.Correlation(tail(Cat.dret, 180))
chart.Correlation(tail(Cat.dret, 90))
chart.Correlation(tail(Cat.dret, 60))
chart.Correlation(tail(Cat.dret, 30))
chart.Correlation(tail(Cat.dret, 10))
# 
# chart.RollingCorrelation(Cat.dret$Forex['2016:/'], Cat.dret[,2:6]['2016:/'], width = 6, legend.loc = 'left')
# chart.RollingCorrelation(Cat.dret$Energy['2016:/'], Cat.dret[,c(1, 3:6)]['2016:/'], width = 6, legend.loc = 'left')
# chart.RollingCorrelation(Cat.dret$Metals['2016:/'], Cat.dret[,c(1:2,4:6)]['2016:/'], width = 6, legend.loc = 'left')
# chart.RollingCorrelation(Cat.dret$Fixed['2016:/'], Cat.dret[,c(1:3,5:6)]['2016:/'], width = 6, legend.loc = 'left')
# chart.RollingCorrelation(Cat.dret$Agri['2016:/'], Cat.dret[,c(1:4,6)]['2016:/'], width = 6, legend.loc = 'left')
# chart.RollingCorrelation(Cat.dret$Equities['2016:/'], Cat.dret[,1:5]['2016:/'], width = 6, legend.loc = 'left')

plot(GVI.fore, which = 3)

ADF.Vol <- suppressWarnings(adf.test(GVI.sd, alternative = 'stationary', k = 1))
#summary(SDImpact, 'report')

coef(GVI.fit)
fitted(GVI.fore)
sigma(GVI.fore) #Sigma =  Conditional Standard deviation

summary(GVI.sd)
cat('\n', "Increasing Volatility (Mean Reversion)?", last(GVI.sd) < mean(GVI.sd), '\n', '\n', 
    "Current Volatility",last(GVI.sd),'\n', '\n', "5%:", quantile(GVI.sd, .05),
    '\n', "95%:", quantile(GVI.sd, .95), '\n', '\n', "ADF p-value is: ", ADF.Vol$p.value, '\n', '\n')

