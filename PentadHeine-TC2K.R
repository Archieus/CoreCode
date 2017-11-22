library(quantmod)
library(PerformanceAnalytics)
Sys.setenv(TZ = "UTC")

#### Close data from TC2000 Convert TXT to XTS ####
mktdata <- read.table('timing', header = FALSE, sep = ",")

MDzoo <- read.zoo(mktdata, sep = ",", format = "%m/%d/%Y", split = 3)
MDxts <- na.omit(as.xts(MDzoo))

####Convert to Weekly Data for Use in the Models####
TLT.w <- to.period(MDxts$TLT, period = "weeks")
TNX.w <- to.period(MDxts$`TNX--X`, period = "weeks")
IRX.w <- to.period(MDxts$`IRX--X`, period = "weeks")
DJP.w <- to.period(MDxts$CRY0, period = "weeks")
GSPC.w <- to.period(MDxts$`SP-500`, period = "weeks")
DJIA.w <- to.period(MDxts$`DJ-30`, "weeks")
DJTA.w <- to.period(MDxts$`DJ-20`, "weeks")
DJUA.w <- to.period(MDxts$`DJ-15`, "weeks")
AD.w <- to.period(MDxts$T2100, "weeks")

AD.Buy <- WMA(AD.w[,4],14)*1.005
AD.Sell <- WMA(AD.w[,4],14)*.98

#### Heine Model Components ####
HBM.TLT <- ifelse(TLT.w[,4] > SMA(TLT.w[,4], 24),1,0)
HBM.TNX <- ifelse(TNX.w[,4] < SMA(TNX.w[,4], 6),1,0)
HBM.IRX <- ifelse(IRX.w[,4] < SMA(IRX.w[,4], 6),1,0)
HBM.DJU <- ifelse(DJUA.w[,4] > SMA(DJUA.w[,4], 10),1,0)
HBM.DJP <- ifelse(DJP.w[,4] < SMA(DJP.w[,4], 20),1,0)

HBM.signal <- ifelse(as.numeric(tail(HBM.TLT,250))+as.numeric(tail(HBM.TNX,250))+as.numeric(tail(HBM.IRX,250))
                     +as.numeric(tail(HBM.DJU,250))+as.numeric(tail(HBM.DJP,250)) >= 3, "Buy", "Sell")

####Heine Bond Model Backtest ####
HBM.Sig <- ifelse(as.numeric(tail(HBM.TLT,250))+as.numeric(tail(HBM.TNX,250))+as.numeric(tail(HBM.IRX,250))
                  +as.numeric(tail(HBM.DJU,250))+as.numeric(tail(HBM.DJP,250)) >= 3, 1, 0)

HBM.m <- as.matrix(HBM.Sig)
TLT.ret <- Return.calculate(TLT.w, 'log')
charts.PerformanceSummary(cbind(tail(TLT.ret[,4],200), tail(HBM.m,200)))
sigret <- na.omit(tail(TLT.ret[,4],200)*tail(HBM.m,200))
table.Drawdowns(sigret, top = 10)
table.DownsideRisk(sigret)
table.AnnualizedReturns(sigret)
charts.PerformanceSummary(sigret)

#### Breadth Thrust (NDR) ####
layout(1:2)
plot(GSPC.w[,4], main = "S&P", type = 'l')
plot(AD.w[,4], main = "AD Line", type = 'l')

#### Plot AD Line with MA ####
chartSeries(AD.w, TA = 'addWMA(14, col = "red")')

#### Pentad Stock Model BULLISH Components ####

PSM.SP5 <- ifelse(GSPC.w[,4] > WMA(GSPC.w[,4], 65),1,0)
PSM.AD <- ifelse(AD.w[,4] > AD.Buy,1,0)
PSM.DJT <- ifelse(DJTA.w[,4] > (1.005*SMA(DJTA.w[,4], 25)),1,0)
PSM.DJU <- ifelse(SMA(DJUA.w[,4],27) > Lag(SMA(DJUA.w[,4],27)),1,0)
PSM.TLT <- ifelse(TLT.w[,4] > (1.01*WMA(TLT.w[,4],38)),1,0)

PSM.Bull <- ifelse(as.numeric(tail(PSM.SP5,250)) + as.numeric(tail(PSM.AD,250)) + as.numeric(tail(PSM.DJT,250))
                   + as.numeric(tail(PSM.DJU,250)) + as.numeric(tail(PSM.TLT,250)) == 5, "Enter", "No Signal")

#### Pentad Stock Model BEARISH Components ####
PSM.SPBe <- ifelse(GSPC.w[,4] < (.97*WMA(GSPC.w[,4], 65)),0,1)
PSM.ADBe <- ifelse(AD.w[,4] < AD.Sell,0,1)
PSM.DTBe <- ifelse(DJTA.w[,4] < (.975*SMA(DJTA.w[,4],25)),0,1)
DU.SMA <- na.omit(SMA(DJUA.w[,4],27))
DU.max <- rollmax(DU.SMA,4)
PSM.DUBe <- ifelse(SMA(DJUA.w[,4],27) < DU.max*.97,0,1)
PSM.TLTBe <- ifelse(TLT.w[,4] < (WMA(TLT.w[,4],38)*.98),0,1)

PSM.Bear <- ifelse(as.numeric(tail(PSM.SPBe,250)) + as.numeric(tail(PSM.ADBe,250)) + as.numeric(tail(PSM.DTBe,250))
                   + as.numeric(tail(PSM.DUBe,250)) + as.numeric(tail(PSM.TLTBe,250)) <= 3, "Exit", "No Signal")


cat('\n', '\n', "HBM.TLT", last(HBM.TLT), '\n', "HBM.TNX", last(HBM.TNX), '\n', "HBM.IRX", last(HBM.IRX), '\n',
    "HBM.DJU", last(HBM.DJU), '\n', "HBM.DJP", last(HBM.DJP), '\n',  "Heine Bond Model =", last(HBM.signal),
    '\n', '\n', "PSM.SP5", last(PSM.SP5), '\n', "PSM.AD ", last(PSM.AD), '\n', "PSM.DJT", last(PSM.DJT), '\n',
    "PSM.DJU", last(PSM.DJU), '\n', "PSM.TLT", last(PSM.TLT), '\n', "Pentad Entry Signal =", last(PSM.Bull), '\n','\n',
    "PSM.SPBe", last(PSM.SPBe), '\n', "PSM.ADBe", last(PSM.ADBe), '\n', "PSM.DTBe", last(PSM.DTBe), '\n', 
    "PSM.DUBe", last(PSM.DUBe), '\n', "PSM.TLTBe", last(PSM.TLTBe), '\n', "Pentad Exit Signal =", last(PSM.Bear), '\n')


