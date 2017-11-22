library(quantmod)
library(PerformanceAnalytics)
library(fPortfolio)
Sys.setenv(TZ = 'UTC')

#### Close data from TC2000 Convert TXT to XTS ####
MVtxt <- read.table('MinVar', header = FALSE, sep = ",")

MVzoo <- read.zoo(MVtxt, sep = ",", format = "%m/%d/%Y", split = 3)
MVxts <- as.xts(MVzoo)

Fidelity <- na.omit(cbind(MVxts[,1:11]))
Schwab <- na.omit(cbind(MVxts[,13:22]))
SPDR <- na.omit(cbind(MVxts[,12], MVxts[,24:33]))
GSPC <- na.omit(MVxts[,23])

GSPC.ret <- monthlyReturn(GSPC, type = "log")
## Return Calculations for Minimum Variance Optimization ##
GIC.ret <- na.omit(Return.calculate(SPDR, method = "discrete"))
FID.ret <- na.omit(Return.calculate(Fidelity, method = "discrete"))
SCH.ret <- na.omit(Return.calculate(Schwab, method = "discrete"))

dateroll <- rollingWindows(GIC.ret, period = '12m', by = '1m')

globminSpec <- portfolioSpec()
#setTargetRisk(globminSpec) <- .01

GICEQ.roll <- rollingMinvariancePortfolio(as.timeSeries(GIC.ret), spec = globminSpec,
                                          constraints = c('LongOnly'),from = dateroll$from,
                                          to = dateroll$to)

FIDEQ.roll <- rollingMinvariancePortfolio(as.timeSeries(FID.ret), spec = globminSpec,
                                          constraints = c('LongOnly'),from = dateroll$from,
                                          to = dateroll$to)

SCHEQ.roll <- rollingMinvariancePortfolio(as.timeSeries(SCH.ret), spec = globminSpec,
                                          constraints = c('LongOnly'),from = dateroll$from,
                                          to = dateroll$to)

print(last(GICEQ.roll))
print(last(FIDEQ.roll))
print(last(SCHEQ.roll))


