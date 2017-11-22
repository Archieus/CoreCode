library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'EST5EDT')

### MUTUAL FUND DATA FROM TC2000.com ###
#### reads/loads text into R and converts to XTS ####

#### FIXED INCOME MUTUAL FUNDS ####
IIBAXtxt <- read.table('IIBAX.txt', header = TRUE, sep = ',')
IIBAXzoo <- read.zoo(IIBAXtxt, sep = ",", format = "%m/%d/%Y")
IIBAXxts <- as.xts(IIBAXzoo)

NEFRXtxt <- read.table('NEFRX.txt', header = TRUE, sep = ',')
NEFRXzoo <- read.zoo(NEFRXtxt, sep = ",", format = "%m/%d/%Y")
NEFRXxts <- as.xts(NEFRXzoo)

TGMNXtxt <- read.table('TGMNX.txt', header = TRUE, sep = ',')
TGMNXzoo <- read.zoo(TGMNXtxt, sep = ",", format = "%m/%d/%Y")
TGMNXxts <- as.xts(TGMNXzoo)

#### EQUITY MUTUAL FUNDS ####
DDVAXtxt <- read.table('DDVAX.txt', header = TRUE, sep = ',')
DDVAXzoo <- read.zoo(DDVAXtxt, sep = ",", format = "%m/%d/%Y")
DDVAXxts <- as.xts(DDVAXzoo)

PEMGXtxt <- read.table('PEMGX.txt', header = TRUE, sep = ',')
PEMGXzoo <- read.zoo(PEMGXtxt, sep = ",", format = "%m/%d/%Y")
PEMGXxts <- as.xts(PEMGXzoo)

NOSGXtxt <- read.table('NOSGX.txt', header = TRUE, sep = ',')
NOSGXzoo <- read.zoo(NOSGXtxt, sep = ",", format = "%m/%d/%Y")
NOSGXxts <- as.xts(NOSGXzoo)

BCSIXtxt <- read.table('BCSIX.txt', header = TRUE, sep = ',')
BCSIXzoo <- read.zoo(BCSIXtxt, sep = ",", format = "%m/%d/%Y")
BCSIXxts <- as.xts(BCSIXzoo)

OAKBXtxt <- read.table('OAKBX.txt', header = TRUE, sep = ',')
OAKBXzoo <- read.zoo(OAKBXtxt, sep = ",", format = "%m/%d/%Y")
OAKBXxts <- as.xts(OAKBXzoo)

OIDAXtxt <- read.table('OIDAX.txt', header = TRUE, sep = ',')
OIDAXzoo <- read.zoo(OIDAXtxt, sep = ",", format = "%m/%d/%Y")
OIDAXxts <- as.xts(OIDAXzoo)

MALGXtxt <- read.table('MALGX.txt', header = TRUE, sep = ',')
MALGXzoo <- read.zoo(MALGXtxt, sep = ",", format = "%m/%d/%Y")
MALGXxts <- as.xts(MALGXzoo)

#### CREATE DATA TABLE TO BE USED IN OPTIMIZATION ####
SchFI.x <- cbind(IIBAXxts[,4], NEFRXxts[,4], TGMNXxts[,4])
colnames(SchFI.x) <- c('IIBAX', 'NEFRX', 'TGMNX')

SchEQ.x <- cbind(DDVAXxts[,4], PEMGXxts[,4], NOSGXxts[,4], BCSIXxts[,4], OAKBXxts[,4],
                 OIDAXxts[,4], MALGXxts[,4])
colnames(SchEQ.x) <- c('DDVAX', 'PEMGX', 'NOSGX', 'BCSIX', 'OAKBX', 'OIDAX', 'MALGX')

#Calculate daily log component returns
SchFI.Ret <- na.omit(Return.calculate(SchFI.x, method = ("log")))
SchEQ.Ret <- na.omit(Return.calculate(SchEQ.x, method = ("log")))

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%

SchFIRoll <- rollingWindows(SchFI.Ret, period = '12m', by = '3m')

#Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio Using Rolling Windows#
SchFI.roll <- rollingTangencyPortfolio(as.timeSeries(SchFI.Ret), spec = Default,
                                       constraints = c('minW[1:3] = .1', 'maxW[1:3] = .5'),
                                       from = SchFIRoll$from, to = SchFIRoll$to)

SchEQ.roll <- rollingTangencyPortfolio(as.timeSeries(SchEQ.Ret), spec = Default,
                                       constraints = c('minW[1:7] = .1', 'maxW[1:7] = .35'),
                                       from = SchFIRoll$from, to = SchFIRoll$to)

EQexINTL.roll <- rollingTangencyPortfolio(as.timeSeries(SchEQ.Ret), spec = Default,
                                          constraints = c('minW[1:5] = .1', 'maxW[1:5] = .35',
                                                          'maxW[6:7] = 0'),
                                          from = SchFIRoll$from, to = SchFIRoll$to)

last(SchFI.roll)
last(SchEQ.roll)
last(EQexINTL.roll)



