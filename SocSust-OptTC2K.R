library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)

### MUTUAL FUND DATA FROM TC2000.com ###
#### reads/loads text into R and converts to XTS ####
DOMIXtxt <- read.table('DOMIX.txt', header = TRUE, sep = ',')
DOMIXzoo <- read.zoo(DOMIXtxt, sep = ",", format = "%m/%d/%Y")
DOMIX <- as.xts(DOMIXzoo)

CVMAXtxt <- read.table('CVMAX.txt', header = TRUE, sep = ',')
CVMAXzoo <- read.zoo(CVMAXtxt, sep = ",", format = "%m/%d/%Y")
CVMAX <- as.xts(CVMAXzoo)

MYPVXtxt <- read.table('MYPVX.txt', header = TRUE, sep = ',')
MYPVXzoo <- read.zoo(MYPVXtxt, sep = ",", format = "%m/%d/%Y")
MYPVX <- as.xts(MYPVXzoo)

NRAAXtxt <- read.table('NRAAX.txt', header = TRUE, sep = ',')
NRAAXzoo <- read.zoo(NRAAXtxt, sep = ",", format = "%m/%d/%Y")
NRAAX <- as.xts(NRAAXzoo)

CAAPXtxt <- read.table('CAAPX.txt', header = TRUE, sep = ',')
CAAPXzoo <- read.zoo(CAAPXtxt, sep = ",", format = "%m/%d/%Y")
CAAPX <- as.xts(CAAPXzoo)

PAXWXtxt <- read.table('PAXWX.txt', header = TRUE, sep = ',')
PAXWXzoo <- read.zoo(PAXWXtxt, sep = ",", format = "%m/%d/%Y")
PAXWX <- as.xts(PAXWXzoo)

PXSAXtxt <- read.table('PXSAX.txt', header = TRUE, sep = ',')
PXSAXzoo <- read.zoo(PXSAXtxt, sep = ",", format = "%m/%d/%Y")
PXSAX <- as.xts(PXSAXzoo)

PDRAXtxt <- read.table('PDRAX.txt', header = TRUE, sep = ',')
PDRAXzoo <- read.zoo(PDRAXtxt, sep = ",", format = "%m/%d/%Y")
PDRAX <- as.xts(PDRAXzoo)

PRFAXtxt <- read.table('PRFAX.txt', header = TRUE, sep = ',')
PRFAXzoo <- read.zoo(PRFAXtxt, sep = ",", format = "%m/%d/%Y")
PRFAX <- as.xts(PRFAXzoo)

PAXHXtxt <- read.table('PAXHX.txt', header = TRUE, sep = ',')
PAXHXzoo <- read.zoo(PAXHXtxt, sep = ",", format = "%m/%d/%Y")
PAXHX <- as.xts(PAXHXzoo)

Social.x <- cbind(DOMIX$Close,CVMAX$Close, MYPVX$Close, NRAAX$Close, CAAPX$Close, PAXWX$Close, PXSAX$Close,
                  PDRAX$Close, PRFAX$Close, PAXHX$Close)
names(Social.x) <- c("DOMIX", "CVMAX", "MYPVX", "NRAAX", "CAAPX", "PAXWX", "PXSAX", "PDRAX", "PRFAX",
                     "PAXHX")

#Calculate daily log component returns
Asset.Ret <- na.omit(Return.calculate(Social.x, method = c("log")))

#Extract Returns by asset class
EqINT.Ret <-cbind(Asset.Ret[,1:7])
Fixed.Ret <- cbind(Asset.Ret[,8:10])

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%
FIDate <- rollingWindows(Fixed.Ret, period = '12m', by = '3m')
EQDate <- rollingWindows(EqINT.Ret, period = '12m', by = '3m')

#Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio
# SocFI.tp <- tangencyPortfolio(as.timeSeries(Fixed.Ret['2015-01-01:/2016-01-31']), spec = Default,
#                               constraints = c('minW[1:3] = .05', 'maxW[1:3] = .5'))

SocFI.roll <- suppressWarnings(rollingTangencyPortfolio(as.timeSeries(Fixed.Ret), spec = Default,
                                       constraints = c('minW[1:3] = .05','maxW[1:3] = .5'),
                                       from = FIDate$from, to = FIDate$to))

# SocEQ.tp <- tangencyPortfolio(as.timeSeries(EqINT.Ret), spec = Default,
#                                constraints = c('minW[1:7] = .05', 'maxW[1:7] = .30', 'maxW[2] = .05', 
#                                                'maxW[6] = .2'))

SocEQ.roll <- suppressWarnings(rollingTangencyPortfolio(as.timeSeries(EqINT.Ret), spec = Default,
                                       constraints = c('minW[1:7] = .05', 'maxW[1:7] = .30'),
                                       from = EQDate$from, to = EQDate$to))

last(SocEQ.roll)
last(SocFI.roll)

