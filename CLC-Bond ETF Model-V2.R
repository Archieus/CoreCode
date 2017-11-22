library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
TETFCtxt <- read.table("~/1. R Programming/Weekly Reports/tdafixed", header = FALSE, sep = ',')
TETFCzoo <- read.zoo(TETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TETFCxts <- as.xts(TETFCzoo)

#which(colnames(TETFCxts)=="GOVT") #Find column number associated with a column name ###

ETFData <- TETFCxts['2014-11-07:/']

#Calculate daily log component returns
Fixed.Ret <- na.omit(Return.calculate(ETFData, method = ("log")))

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%

FIXEDroll <- rollingWindows(Fixed.Ret, period = "12m", by = "1m")

#### Identify the Components to make up the final Fixed Income models ####

AltFI <- as.ts("maxsumW[c(1,2,4,6,13)] = .3") # Maximum allocation into CWB, HYMB, FPE (combined as a group)
StdFI <- as.ts("maxW[c(3,5,7:12)] = .3") #Maximum allocation into each of the individual ETFs

groupConstraints <- c(AltFI, StdFI)

#Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio Using Rolling Windows#
Fixed.roll <- rollingTangencyPortfolio(as.timeSeries(Fixed.Ret), spec = Default,
                                       constraints = groupConstraints,
                                       from = FIXEDroll$from, to = FIXEDroll$to)

FIWeights <- read.csv("FixedWts.csv")
FIWeights.z <- read.zoo("FixedWts.csv", header = TRUE, sep = ",")
FIWeights.x <- as.xts(FIWeights.z)
 
FIRet.ep <- endpoints(Fixed.Ret, on = 'months', k=1) 
FIRet.mo <- Fixed.Ret[FIRet.ep]
  
FixedRet <- round(Return.portfolio(FIRet.mo, weights = FIWeights.x, wealth.index = FALSE, 
                                   contribution = FALSE, geometric = TRUE), digits = 6)
 
table.CalendarReturns(FixedRet)
SharpeRatio(FixedRet, .01/12, FUN = "StdDev")
sd(FixedRet)

last(Fixed.roll)

