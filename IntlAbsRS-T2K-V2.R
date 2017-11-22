library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = "UTC")

##RELATIVE MOM = Compare the share price movement of one company to the price movement of another Co.
##ABSOLUTE MOM = Compare the movement of a company's share price with the return of a short govt bond.

#### reads/loads CSV into R ####
Descr <- read.csv('IntlNames2.csv', header = FALSE)

Datalist <- read.table('IntlETF', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)
DLxts <- na.locf(DLxts, na.rm = FALSE, fromLast = TRUE)

StartDate <- first(index(DLxts))

# TNX <- Quandl("CHRIS/CME_TY1", type = "xts", start_date = "2007-12-31",
#               end_date = Sys.Date()) # Ten Year US Treasury Constant Maturity

TU1 <- Quandl("CHRIS/CME_TU1", type = "xts", start_date = StartDate) # Two Year US Treasury Constant Maturity

SP500.ret <- na.omit(monthlyReturn(DLxts$SP-500, method = c("discrete")))
Monthly.ret <- do.call(cbind, lapply(DLxts, monthlyReturn))
names(Monthly.ret) <- colnames(DLxts)

####CREATE an XTS object with monthly ADJ CLOSE####
TRAdj <- na.omit(round(DLxts[endpoints(DLxts, 'months')],1))
names(TRAdj) <- colnames(DLxts)
SP500.mo <- to.period(DLxts$SP-500, period = "months")
TU1.mo <- to.period(TU1[,6], period = "months")

####Calculate 12-mo ROC####
TRROC <- ROC(TRAdj, 12)
SPROC <- ROC(SP500.mo[,4], 12)
TU1ROC <- ROC(TU1.mo[,4], 12)

# Calculate the INTL RS for the stocks

DList <- function(Sym) {
  ((((Sym - lag(Sym,k=63)) / lag(Sym,k=63)) * .4)
   + (((Sym - lag(Sym, k=126)) / lag(Sym, k=126)) * .2)
   + (((Sym - lag(Sym,k=189)) / lag(Sym, k=189)) * .2)
   + (((Sym - lag(Sym, k=252)) / lag(Sym,k=252)) * .2)) * 100
}

DLxts.RS <- na.omit(do.call(cbind, lapply(DLxts, DList)))

RP <- function(Sym) {
  (Sym/DLxts[,40])*100 #RS versus S&P
}

Relperf <- na.omit(do.call(cbind, lapply(DLxts, RP)))

### Calculate the Mansfield Relative Performance ###
MRP <- function(RS) {
  ((((RS/SMA(RS, n=251))-1)+((RS/SMA(RS, n=188))-1)+
      ((RS/SMA(RS, n=125))-1)+(2*((RS/SMA(RS, n=60))-1))+
      (2*((RS/SMA(RS, n=40))-1))+(2*((RS/SMA(RS, n=20))-1))+
      (2*((RS/SMA(RS, n=5))-1)))*100)/11
}

Man.rp <- round(na.omit(do.call(cbind, lapply(Relperf, MRP))),4)

# Calculate Excess Return or Absolute Momentum versus Short-term Treasury
AbsMom <- matrix(0, nrow = nrow(TRAdj), ncol= ncol(TRAdj))
for (i in seq(1, ncol(TRAdj), 1)) {
  AbsMom[,i] <- round(TRROC[,i] - TU1ROC, digits = 4)
}

AbsMom.df <- data.frame(coredata(AbsMom)) #Convert to dataframe & assigning coredata
rownames(AbsMom.df) <- index(TRAdj) # Assigning the dates from S&P 500 as index for df
AbsMom.z <- as.zoo(AbsMom.df) #Convert to zoo format
AbsMom.x <- na.omit(as.xts(AbsMom.z)) #Convert to xts format
names(AbsMom.x) <- colnames(DLxts) # Assign column labels from the variable 'Assets'

#### Calculate 30 Week EMA for ETFs ####
# Convert to weekly Closes
Weekly.cl <- na.omit(round(DLxts[endpoints(DLxts, 'weeks')],2))

### Calculate 30 Week EMA for ETFs ###
MSP.EMA <- lapply(Weekly.cl, function(x) round(last(EMA(x, n = 30)),2))
EMA.df <- as.data.frame(MSP.EMA)
names(EMA.df) <- colnames(DLxts)
EMA.z <- as.zoo(EMA.df)
EMA.x <- as.xts(EMA.z)
Curr.Px <- last(Weekly.cl)

#### Create momentum Plus Report ####
RSABS <- cbind(t(last(Man.rp,1)), t(last(AbsMom.x,1)), t(last(Curr.Px,1)), t(last(EMA.x,1)))
RSABS.df <- as.data.frame(RSABS)
RSABSRpt.df <- cbind(Descr[,2:3],RSABS.df)
names(RSABSRpt.df) <- c("Description", "DM-EM", "RS", "ABS", "close", "30Wk" )


write.csv(RSABSRpt.df, file = "IntlRSABS-Report.csv")

