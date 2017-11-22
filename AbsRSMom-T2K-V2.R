library(quantmod)
library(Quandl)
library(tseries)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = "UTC")

##RELATIVE MOM = Compare the share price movement of one company to the price movement of another Co.
##ABSOLUTE MOM = Compare the movement of a company's share price with the return of a short govt bond.

#### reads/loads CSV into R ####
Descr <- read.csv('ETF-Names.csv', header = FALSE)

Datalist <- read.table('MomPlus', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)

StartDate <- first(index(DLxts))

# TNX <- Quandl("CHRIS/CME_TY1", type = "xts", start_date = "2007-12-31",
#               end_date = Sys.Date()) # Ten Year US Treasury Constant Maturity

TU1 <- Quandl("CHRIS/CME_TU1", type = "xts", start_date = StartDate) # Two Year US Treasury Constant Maturity

SP500.ret <- na.omit(monthlyReturn(DLxts$`SP-500`, method = c("discrete")))
Monthly.ret <- do.call(cbind, lapply(DLxts, monthlyReturn))
names(Monthly.ret) <- colnames(DLxts)

####CREATE an XTS object with monthly ADJ CLOSE####
TRAdj <- round(DLxts[endpoints(DLxts, 'months')],1)
names(TRAdj) <- colnames(DLxts)
SP500.mo <- to.period(DLxts$`SP-500`, period = "months")
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
  (Sym/DLxts$`SP-500`)*100
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

AbsMom <- round(na.omit(do.call(cbind, lapply(TRROC, function(x) {x - TU1ROC[,1]}))),4)

# AbsMom <- matrix(0, nrow = nrow(TRAdj), ncol= ncol(TRAdj))
# for (i in seq(1, ncol(TRAdj), 1)) {
#   AbsMom[,i] <- round(TRROC[,i] - TU1ROC, digits = 4)
# }

# AbsMom.df <- data.frame(coredata(AbsMom)) #Convert to dataframe & assigning coredata
# rownames(AbsMom.df) <- index(TRAdj) # Assigning the dates from S&P 500 as index for df
# AbsMom.z <- as.zoo(AbsMom.df) #Convert to zoo format
# AbsMom.x <- na.omit(as.xts(AbsMom.z)) #Convert to xts format
# names(AbsMom.x) <- colnames(DLxts) # Assign column labels from the variable 'Assets'

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
# RSABS <- cbind(t(last(Man.rp,1)), t(last(AbsMom.x,1)), t(last(Curr.Px,1)), t(last(EMA.x,1)))

### Identify 30- 90-, 250-day slopes of ETFs ###
stkreg.d <- cbind(DLxts, "Days" = 1:nrow(DLxts))

slope30 <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,
                                                                       30)$beta)))
slope90 <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,
                                                                       90)$beta)))
slopeYr <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,
                                                                       250)$beta)))
Beta30 <- slope30[,1:ncol(stkreg.d)-1]
Beta90 <- slope90[,1:ncol(stkreg.d)-1]
BetaYr <- slopeYr[,1:ncol(stkreg.d)-1]

names(Beta30) <- colnames(DLxts)
names(Beta90) <- colnames(DLxts)
names(BetaYr) <- colnames(DLxts)

### ADF P-Values to identify Possible Mean Reversion ###
### Maximum lag lengths 1:annual data, 4:quarterly data, 12:monthly data ###
#PValue <- as.data.frame(rollapply(tail(as.ts(DLxts),90), 30, function(u) adf.test(u,k=1)$p.value))
PValYr <- lapply(tail(DLxts,250), function(x) adf.test(x, k=1)$p.value)
PVal90 <- lapply(tail(DLxts,90), function(x) adf.test(x, k=4)$p.value)
PVal30 <- lapply(tail(DLxts,30), function(x) adf.test(x, k=12)$p.value)
PValues <- cbind(PVal30, PVal90, PValYr)

### Calculate 30-day MEAN for Reversion ###
Mean30 <- lapply(DLxts, function(x) mean(tail(x,30)))

#### Convert List to dataframe ####
df <- data.frame(matrix(unlist(PValues), nrow=nrow(PValues), byrow=F),stringsAsFactors=FALSE)
rownames(df) <- colnames(DLxts)

Avg30 <- data.frame(unlist(Mean30))

#### Rank the Momentum Measures ####
MRP.rk <- as.xts(t(apply(-Man.rp,1,rank)))
ABS.rk <- as.xts(t(apply(-AbsMom,1,rank)))
Mom.rk <- cbind(t(last(MRP.rk,1)), t(last(ABS.rk,1)))
MomAvg <- as.data.frame(rowMeans(Mom.rk))

RSABS <- cbind(t(last(Man.rp,1)), t(last(AbsMom,1)), MomAvg, t(last(Curr.Px,1)), t(last(EMA.x,1)),
               t(last(Beta30,1)), t(last(Beta90,1)), t(last(BetaYr,1)), round(df,2), round(Avg30,2))

RSABS.df <- as.data.frame(RSABS)
RSABSRpt.df <- cbind(Descr[,2],RSABS.df)
names(RSABSRpt.df) <- c("Description", "RS", "ABS", "Rank", "close", "30Wk", "Slope30",
                        "Slope90", "SlopeYr", "PVal30","PVal90", "PValYr", "Mean")

write.csv(RSABSRpt.df, file = "RSABS-Report.csv")

