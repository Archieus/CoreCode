library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(Quandl)

Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = "UTC")

RankRB <- function(x){
  # Computes the rank of an xts object of ranking factors
  # ranking factors are the factors that are ranked (i.e. asset returns)
  #
  # args:
  #   x = xts object of ranking factors
  #
  # Returns:
  #   Returns an xts object with ranks
  #   (e.g. for ranking asset returns, the asset with the greatest return
  #    receives a  rank of 1)
  
  r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}

#MonthlyAd <- function(x){
  # Converts daily data to monthly and returns only the monthly close
  # Note: only used with Yahoo Finance data so far
  # Thanks to Joshua Ulrich for the Monthly Ad function
  #
  # args:
  #   x = daily price data from Yahoo Finance
  #
  # Returns:
  #   xts object with the monthly adjusted close prices

#  sym <- sub("\\..*$", "", names(x)[1])
#  Ad(to.monthly(x, indexAt = 'lastof', drop.time = TRUE, name = sym))
#}

CAGR <- function(x, m){
  # Function to compute the CAGR given simple returns
  #
  # args:
  #  x = xts of simple returns
  #  m = periods per year (i.e. monthly = 12, daily = 252)
  #
  # Returns the Compound Annual Growth Rate
  x <- na.omit(x)
  cagr <- apply(x, 2, function(x, m) prod(1 + x)^(1 / (length(x) / m)) - 1, m = m)
  return(cagr)
}

SimpleMomentumTest <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3){
  # returns a list containing a matrix of individual asset returns
  # and the combined returns
  # args:
  #  xts.ret = xts of one period returns
  #  xts.rank = xts of ranks
  #  n = number of top ranked assets to trade
  #  ret.fill.na = number of return periods to fill with NA
  #
  # Returns:
  #  returns an xts object of simple returns
  
  # trade the top n asset(s)
  # if the rank of last period is less than or equal to n,
  # then I would experience the return for this month.
  
  # lag the rank object by one period to avoid look ahead bias
  lag.rank <- lag(xts.rank, k = 1, na.pad = TRUE)
  n2 <- nrow(lag.rank[is.na(lag.rank[,1]) == TRUE])
  z <- max(n2, ret.fill.na)
  
  # for trading the top ranked asset, replace all ranks above n
  # with NA to set up for element wise multiplication to get
  # the realized returns
  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA
  # set the element to 1 for assets ranked <= to rank
  lag.rank[lag.rank <= n] <- 1
  
  # element wise multiplication of the
  # 1 period return matrix and lagged rank matrix
  mat.ret <- as.matrix(xts.ret) * lag.rank
  
  # average the rows of the mat.ret to get the
  # return for that period
  vec.ret <- rowMeans(mat.ret, na.rm = TRUE)
  vec.ret[1:z] <- NA
  
  # convert to an xts object
  vec.ret <- xts(x = vec.ret, order.by = index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.ret, rank = lag.rank)
  return(f)
}

Datalist <- read.table('momplus', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- na.omit(as.xts(DLzoo))

StartDate <- first(index(DLxts))

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

# Calculate the RS for the stocks

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
MRP.mo <- Man.rp[endpoints(Man.rp, 'months')]

# Calculate Excess Return or Absolute Momentum versus Short-term Treasury
AbsMom <- round(na.omit(do.call(cbind, lapply(TRROC, function(x) {x - TU1ROC[,1]}))),4)

rank.ABS <- as.xts(t(apply(-AbsMom,1,rank)))
rank.MRP <- as.xts(t(apply(-MRP.mo,1,rank)))

RSAbs <- (rank.ABS + rank.MRP)/2
rank.RSAbs <- as.xts(t(apply(RSAbs, 1, rank))) #use "-RSAbs" if best (highests value) is to be ranked "1".
write.csv(as.data.frame(rank.RSAbs), file = "RankedRSAbs.csv", row.names = TRUE)

          