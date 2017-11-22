library(quantmod)
library(PerformanceAnalytics)
# AlphaVantage API Key: 4FS12H08TP68SJV2

#### Read CSV for Symbols List ###
symblist <- read.csv("CLCFixed.csv", header = FALSE)
symbol <- t(symblist)

####Download ADJUSTED PRICE DATA from AlphaVantage
###outputsize=c(full,compact) full= 20 years of data, compact = 100 datapoints

#### CREATE COMPONENTS FOR API CALL ####
apikey <- "&outputsize=full&apikey=4FS12H08TP68SJV2&datatype=csv"
URLbase <- "http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol="

cu <-NULL
ru <-NULL

### Loop to download data for all symbols in list as its own object ###

for(i in 1:length(symbol)){
  cu[i] <- paste0(URLbase, symbol[i])
  ru[i] <- paste0(cu[i],apikey)
  assign(paste(symbol[i]), read.csv(ru[i]))
}

### Extract Open, High, Low, Close, Volume, Adjusted Close (Quantmod format) ###

for(i in 1:length(symbol)) {
  assign(paste0(symbol[i], ".x"), cbind(get(symbol[,i], envir = .GlobalEnv)$open, get(symbol[,i], envir = .GlobalEnv)$high,
                                        get(symbol[,i], envir = .GlobalEnv)$low, get(symbol[,i], envir = .GlobalEnv)$close, 
                                        get(symbol[,i], envir = .GlobalEnv)$volume, get(symbol[,i], envir = .GlobalEnv)$adjusted_close))
}

# #### Create a Matrix of Adjusted Close Values ####
# AC <- matrix(0, ncol = ncol(symbol), nrow = 745)
# for(i in 1:length(symbol)){
#   AC[,i] <- cbind(get(symbol[,i], envir = .GlobalEnv)$adjusted_close)
# }

AC <-  cbind(head(AGG.x[,6],745), head(CWB.x[,6],745), head(EMB.x[,6],745), head(FLOT.x[,6],745),
             head(FPE.x[,6],745), head(GOVT.x[,6],745), head(HYMB.x[,6],745), head(LMBS.x[,6],745),
             head(NEAR.x[,6],745),head(PZA.x[,6],745), head(SPAB.x[,6],745), head(SPIB.x[,6],745),
             head(SPLB.x[,6],745),head(SRLN.x[,6],745))


#### Extract Date Values to be used as an XTS Index 
Dates <- get(symbol[1], envir = .GlobalEnv)$timestamp
df <- matrix(unlist(Dates))

AC.df <- as.data.frame(AC)
colnames(AC.df) <- symbol
row.names(AC.df) <- head(df,745)

#### Create the XTS Object to be used for analysis ####
AC.zoo <- as.zoo(AC.df)
AdjClose <- as.xts(AC.zoo)

#### BEGIN EXPONENTIAL REGRESSION CALCULATIONS ####
FIreg <- cbind(AdjClose, "Days" = 1:nrow(AdjClose))

reg.r2 <- do.call(merge, lapply(FIreg, function(x) na.omit(rollSFM(log(x),FIreg$Days, 90)$r.squared)))

r.sqrd <- (reg.r2[,1:ncol(FIreg)-1])
names(r.sqrd) <- colnames(AdjClose)

slope.b <- do.call(merge, lapply(FIreg, function(x) na.omit(rollSFM(log(x), FIreg$Days,90)$beta)))

Be.ta <- slope.b[,1:ncol(FIreg)-1]
names(Be.ta) <- colnames(AdjClose)

Ann.sl <- ((exp(Be.ta)^250)-1)*100
names(Ann.sl) <- colnames(AdjClose)
Adj.sl <- round((r.sqrd * Ann.sl),4)
names(Adj.sl) <- colnames(AdjClose)
#### END EXPONENTIAL REGRESSION CALCULATIONS ####

### RANK Exponential Regression Results where "1" is the best ###
ExpReg.rk <- as.xts(t(apply(-Adj.sl,1,rank)))

#### CALCULATE RETURNS AND CONVERT TO MONTHLY FOR PERFORMANCE CALCULATIONS ####
Fixed.Ret <- na.omit(Return.calculate(AdjClose, method = ("discrete")))

FIWeights <- read.csv("ExpRegWts.csv")
FIWeights.z <- read.zoo("ExpRegWts.csv", header = TRUE, sep = ",")
FIWeights.x <- as.xts(FIWeights.z)

FIRet.ep <- endpoints(Fixed.Ret, on = 'months', k=1) 
FIRet.mo <- Fixed.Ret[,2:14][FIRet.ep]

AGGret.mo <- Fixed.Ret[,1][FIRet.ep]

FixedRet <- round(Return.portfolio(FIRet.mo, weights = FIWeights.x, wealth.index = FALSE, 
                                   contribution = FALSE, geometric = TRUE), digits = 6)

#### GENERATE PORTFOLIO STATISTICS RESULTS FOR PORTFOLIO AND BENCHMARK ####
table.CalendarReturns(FixedRet)
table.CalendarReturns(AGGret.mo['2015-11:/'])

SharpeRatio(FixedRet, .01/12, FUN = "StdDev")
SharpeRatio(AGGret.mo['2015-11:/'], .01/12, FUN = "StdDev")

Return.annualized(FixedRet)
Return.annualized(AGGret.mo['2015-11:/'], scale = 12)

sd(FixedRet)
sd(AGGret.mo['2015-11:/'])
