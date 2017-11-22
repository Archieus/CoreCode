library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(depmixS4)
library(reshape2)
library(ggplot2)
Sys.setenv(TZ = "EST5EDT")

Datalist <- read.table('regime', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)
DLxts <- na.locf(DLxts, na.rm = FALSE, fromLast = TRUE)
set.seed(2)

SPHB.mo <- to.period(DLxts$SPHB['2011-05-05:/'], period = 'months')
SPHB.adj <- SPHB.mo[,4]
SPHB.ts <- as.ts(SPHB.adj)

SPLV.mo <- to.period(DLxts$SPLV['2011-05-05:/'], period = 'months')
SPLV.adj <- SPLV.mo[,4]
SPLV.ts <- as.ts(SPLV.adj)

###Testing for non-stationality or explosive state###
SPHB.adf <- adf.test(tail(SPHB.ts, 36), alternative = c('stationary'), k = 1)
SPLV.adf <- adf.test(tail(SPLV.ts, 36), alternative = c('stationary'), k = 1)

###Hidden Markov Model###
SPHB.ROC <- ROC(SPHB.mo[,4], n=12)
SPHB.log <- Return.calculate(SPHB.mo[,4], method = 'log')
ModelDataHB.v <- na.omit(data.frame(SPHB.log, SPHB.ROC[,1]))
colnames(ModelDataHB.v) <- c("LogReturns", "ROC12")

SPLV.ROC <- ROC(SPLV.mo[,4], n=12)
SPLV.log <- Return.calculate(SPLV.mo[,4], method = 'log')
ModelData.v <- na.omit(data.frame(SPLV.log, SPLV.ROC[,1]))
colnames(ModelData.v) <- c("LogReturns", "ROC12")

Periods <- matrix(0,ncol = 1, nrow =1)
if(nrow(ModelData.v) < 195) {
  Periods <- as.integer(nrow(ModelData.v))
} else {Periods <- 195}

####HIDDEN MARKOV MODEL RATE OF CHANGE####
HMMHB <- depmix(list(LogReturns~1,ROC12~1), data = ModelDataHB.v, nstates = 2, family = list(gaussian(), gaussian()))
HMMfitHB <- fit(HMMHB, verbose = FALSE) # fit the model to the data set
print(HMMfitHB) #Compare the log likelihood as well as the AIC and BIC values to help choose a model
summary(HMMfitHB) # Transition matrix gives the probability of moving from one state to the next
HMMprobHB <- posterior(HMMfitHB) #find the posterior odds for each state over the data set
rowSums(head(HMMprobHB)[,2:3])          # Check that probabilities sum to 1
pBearHB <- tail(HMMprobHB[,2], 195) #Pick out "Bear" state or Low Volatility State

HMM <- depmix(list(LogReturns~1,ROC12~1), data = ModelData.v, nstates = 2, family = list(gaussian(), gaussian()))
HMMfit <- fit(HMM, verbose = FALSE) # fit the model to the data set
print(HMMfit) #Compare the log likelihood as well as the AIC and BIC values to help choose a model
summary(HMMfit) # Transition matrix gives the probability of moving from one state to the next
HMMprob <- posterior(HMMfit) #find the posterior odds for each state over the data set
rowSums(head(HMMprob)[,2:3])          # Check that probabilities sum to 1
pBear <- tail(HMMprob[,2], 195) #Pick out "Bear" state or Low Volatility State
##########################################

### Identify Bull and Bear States for SPHB & SPLV ###
summaryMatHB <- data.frame(summary(HMMfitHB))
colnames(summaryMatHB) <- c("Intercept", "SD")
bullstateHBM <- which(summaryMatHB$Intercept > 0.00)
bearstateHBM <- which(summaryMatHB$Intercept < 0.00)

summaryMat <- data.frame(summary(HMMfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullstateLVM <- which(summaryMat$Intercept > 0.00)
bearstateLVM <- which(summaryMat$Intercept < 0.00)

### Loop to automate identification of Bull State ###
if(summaryMatHB[1,1] > 0 & summaryMatHB[2,1] > 0) {
  bullstateHB <- 3
}else {
  if(summaryMatHB[2,1] > summaryMatHB[1,1] & summaryMatHB[2,1] > 0) {
  bullstateHB <- 2
  }else {
    if(summaryMatHB[1,1] > summaryMatHB[2,1] & summaryMatHB[1,1] > 0) {
    bullstateHB <- 1}
}
}

if(summaryMat[1,1] > 0 & summaryMat[2,1] > 0) {
  bullstate <- 3
}else {
  if(summaryMat[2,1] > summaryMat[1,1] & summaryMat[2,1] > 0){
  bullstate <- 2
  }else {
    if(summaryMat[1,1] > summaryMat[2,1]  & summaryMat[1,1] > 0) {
    bullstate <- 1}
}
}

################################     
###Build Dataframe for ggplot###
################################

SPHB.mo$logret <- log(SPHB.mo[,4]) - lag(log(SPHB.mo[,4]))
SPHB.mo <- na.exclude(SPHB.mo)
SPHB.df <- data.frame(tail(SPHB.mo, Periods))
SPHB.df$Date <-as.Date(row.names(SPHB.df),"%Y-%m-%d")
SPHB.df$pBear <- tail(pBearHB, Periods)   # Put pBear in the data frame for plotting   
datafHB <- melt(SPHB.df[(nrow(SPHB.df)-25):(nrow(SPHB.df)),4:7],id="Date",measure=c("logret",'pBear')) ##[row, Columns]

SPLV.mo$logret <- log(SPLV.mo[,4]) - lag(log(SPLV.mo[,4]))
SPLV.mo <- na.exclude(SPLV.mo)
SPLV.df <- data.frame(tail(SPLV.mo, Periods))
SPLV.df$Date <-as.Date(row.names(SPLV.df),"%Y-%m-%d")
SPLV.df$pBear <- tail(pBear, Periods)   # Put pBear in the data frame for plotting   
dataf <- melt(SPLV.df[(nrow(SPLV.df)-25):(nrow(SPLV.df)),4:7],id="Date",measure=c("logret",'pBear')) ##[row, Columns]

#######Alternative Plot#######
States.HMMHB <- HMMprobHB$state
StatesHB.df <- data.frame(last(States.HMMHB, Periods)) #States
SPHB.df <- data.frame(last(SPHB.mo[,5], Periods)) #Log Return
ROC.df <- data.frame(last(SPHB.ROC, Periods)) #ROC
SPHB.df <- cbind(SPHB.df, ROC.df, StatesHB.df)
SPHB.z <- as.zoo(SPHB.df)
SPHB.x <- as.xts(SPHB.z)
names(SPHB.x) <- c('Log', 'ROC', 'State')

States.HMM <- HMMprob$state
States.df <- data.frame(last(States.HMM, Periods)) #States
SPLV.df <- data.frame(last(SPLV.mo[,5], Periods)) #Log Returns
ROC.df <- data.frame(last(SPLV.ROC, Periods)) #ROC
SPLV.df <- cbind(SPLV.df, ROC.df, States.df)
SPLV.z <- as.zoo(SPLV.df)
SPLV.x <- as.xts(SPLV.z)
names(SPLV.x) <- c('Log', 'ROC', 'State')

layout(1:3)
par(col = "blue", lwd = 2)
plot(runCor(SPHB.x$Log,SPLV.x$Log), main = "Running Correlation SPHB:SPLV")
par(col = "purple", lwd = 2)
plot(SPHB.mo['2012-09:/'][,4]/SPLV.mo['2012-09:/'][,4], main = "Rel Performance SPHB/SPLV",
     sub = "Rising indicates High Beta doing better than Low Volatility")
par(col = "green", lwd = 2)
plot(SPHB.x$State['2012-09:/'], type = 'l', main = '')
par(new = TRUE, col = "red", lwd = 2)
plot(SPLV.x$State['2012-09:/'], type = 'l', main = '')

#Plot SPLV Regimes
par(col = "blue", lwd = 2)
plot(SPLV.log['2012-09:/'], type = 'h', main = "SPLV Log Return")
par(col = "purple", lwd = 2)
plot(SPLV.ROC, main = "SPLV ROC")
par(col = "red")
plot(SPLV.x$State['2012-09:/'], type = 'l', main = "SPLV Regime/State")

#Plot SPHB Regimes
par(col = "blue", lwd = 2)
plot(SPHB.log['2012-09:/'], type = 'h', main = "SPHB Log Return")
par(col = "purple", lwd = 2)
plot(SPHB.ROC, main = "SPHB ROC")
par(col = "red")
plot(SPHB.x$State['2012-09:/'], type = 'l', main = "SPHB Regime/State")


####Generate Signals and Weights####
Signals <- as.data.frame(cbind(HMMprobHB[,1], round(HMMprobHB[,3],4), HMMprob[,1], round(HMMprob[,3],4)))
colnames(Signals) <- c("HB", "HBBull", "LV", "LVBull")
rownames(Signals) <- index(SPHB.x)
Sigs.x <- as.xts(Signals)

HBWts <- matrix(0, nrow = nrow(Signals), ncol = 1)
for( i in 1:nrow(Signals)) {
  if(Signals$HB[i] == bullstateHB && Signals$LV[i] != bullstate) {
    HBWts[i] <- round(Signals$HBBull[i],0)
  }else if (Signals$HB[i] != bullstateHB) {
    HBWts[i] <- 0
  }else {HBWts[i] <- round(Signals$HBBull[i]/2,2)}
}

LVWts <-matrix(0, nrow = nrow(Signals), ncol = 1)
for(i in 1:nrow(Signals)) {
  if(Signals$LV[i] == bullstate && Signals$HB[i] != bullstateHB) {
    LVWts[i] <- round(Signals$LVBull[i],0)
  }else if(Signals$LV[i] != bullstate) {
    LVWts[i] <- 0
  }else {LVWts[i] <- round(Signals$LVBull[i]/2,2)}
}

BILwt <- 1-HBWts-LVWts

Sig.nals <- cbind(HBWts, LVWts, BILwt)
colnames(Sig.nals) <- c("SPHB", "SPLV", "BIL")
Signals.df <- as.data.frame(coredata(Sig.nals))
rownames(Signals.df) <- index(Sigs.x)

#write.csv(Signals.df, file = "HBLVSigs.csv")

cat('\n', '\n', "SPHB Bullish State represented by State(s):",
   bullstateHBM, "=", summaryMatHB$Intercept[bullstateHBM], '\n',
   "Bearish State represented by State(s):", bearstateHBM, "=", summaryMatHB$Intercept[bearstateHBM],
   '\n',"Current SPHB State:", last(HMMprobHB$state), '\n', "Probability State 1:", round(last(HMMprobHB$S1),4)*100, '\n',
   "Probability State 2:", round(last(HMMprobHB$S2),4)*100, '\n', '\n',
   "SPLV Bullish State represented by State(s):",
   bullstateLVM, "=", summaryMat$Intercept[bullstateLVM], '\n',
   "Bearish State represented by State(s):", bearstateLVM, "=", summaryMat$Intercept[bearstateLVM], '\n',
   "Current SPLV State:", last(HMMprob$state), '\n', "Probability State 1:", round(last(HMMprob$S1),4)*100, '\n',
   "Probability State 2:", round(last(HMMprob$S2),4)*100, '\n')


