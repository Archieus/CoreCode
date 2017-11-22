library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(depmixS4)
library(reshape2)
library(ggplot2)
Sys.setenv(TZ = "UTC")

Datalist <- read.table('regime', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)
DLxts <- na.locf(DLxts, na.rm = FALSE, fromLast = TRUE)

GSPC.mo <- to.period(DLxts[,2], period = 'months')
GSPC.adj <- GSPC.mo[,4]
GSPC.ts <- as.ts(GSPC.adj)

###Testing for non-stationality or explosive state###
SP500.adf <- adf.test(tail(GSPC.ts, 36), alternative = c('stationary'), k = 1)

###Rolling 36-month ADF p-Value using rollapply() or rollapplyr() for align "right" ###
#rollapply(GSPC.ts, 36, function(u) adf.test(u, k=1)$p.value)

###Hidden Markov Model###
SP500.ohlc <- GSPC.mo[,1:4] #subset ohlc data for Historical Volatility calculation

SP500.ROC <- ROC(GSPC.adj, n=12)
SP500.log <- Return.calculate(GSPC.mo[,4], method = 'log')
ModelData.v <- na.omit(data.frame(SP500.log, SP500.ROC[,1]))
colnames(ModelData.v) <- c("LogReturns", "ROC12")
### Setting the log returns & ROC as the response variables, using the dataframe for the S&P.
### We are setting up two regimes (Bull & Bear) based on 12-month ROC.  We are setting the
### response distribution to be gaussian.
########################################

HMM <- depmix(list(LogReturns~1,ROC12~1), data = ModelData.v, nstates = 3, family = list(gaussian(), gaussian()))
set.seed(1)
HMMfit <- fit(HMM, verbose = FALSE) # fit the model to the data set
print(HMMfit) #Compare the log likelihood as well as the AIC and BIC values to help choose a model
summary(HMMfit) # Transition matrix gives the probability of moving from one state to the next

### Identify Bull and Bear States ###
summaryMat <- data.frame(summary(HMMfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullstate <- which(summaryMat$Intercept > 0)
bearstate <- which(summaryMat$Intercept < 0)

HMMprob <- posterior(HMMfit) #find the posterior odds for each state over the data set
rowSums(head(HMMprob)[,2:4])          # Check that probabilities sum to 1
pBear <- tail(HMMprob[,2], 600)

################################     
###Build Dataframe for ggplot###
################################

GSPC.mo$logret <- log(GSPC.mo[,4]) - lag(log(GSPC.mo[,4]))
GSPC.mo <- na.exclude(GSPC.mo)
SP500.df <- data.frame(tail(GSPC.mo, 600))
SP500.df$Date <-as.Date(row.names(SP500.df),"%Y-%m-%d")
SP500.df$pBear <- tail(pBear, 600)   # Put pBear in the data frame for plotting   
dataf <- melt(SP500.df[(nrow(SP500.df)-150):(nrow(SP500.df)),4:7],id="Date",measure=c("logret",'pBear')) ##[row, Columns]

##Plot the log return time series along withe the time series of probabilities##

# qplot(Date,value,data=dataf,geom="line",
#       main = "SP 500 Log returns and 'Bear' state probabilities",
#       ylab = "") + 
#   facet_grid(variable ~ ., scales="free_y")

#######Alternative Plot#######
States.HMM <- HMMprob$state
States.df <- data.frame(last(States.HMM, 600)) #States
GSPC.df <- data.frame(last(GSPC.mo[,5], 600)) # Log Return
ROC.df <- data.frame(last(SP500.ROC, 600)) # ROC
GSPC.df <- cbind(GSPC.df, ROC.df, States.df)
GSPC.z <- as.zoo(GSPC.df)
GSPC.x <- as.xts(GSPC.z)
names(GSPC.x) <- c('Log', 'ROC', 'State')

layout(1:3)
par(col = 'blue', lwd = 2)
plot(GSPC.x$Log['1990:/'], type = 'h', main = 'S&P Monthly Log Returns')
par(col = 'red', lwd = 2)
plot(GSPC.x$ROC['1990:/'], type = 'l', main = 'S&P Monthly Rate of Change')
par(col = "darkgreen", lwd = 2)
plot(GSPC.x$State['1990:/'], type = 'l', main = 'S&P State Regime')

#plot(HMMprob[,2], type = 's', main = 'State 1')
#plot(HMMprob[,3], type = 's', main = 'State 2')
#plot(HMMprob[,4], type = 's', main = 'State 3') 

###################################################
###Print Results of Augmented Dickey Fuller Test###
###################################################
write.csv(tail(HMMprob), file = "Regime.csv")


#### SAVE PLOT TO PNG ####
png(filename="regime.png", width = 740, height = 522)
layout(1:3)
par(col = 'blue', lwd = 2)
plot(GSPC.x$Log['1990:/'], type = 'h', main = 'S&P Monthly Log Returns')
par(col = 'red', lwd = 2)
plot(GSPC.x$ROC['1990:/'], type = 'l', main = 'S&P Monthly Rate of Change')
par(col = "darkgreen", lwd = 2)
plot(GSPC.x$State['1990:/'], type = 'l', main = 'S&P State Regime')
dev.off()

if(SP500.adf$p.value > .05) {
  cat('\n', '\n','The rolling 36-month p-value of', round(SP500.adf$p.value, 2), 'is greater than .05 thereby accepting the null hypothesis', '\n',
      'that the SP500 is non-stationary (non-mean reverting) and can be in a short-term bubble state.', '\n')
}else {
  cat('\n', '\n', 'The rolling 36-month p-value of', round(SP500.adf$p.value, 2), ' is less than .05, thereby rejecting the null hypothesis', '\n',
      'that the SP500 is stationary or mean-reverting.', '\n')
}



cat('\n', '\n', "Normal or Bullish State represented by:",
    bullstate, "=", summaryMat$Intercept[bullstate], '\n',
    "Bearish State represented by:", bearstate, "=", summaryMat$Intercept[bearstate], '\n')

round(tail(HMMprob),4)

