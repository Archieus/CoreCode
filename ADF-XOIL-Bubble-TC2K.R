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
#DLxts <- na.locf(DLxts, na.rm = FALSE, fromLast = TRUE)

XOI.mo <- to.period(na.locf(DLxts[,6]['2003-02-21:/'],na.rm = FALSE, fromLast = TRUE), period = 'months')
XOI.adj <- XOI.mo[,4]
XOI.ts <- as.ts(XOI.adj)

###Testing for non-stationality or explosive state###
XOIL.adf <- adf.test(tail(XOI.ts, 36), alternative = c('stationary'), k = 1)

###Hidden Markov Model###
XOIL.ROC <- ROC(XOI.mo[,4], n=12)
XOIL.log <- Return.calculate(XOI.mo[,4], method = 'log')
XModelData.v <- na.omit(data.frame(XOIL.log, XOIL.ROC[,1]))
colnames(XModelData.v) <- c("LogReturns", "ROC12")
### Setting the log returns & ROC as the response variables, using the dataframe for the XOI.
### We are setting up two regimes (Bull & Bear) based on 12-month ROC.  We are setting the
### response distribution to be gaussian.
########################################

XHMM <- depmix(list(LogReturns~1,ROC12~1), data = XModelData.v, nstates = 3, family = list(gaussian(), gaussian()))
#set.seed(1)
XHMMfit <- fit(XHMM, verbose = FALSE) # fit the model to the data set
print(XHMMfit) #Compare the log likelihood as well as the AIC and BIC values to help choose a model
summary(XHMMfit) # Transition matrix gives the probability of moving from one state to the next

### Identify Bull and Bear States ###
summaryMat <- data.frame(summary(XHMMfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullstate <- which(summaryMat$Intercept > 0)
bearstate <- which(summaryMat$Intercept < 0)

XHMMprob <- posterior(XHMMfit) #find the posterior odds for each state over the data set
rowSums(head(XHMMprob)[,2:4])          # Check that probabilities sum to 1
XpBear <- tail(XHMMprob[,2], 160)

################################     
###Build Dataframe for ggplot###
################################

XOI.mo$logret <- log(XOI.mo[,4]) - lag(log(XOI.mo[,4]))
XOI.mo <- na.exclude(XOI.mo)
XOIL.df <- data.frame(tail(XOI.mo, 160))
XOIL.df$Date <-as.Date(row.names(XOIL.df),"%Y-%m-%d")
XOIL.df$XpBear <- tail(XpBear, 160)   # Put XpBear in the data frame for plotting   
Xdataf <- melt(XOIL.df[(nrow(XOIL.df)-25):(nrow(XOIL.df)),4:7],id="Date",measure=c("logret",'XpBear')) ##[row, Columns]

##Plot the log return time series along withe the time series of probabilities##

# qplot(Date,value,data=Xdataf,geom="line",
#       main = "SP 500 Log returns and 'Bear' state probabilities",
#       ylab = "") + 
#   facet_grid(variable ~ ., scales="free_y")

#######Alternative Plot#######
States.XHMM <- XHMMprob$state
States.df <- data.frame(last(States.XHMM, 160))
XOI.df <- data.frame(last(XOI.mo[,5], 160))
ROC.df <- data.frame(last(XOIL.ROC, 160))
XOI.df <- cbind(XOI.df, ROC.df, States.df)
XOI.z <- as.zoo(XOI.df)
XOI.x <- as.xts(XOI.z)
names(XOI.x) <- c('Log', 'ROC', 'State')

layout(1:3)
par(col = 'blue', lwd = 2)
plot(XOI.x$Log['1980:/'], type = 'h', main = 'XOI Monthly Log Returns')
par(col = 'red', lwd = 2)
plot(XOI.x$ROC['1980:/'], type = 'l', main = 'XOI Monthly Rate of Change')
par(col = "darkgreen", lwd = 2)
plot(XOI.x$State['1980:/'], type = 'l', main = 'XOI State Regime')

#plot(XHMMprob[,2], type = 's', main = 'State 1')
#plot(XHMMprob[,3], type = 's', main = 'State 2')
#plot(XHMMprob[,4], type = 's', main = 'State 2') 

###################################################
###Print Results of Augmented Dickey Fuller Test###
###################################################

if(XOIL.adf$p.value > .05) {
  cat('\n', '\n','The rolling 36-month p-value of', round(XOIL.adf$p.value, 2), 'is greater than .05 thereby accepting the null hypothesis', '\n',
      'that the XOIL is non-stationary (non-mean reverting) and can be in a short-term bubble state.', '\n')
}else {
  cat('\n', '\n', 'The rolling 36-month p-value of', round(XOIL.adf$p.value, 2), ' is less than .05, thereby rejecting the null hypothesis', '\n',
      'that the XOIL is stationary or mean-reverting.', '\n')
}

cat('\n', '\n', "Normal or Bullish State represented by column(s):",
    bullstate, "=", summaryMat$Intercept[bullstate], '\n',
    "Bearish State represented by column(s):", bearstate, "=", summaryMat$Intercept[bearstate], '\n')

round(tail(XHMMprob),4)

