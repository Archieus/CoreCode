library(Quandl)
library(quantmod)
Sys.setenv(TZ = "EST5EDT")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
VVIX <- Quandl("CBOE/VVIX", type = "xts", start_date = "2007-01-03")
SKEW <- Quandl("CBOE/SKEW", type = 'xts', start_date = "2007-01-03")
VIX <- Quandl("CBOE/VIX", type = "xts", start_date = "2007-01-03")
names(VIX) <- c('Open', 'High', 'Low', 'Close')

PUTCALL <- Quandl('CBOE/SPX_PC', type = "xts", start_date = "2007-01-03")

getSymbols('^GSPC', src = "yahoo", from = "2007-01-03")
GSPC <- na.locf(GSPC, na.rm = FALSE, fromLast = TRUE)
SP500.mo <- to.period(GSPC, period = 'months')

VIXLC.mo <- runMin(VIX$Close, 1000)
VIX.lc <- last(VIXLC.mo, 1000)
VIX.h <- last(VIX$High, 1000)
###Identify VIX Spike of 50% or greater###
VIX.spk <- matrix(0, nrow = 1000, ncol = 1)
for (i in 1:1000){
  if(VIX.h[i] >= (1.50*VIX.lc[i])) {
    VIX.spk[i] <- VIX.h[i]
  }
}
###Identify Close >= 62% Retracement from High Spike
VIX62.ret <- VIX.spk * .62
VIX.md <- matrix(nrow = 1000, ncol = 1)
VIX.c <- last(VIX$Close, 1000)
for (i in 1:1000) {
  if(VIX.c[i] <= VIX62.ret[i]) {
    VIX.md[i] <- VIX.c[i]
  } 
}

VIX.df <- data.frame(VIX.md)
rownames(VIX.df) <- index(last(VIX, 1000))
VIX.sdf <- data.frame(VIX.spk)
rownames(VIX.sdf) <- index(last(VIX,1000))
Spike.Ret <- cbind(VIX.df, VIX.sdf)

VVIX.mo <- to.period(VVIX, period = 'months')
VVIX.ROC <- na.omit(ROC(VVIX.mo, 12))

VIX.mo <- to.period(VIX, period = 'months')

### Calculate Ulcer Index ###
MaxPx <- (runMax(na.locf(GSPC[,2], na.rm = FALSE, fromLast = TRUE),21))

Ri.fun <- function(x,y) {100*((x-y)/y)}
Ri <- na.omit(Ri.fun(GSPC[,4], MaxPx))
Ri.mu <- na.omit(runMean(Ri,21))
Ulcer <- sqrt(abs(Ri.mu))
# Regression of Ulcer Index & SKEW Index
Ulcer.reg <- rollSFM(Ulcer, .index(Ulcer))
Ulcer.rma <- Ulcer.reg$alpha +Ulcer.reg$beta*.index(Ulcer) ## Regressed Moving Average ##

SKEW.reg <- rollSFM(SKEW, .index(SKEW))
SKEW.rma <- SKEW.reg$alpha + SKEW.reg$beta*.index(SKEW)

## Plot Data Series ##
par(col = 'red', mar = c(5,4,4,5))
plot(last(VVIX, 252), type = "l", lty = 2, xlab = '', ylab = '', main ='')
axis(side = 2)
mtext(side = 2, line = 2, 'Volatility of VIX (- -)')
par(new = TRUE, col = 'green')
plot(last(GSPC[,6], 252), type = "l", col = "green", axes = FALSE, bty = "n", xlab = "", ylab = "",
     main ='VVIX vs S&P')
axis(side = 4)
mtext(side = 4, line = 2, 'S&P 500')

par(col = 'dark red', lty = 1, mar = c(5,4,4,5))
plot(last(VIX[,4], 252), type = "l", lty = 2, xlab = '', ylab = '', main ='')
axis(side = 2)
mtext(side = 2, line = 2, 'VIX Index(- -)')
par(new = TRUE, col = 'dark green')
plot(last(GSPC[,6], 252), type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", main = 'VIX vs S&P')
axis(side = 4)
mtext(side = 4, line =2, 'S&P 500')

### Plot VVIX 12-mo ROC and monthly VIX ###
par(col = 'black', lty = 1, mar = c(5,4,4,5))
plot(VVIX.ROC[,4]['2010:/'], type = 'l', lty = 2, xlab = '', ylab ='', main ='VVIX 12-Mo ROC vs VIX' )
axis(side = 2)
mtext(side = 2, line  = 2, 'VVIX 12-mo ROC (- -)')
par(new = TRUE, col = 'red')
plot(VIX.mo[,4]['2010:/'], type = 'l', axes = FALSE, bty = 'n', xlab = '', ylab = '', main = '')
axis(side = 4)
mtext(side = 4, line = 2, 'Monthly VIX')

### Plot Ulcer Index ###
layout(1:3)
chart_Series(Ulcer['2015:/'], TA = "add_TA(Ulcer.rma, on = 1)", name = "Ulcer Index")
chart_Series(SKEW['2015:/'], TA = "add_TA(SKEW.rma, on = 1)", name = "SKEW Index")
chart_Series(GSPC['2015:/'])

### Plot Daily VIX and Skew Index ###
layout(1:3)
chart_Series(last(VIX[,4],252))
chart_Series(last(SKEW,252), TA = "add_TA(BBands(SKEW)$up, on = 1)")
chart_Series(last(GSPC,252))

###
layout(1:2)
VVIX.reg <- rollSFM(VVIX, .index(VVIX), 90)
VVIX.rma <- VVIX.reg$alpha + VVIX.reg$beta*.index(VVIX)
chart_Series(VVIX['2015:/'], TA = "add_TA(VVIX.rma,on = 1)")

VIX.reg <- rollSFM(VIX[,4], .index(VIX), 90)
VIX.rma <- VIX.reg$alpha + VIX.reg$beta*.index(VIX)
chart_Series(VIX[,4]['2015:/'], TA = "add_TA(VIX.rma, on = 1)")

