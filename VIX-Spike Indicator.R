library(Quandl)
library(quantmod)
Sys.setenv(TZ = "UTC")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
VVIX <- Quandl("CBOE/VVIX", type = "xts", start_date = "2007-01-03")
VIX <- Quandl("CBOE/VIX", type = "xts", start_date = "2007-01-03")
suppressWarnings(getSymbols('^GSPC', src = "yahoo", from = "2007-01-03"))

###Identify Monthly Low Close###
names(VIX) <- c('Open', 'High', 'Low', 'Close')
VIX.lc <- runMin(VIX$Close, 21)
VIX.ll <- cbind(VIX, VIX.lc)
names(VIX.ll) <- c('Open', 'High', 'Low', 'Close', 'LC20')

###Identify the Spike >= 50% above the Monthly Low Close###
spk.lvl <- VIX.ll$LC20 * 1.5
VIX.lls <- cbind(na.omit(VIX.ll), na.omit(spk.lvl))
names(VIX.lls) <- c('Open', 'High', 'Low', 'Close', 'LC20', 'Spk50')

###Extract Date and Value of the intraday High >= 50% Spike###
spk.h <- matrix(0, nrow = nrow(VIX.lls), ncol = 1)
spk.date <- matrix(0, nrow = nrow(VIX.lls), ncol = 1)
for (i in 1:nrow(VIX.lls)) {
  if(VIX.lls$High[i] >= VIX.lls$Spk50[i]) {
    spk.date[i] <- index(VIX.lls)[i]
    spk.h[i] <- VIX.lls$High[i]
  }
}
VIX.llh <- cbind(na.omit(VIX.lls[,1:5]), na.omit(spk.h))
names(VIX.llh) <- c('Open', 'High', 'Low', 'Close', 'LC21', 'SpkHi')

###Identify 62% retracement from Spk50###
ret.lvl <- matrix(0, nrow = nrow(VIX.llh), ncol = 1)
for (i in 1:nrow(VIX.llh)){
  if (VIX.llh$SpkHi[i] > 0) {
    ret.lvl[i] <- VIX.llh$SpkHi[i] - ((VIX.llh$SpkHi[i]-VIX.llh$LC21[i])*.618)
  } else {
    ret.lvl[i] <- 0
  }
}


VIX.lsr <- cbind(na.omit(VIX.llh), na.omit(round(ret.lvl, 2)))
names(VIX.lsr) <- c('Open', 'High', 'Low', 'Close', 'LC21', 'SpkHi', 'Retrace')

VIX.1D <- na.omit(((VIX$Close-VIX$Open)/runSD(VIX$Close,21))) #VIX 1-Day spike

VIX.bb <- na.omit(BBands(VIX[,2:4], n = 20, sd = 2.5))
BB.width <- ((VIX.bb$up-VIX.bb$dn)/VIX.bb$mavg)*100
BB.data <- na.omit(cbind(tail(VIX[,2:3],2400), tail(VIX.bb[,1],2400), tail(VIX.bb[,3],2400), tail(VIX.1D,2400)))

VIX.sig <- matrix(0, nrow = nrow(BB.data), ncol = 1)

for (i in 1:nrow(BB.data)) {
  if (BB.data$High[i] >= BB.data$up[i] & BB.data$Close[i] >= 2) {
    VIX.sig[i] <- "XIV"
  } else {
  if (BB.data$Low[i] <= BB.data$dn[i] & BB.data$Close[i] <= -2) {
      VIX.sig[i] <- "VXX"
    }
  }
}

sig.df <- as.data.frame(VIX.sig)
bb.df <- as.data.frame(BB.data)
bb.sig <- cbind(bb.df, sig.df)
sig.xts <- as.xts(bb.sig)
names(sig.xts) <- c('High', 'Low', 'dn', 'up', 'Spike', 'Signal')


chartSeries(VIX['2016:/'], name = "VIX, 50SMA & 200SMA", theme =chartTheme('white'),
            TA = c(addSMA(200, col = 'blue'), addSMA(50, col = 'red')))

chartSeries(VIX['2016:/'], name = "VIX Bollinger Bands", theme = chartTheme('white'),
            TA = c(addBBands(20, sd = 2.5, maType = "SMA")))

View(tail(VIX.lsr,21))
View(tail(sig.xts,21))

