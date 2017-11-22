library(Quandl)
library(quantmod)
library(urca)
Sys.setenv(TZ = "UTC")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

VVIX <- Quandl("CBOE/VVIX", type = "xts", start_date = "2007-01-03")
VIX <- Quandl("CBOE/VIX", type = "xts", start_date = "2007-01-03")
suppressWarnings(getSymbols('^GSPC', src = "yahoo", from = "2007-01-03"))

HV610.vix <- (volatility(VIX, n = 6, calc = 'close', N = 252))/
  (volatility(VIX, n = 10, calc = 'close', N = 252))
HV625.vix <- (volatility(VIX, n = 6, calc = 'close', N = 252))/
  (volatility(VIX, n = 25, calc = 'close', N = 252))
names(HV610.vix) <- 'HV610'
names(HV625.vix) <- 'HV625'
HV.comb <- cbind(HV610.vix, HV625.vix)

###Check for mean reversion of the VIX Volatility Measures
HV610.ts <- as.ts(HV610.vix)
HV610.ADF <- ur.df(na.omit(HV610.ts, lags = 12, type = 'trend')) #urca ADF test (test Statistic)
HV625.ts <- as.ts(HV625.vix)
HV625.ADF <- ur.df(na.omit(HV625.ts, lags = 12, type = 'trend')) #urca ADF test (test Statistic)
cat("\n", "Null hyptohesis = there is no cointegrating relationship (no mean reversion)", "\n",
    "Reject null hypothesis if (negative t-stat) < (negative crit Value)", "\n",
    "Reject null hypothesis if (positive t-stat) > (positive crit value)", "\n", "\n")

###
summary(HV610.ADF)
summary(HV625.ADF)

### If 5-day-Minimum 6/10 & 6/25 HV of VIX is below 50%, look for increased volatility in the market ###
if (last(runMin(HV610.vix,3)) < .5 && last(runMin(HV625.vix,3)) < .5) {
  cat("\n", "HEDGE - HV610 =", round(last(runMin(HV610.vix,3)), digits = 2),
      "and HV625 =", round(last(runMin(HV625.vix,3)), digits = 2), "are both below 50% w/in the last 3 days.",
      "\n", "\n", "30-Day Mean of VIX", round(mean(tail(VIX,30)),2), " versus Current VIX Close:", last(VIX[,4]), "\n", "\n")
} else {
  cat("\n", "\n", "NO HEDGE - HV610 =", round(last(runMin(HV610.vix,3)), digits = 2),
      "and HV625 =", round(last(runMin(HV625.vix,3)), digits = 2), "are both ABOVE 50% w/in the last 3 days.",
      "\n", "\n")
}
