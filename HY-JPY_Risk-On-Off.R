library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
Sys.setenv(TZ = "UTC")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
suppressWarnings(getSymbols('BAMLHYH0A0HYM2TRIV', src  = 'FRED', return.xlass = 'xts', from = '1996-12-31'))
suppressWarnings(getSymbols('EXJPUS', src  = 'FRED', return.xlass = 'xts', from = '1996-12-31'))
suppressWarnings(getSymbols('^GSPC', src = 'yahoo', from = '1996-12-31'))

HY.JPY <- na.locf(BAMLHYH0A0HYM2TRIV/EXJPUS, na.rm = FALSE)
HYJPY.ROC <- ROC(HY.JPY, 10)

HYld.ret <- dailyReturn(na.locf(BAMLHYH0A0HYM2TRIV), na.rm = FALSE)
JPY.ret <- dailyReturn(na.locf(EXJPUS), na.rm = FALSE)
SP.ret <- dailyReturn(na.locf(GSPC), na.rm = FALSE)

chart.RollingCorrelation(HYld.ret,SP.ret, width = 252, colorset = 2,
                         main = "Rolling 252-Day HY/S&P Correlation")

layout(1:2)
plot(GSPC[,6]['2000:/'], main = 'S&P 500')
chart.RollingCorrelation(HYld.ret['1999:/'],SP.ret['1999:/'], width = 252, colorset = 2,
                         main = "Rolling 252-Day HY/S&P Correlation")

layout(1:3)
plot(GSPC[,6]['1991:/'], main = 'S&P 500')
plot(HY.JPY['1991:/'], main = 'HY/Yen Spread')
plot(HYJPY.ROC['1991:/'], type = 'h', main = 'ROC(10) HY/Yen', sub = 'Risk-On >0')



