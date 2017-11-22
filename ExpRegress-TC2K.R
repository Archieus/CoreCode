library(quantmod)
library(PerformanceAnalytics)
Sys.setenv(TZ = "EST5EDT")

#### Close data from TC2000 Convert TXT to XTS ####
ERtxt <- read.table('etftrPx', header = FALSE, sep = ",")

ERzoo <- read.zoo(ERtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ERxts <- as.xts(ERzoo)

stkreg <- na.locf(ERxts, na.rm = FALSE, fromLast = TRUE)
stkreg.d <- cbind(ERxts, "Days" = 1:nrow(ERxts))

reg.r2 <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x),
                                                                      stkreg.d$Days, 90)$r.squared)))
r.sqrd <- (reg.r2[,1:ncol(stkreg.d)-1])
names(r.sqrd) <- colnames(ERxts)

slope.b <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,
                                                                       90)$beta)))
Be.ta <- slope.b[,1:ncol(stkreg.d)-1]
names(Be.ta) <- colnames(ERxts)

Ann.sl <- ((exp(Be.ta)^250)-1)*100
names(Ann.sl) <- colnames(ERxts)
Adj.sl <- round((r.sqrd * Ann.sl),4)
names(Adj.sl) <- colnames(ERxts)

####CALCULATE CVaR for each Symbol####

stkret <- na.omit(Return.calculate(stkreg, method = "discrete"))
stk.CVAR <- t(as.data.frame(sapply(stkret, function(x) CVaR(x, p =0.99, method = "modified"))))
Dlr.rsk <- stk.CVAR*last(stkreg)

PosDlrs <- abs(((100000*.001)/Dlr.rsk)*last(stkreg))
stkWt <- PosDlrs/sum(PosDlrs)

EMA.100 <- lapply(stkreg, function(x) round(last(EMA(x, n = 100)),2))
EMA.df <- as.data.frame(EMA.100)
names(EMA.df) <- colnames(ERxts)

Action <- ifelse(as.data.frame(last(stkreg) > EMA.df), "Buy", "Hold")

PDlrs <- matrix(0, nrow = 1, ncol = ncol(ERxts))

for (i in 1:ncol(ERxts)) {
  if (Action[i] != "Buy"){
    PDlrs[i] <- 0
  } else {
    PDlrs[i] <- abs(((100000*.001)/Dlr.rsk[,i])*last(stkreg[,i]))
  }
}

PosWt <- PDlrs/sum(PDlrs)

#EWH.reg <- rollSFM(EWH[,4], .index(EWH), 90)
#EWH.rma <- EWH.reg$alpha + EWH.reg$beta*.index(EWH)
#chart_Series(EWH, TA = "add_TA(EWH.rma, on = 1)")

Results <- rbind(round(last(Ann.sl), 4), round(last(r.sqrd),4), round(last(Adj.sl),4), round(last(stkreg),2),
                 EMA.df, round(PosWt,2))

row.names(Results) <- c('Ann Slope', 'R-Sqrd', "Adj Slope", "Price", "100 EMA", "PortWt")
Rpt <- t(as.data.frame(Results))

Results2 <- rbind(round(last(Ann.sl), 4), round(last(r.sqrd),4), round(last(Adj.sl),4), round(last(stkreg),2),
                  EMA.df, round(stkWt,2))

row.names(Results2) <- c('Ann Slope', 'R-Sqrd', "Adj Slope", "Price", "100 EMA", "PortWt")
Rpt2 <- t(as.data.frame(Results2))

Results3 <- rbind(round(last(Ann.sl), 4), round(last(r.sqrd),4), round(last(Adj.sl),4), round(last(stkreg),2),
                  EMA.df, round(PosDlrs,2))

row.names(Results3) <- c('Ann Slope', 'R-Sqrd', "Adj Slope", "Price", "100 EMA", "PortDlrs")
Rpt3 <- t(as.data.frame(Results3))

write.csv(Rpt,file = "PortWts-EMA.csv", row.names = TRUE)
write.csv(Rpt2,file = "PortWts-ex-EMA.csv", row.names = TRUE)
write.csv(Rpt3, file = "PortDlrs.csv", row.names = TRUE)

