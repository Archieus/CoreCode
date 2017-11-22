library(Quandl)
library(tseries)
library(quantmod)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

# ML.HYOAS - FRED - BAMLH0A0HYM2 # ML.HYEY - FRED - BAMLH0A0HYM2EY # ML.BBBOAS - FRED - BAMLC0A4CBBB
# ML.BBBEY - FRED - BAMLC0A4CBBBEY # ML.HYCCCOAS - FRED - BAMLH0A3HYC # ML.HYCCCEY - FRED - BAMLH0A3HYCEY
# ML.HYBBOAS - FRED - BAMLH0A1HYBB # ML.HYBBEY - FRED - BAMLH0A1HYBBEY # ML.AAOAS - FRED - BAMLC0A2CAA
# ML.AAEY - FRED - BAMLC0A2CAAEY # ML.AAAOAS - FRED - BAMLC0A1CAAA # ML.AAAEY - FRED - BAMLC0A1CAAAEY

# ML.1-3YrEYInvGrade - FRED - BAMLC1A0C13YEY #  ML.1-3YrOAS - FRED - BAMLC1A0C13Y
# ML.3-5YrEYInvGrade FRED - BAMLC2A0C35YEY # ML.3-5YrOAS - FRED - BAMLC2A0C35Y
# ML.5-7YrEYInvGrade - FRED - BAMLC3A0C57YEY #  ML.5-7YrOAS - FRED - BAMLC3A0C57Y
# ML.7-10YrEYInvGrade- FRED - BAMLC4A0C710YEY # ML.7-10YrOAS - FRED - BAMLC4A0C710Y
# ML.10-15YrEYInvGrade FRED - BAMLC7A0C1015YEY # ML.10-15YrOAS FRED - BAMLC7A0C1015Y
# ML.15+YrEYInvGrade - FRED - BAMLC8A0C15PYEY # ML.15+YrOAS - FRED - BAMLC8A0C15PY


FISymbs <- c("BAMLH0A0HYM2", "BAMLH0A0HYM2EY", "BAMLC0A4CBBB", "BAMLC0A4CBBBEY", "BAMLH0A3HYC",
             "BAMLH0A3HYCEY", "BAMLH0A1HYBB", "BAMLH0A1HYBBEY", "BAMLC0A2CAA", "BAMLC0A2CAAEY",
             "BAMLC0A1CAAA", "BAMLC0A1CAAAEY", "BAMLC1A0C13Y", "BAMLC1A0C13YEY", "BAMLC2A0C35Y",
             "BAMLC2A0C35YEY", "BAMLC3A0C57Y", "BAMLC3A0C57YEY", "BAMLC4A0C710Y", "BAMLC4A0C710YEY",
             "BAMLC7A0C1015Y", "BAMLC7A0C1015YEY", "BAMLC8A0C15PY", "BAMLC8A0C15PYEY")

getSymbols(FISymbs, src = "FRED")

ML.OAS <- na.omit(cbind(BAMLH0A0HYM2, BAMLC0A4CBBB, BAMLH0A3HYC, BAMLH0A1HYBB, BAMLC0A2CAA, BAMLC0A1CAAA,
                        BAMLC1A0C13Y, BAMLC2A0C35Y, BAMLC3A0C57Y, BAMLC4A0C710Y, BAMLC7A0C1015Y,
                        BAMLC8A0C15PY))

ML.YLD <- na.omit(cbind(BAMLH0A0HYM2EY,BAMLC0A4CBBBEY,BAMLH0A3HYCEY,BAMLH0A1HYBBEY,BAMLC0A2CAAEY,
                        BAMLC0A1CAAAEY, BAMLC1A0C13YEY, BAMLC2A0C35YEY, BAMLC3A0C57YEY, BAMLC4A0C710YEY,
                        BAMLC7A0C1015YEY, BAMLC8A0C15PYEY))

OAS.MTH <- ML.OAS[endpoints(ML.OAS, "months")]
YLD.MTH <- ML.YLD[endpoints(ML.YLD, "months")]

#TDCol <- max(count.fields("C:/Users/rodney/Dropbox/RCode/Test Files/Fixed Income/yields.txt", sep = '\t'))
TDCol <- max(count.fields("yields.txt", sep = '\t'))

TDYlds <- read.table("yields.txt", header = TRUE, check.names = FALSE, sep = '\t') #sep = "tab delimited"

#SCHCol <- max(count.fields("C:/Users/rodney/Dropbox/RCode/Test Files/Fixed Income/schyields.txt", sep = '\t'))
SCHCol <- max(count.fields("schyields.txt", sep = '\t'))

SCHYlds <- read.table("schyields.txt", header = TRUE, check.names = FALSE, sep = '\t') #sep = "tab delimited"

Yields <- TDYlds[,2:8]
SCHYields <- SCHYlds[,2:11]
row.names(Yields) <- TDYlds[,1]
row.names(SCHYields) <- SCHYlds[,1]

#### Convert Non-Numeric to NA ####
suppressWarnings(Yields <- as.data.frame(lapply(Yields[], function(x) as.numeric(as.character(x)))))
suppressWarnings(SCHYields <- as.data.frame(lapply(SCHYields[], function(x) as.numeric(as.character(x)))))

#Yields[is.na(Yields)] <- 0 ## Turn NAs to 0
row.names(Yields) <- TDYlds[,1]
row.names(SCHYields) <- SCHYlds[,1]

TsyYlds <- Yields[11,]
TsySch <- SCHYields[6,]

Spreads <- matrix(0, nrow = 11, ncol = 7)
SchSprd <- matrix(0, nrow = 15, ncol = 10)

for(i in 1:7) {
  Spreads[,i] <- (Yields[,i] - TsyYlds[,i])
}

for (x in 1:10) {
  SchSprd[,x] <- (SCHYields[,x] - TsySch[,x])
}

row.names(Spreads) <- TDYlds[,1]
row.names(SchSprd) <- SCHYlds[,1]

colnames(Spreads) <- colnames(TDYlds[2:8])
colnames(SchSprd) <- colnames(SCHYlds[2:11])

#### CHECK OAS for Mean Reversion ####
Avg60 <- last(rollapply(OAS.MTH, 60,mean))
Avg60Y <- last(rollapply(YLD.MTH, 60,mean))

ROC12F <- function(x) {ROC(x,12)}

ROC12 <- na.omit(as.data.frame(lapply(OAS.MTH, ROC12F)))
ROC12Y <- na.omit(as.data.frame(lapply(YLD.MTH, ROC12F)))

OAS.ROC <- last(as.xts(ROC12))
YLD.ROC <- last(as.xts(ROC12Y))

#CurVal <- last(OAS.MTH)
CurVal <- tail(OAS.MTH,2)

#CurValY <- last(YLD.MTH)
CurValY <- tail(YLD.MTH,2)

PValue <- as.data.frame(rollapply(as.ts(OAS.MTH), 60, function(u) adf.test(u,k=1)$p.value))
PValueY <- as.data.frame(rollapply(as.ts(YLD.MTH), 60, function(u) adf.test(u,k=1)$p.value))

MLOAS.d <- cbind(OAS.MTH, "Days" = 1:nrow(OAS.MTH))
slope.b <- do.call(merge, lapply(MLOAS.d, function(x) na.omit(rollSFM(log(x), MLOAS.d$Days,
                                                                       60)$beta)))
MLYLD.d <- cbind(YLD.MTH, "Days" = 1:nrow(YLD.MTH))
slope.Y <- do.call(merge, lapply(MLYLD.d, function(x) na.omit(rollSFM(log(x), MLYLD.d$Days,
                                                                      60)$beta)))

Be.ta <- slope.b[,1:ncol(MLOAS.d)-1]
names(Be.ta) <- names(OAS.MTH)

Be.taY <- slope.Y[,1:ncol(MLYLD.d)-1]
names(Be.taY) <- names(YLD.MTH)


# ADF.OAS <- rbind(CurVal,round(Avg60,2), last(PValue) ,round(OAS.ROC,4), round(last(Be.ta),4))
# #row.names(ADF.OAS) <- c("Curr OAS", "Avg5Yr", "PValue", "ROC", "Slope")
# row.names(ADF.OAS) <- c("Prev.Mo.OAS", "Curr.Mo.OAS", "Avg5Yr", "PValue", "ROC", "Slope")
# colnames(ADF.OAS) <- c("HY.OAS", "BBB.OAS", "CCC.OAS", "HY.BB.OAS", "AA.OAS", "AAA.OAS", "1.3.OAS",
#                        "3.5.OAS", "5.7.OAS", "7.10.OAS", "10.15.OAS", "15+.OAS")

ADF.OAS1 <- rbind(CurVal[,1:6],round(Avg60[,1:6],2), last(PValue[,1:6]) ,round(OAS.ROC[,1:6],4),
                  round(last(Be.ta[,1:6]),4))
row.names(ADF.OAS1) <- c("Prev.Mo.OAS", "Curr.Mo.OAS", "Avg5Yr", "PValue", "ROC", "Slope")
colnames(ADF.OAS1) <- c("HY.OAS", "BBB.OAS", "CCC.OAS", "HY.BB.OAS", "AA.OAS", "AAA.OAS")

ADF.OAS2 <- rbind(CurVal[,7:12],round(Avg60[,7:12],2), last(PValue[,7:12]) ,round(OAS.ROC[,7:12],4), round(last(Be.ta[,7:12]),4))
row.names(ADF.OAS2) <- c("Prev.Mo.OAS", "Curr.Mo.OAS", "Avg5Yr", "PValue", "ROC", "Slope")
colnames(ADF.OAS2) <- c("1.3.OAS", "3.5.OAS", "5.7.OAS", "7.10.OAS", "10.15.OAS", "15+.OAS")

# ADF.YLD <- rbind(CurValY,round(Avg60Y,2), last(PValueY) ,round(YLD.ROC,4), round(last(Be.taY),4))
# row.names(ADF.YLD) <- c("Prev.Mo.YLD", "Curr.Mo.YLD", "Avg5Yr", "PValue", "ROC", "Slope")
# colnames(ADF.YLD) <- c("HY.YLD", "BBB.YLD", "CCC.YLD", "HY.BB.YLD", "AA.YLD", "AAA.YLD", "1.3.YLD",
#                        "3.5.YLD", "5.7.YLD", "7.10.OAS", "10.15.YLD", "15+.YLD")

ADF.YLD1 <- rbind(CurValY[,1:6],round(Avg60Y[,1:6],2), last(PValueY[,1:6]) ,round(YLD.ROC[,1:6],4),
                  round(last(Be.taY[,1:6]),4))
row.names(ADF.YLD1) <- c("Prev.Mo.YLD", "Curr.Mo.YLD", "Avg5Yr", "PValue", "ROC", "Slope")
colnames(ADF.YLD1) <- c("HY.YLD", "BBB.YLD", "CCC.YLD", "HY.BB.YLD", "AA.YLD", "AAA.YLD")

ADF.YLD2 <- rbind(CurValY[,7:12],round(Avg60Y[,7:12],2), last(PValueY[,7:12]) ,round(YLD.ROC[,7:12],4),
                  round(last(Be.taY[,7:12]),4))
row.names(ADF.YLD2) <- c("Prev.Mo.YLD", "Curr.Mo.YLD", "Avg5Yr", "PValue", "ROC", "Slope")
colnames(ADF.YLD2) <- c("1.3.YLD","3.5.YLD", "5.7.YLD", "7.10.YLD", "10.15.YLD", "15+.YLD")

summary(OAS.MTH)
summary(YLD.MTH)

print(Yields)
print(Spreads)

print(SCHYields)
print(SchSprd)

print(ADF.OAS1)
print(ADF.YLD1)

print(ADF.OAS2)
print(ADF.YLD2)


