library(quantmod)
library(PerformanceAnalytics)
Sys.setenv(TZ = "UTC")
suppressWarnings(getSymbols('^GSPC', src = "yahoo", from = Sys.Date()-50, end = Sys.Date()))
suppressWarnings(getSymbols('^IXIC', src = "yahoo", from = Sys.Date()-50, end = Sys.Date()))
suppressWarnings(getSymbols('^NYA', src = "yahoo", from = Sys.Date()-50, end = Sys.Date()))

names(GSPC) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
names(IXIC) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
names(NYA) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')

SP5pvl <- Lag(GSPC$Volume,1)
NAZpvl <- Lag(IXIC$Volume,1)
NYSEpvl <- Lag(NYA$Volume,1)

SPret <- Return.calculate(GSPC)
NAZret <- Return.calculate(IXIC)
NYAret <- Return.calculate(NYA)

SP500 <- na.omit(cbind(GSPC[,1:5], SP5pvl, SPret$Close))
names(SP500) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Priorvl', 'Return')
NAZ <- na.omit(cbind(IXIC[,1:5], NAZpvl, NAZret$Close))
names(NAZ) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Priorvl', 'Return')
NYSE <- na.omit(cbind(NYA[,1:5], NYSEpvl, NYAret$Close))
names(NYSE) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Priorvl', 'Return')


SP5Dist <- matrix(0, nrow = nrow(SP500), ncol = 1)
for(i in 1:nrow(SP500)) {
  if((SP500$Return[i] <= -.0020) & (SP500$Volume[i] > SP500$Priorvl[i])) {
    SP5Dist[i] <- 'Distribution'
  }else{
    if((SP500$Return >= 0.017) & SP500$Volume[i] > SP500$Priorvl[i]){
      SP5Dist[i] <- "FollowThru"
    }else{
      SP5Dist[i] <- "None"
    }
  }
}

NAZDist <- matrix(0, nrow = nrow(NAZ), ncol = 1)
for(y in 1:nrow(NAZ)) {
  if((NAZ$Return[y] <= -.0020) & (NAZ$Volume[y] > NAZ$Priorvl[y])) {
    NAZDist[y] <- 'Distribution'
  }else{
    if((NAZ$Return[y] >= 0.017) & NAZ$Volume[y] > NAZ$Priorvl[y]){
      NAZDist[y] <- "FollowThru"
    }else{
      NAZDist[y] <- "None"
    }
  }
}

NYSEDist <- matrix(0, nrow = nrow(NYSE), ncol = 1)
for(x in 1:nrow(NYSE)) {
  if((NYSE$Return[x] <= -.0020) & (NYSE$Volume[x] > NYSE$Priorvl[x])) {
    NYSEDist[x] <- 'Distribution'
  }else{
    if((NYSE$Return[x] >= 0.017) & NYSE$Volume[x] > NYSE$Priorvl[x]){
      NYSEDist[x] <- "FollowThru"
    }else{
      NYSEDist[x] <- "None"
    }
  }
}

SP5Dist.df <- data.frame(SP5Dist)
rownames(SP5Dist.df) <- index(SP500)
SP500.df <- data.frame(SP500)
SP500.IBD <- cbind(SP500.df[,1:5], SP5Dist.df)

NAZDist.df <- data.frame(NAZDist)
rownames(NAZDist.df) <- index(NAZ)
NAZ.df <- data.frame(NAZ)
NAZ.IBD <- cbind(NAZ.df[,1:5], NAZDist.df)

NYSEDist.df <- data.frame(NYSEDist)
rownames(NYSEDist.df) <- index(NYSE)
NYSE.df <- data.frame(NYSE)
NYSE.IBD <- cbind(NYSE.df[,1:5], NYSEDist.df)

Combo <- cbind(tail(SP5Dist.df,33), tail(NAZDist.df,33), tail(NYSEDist.df,33))

View(tail(SP500.IBD, 25))
View(tail(NAZ.IBD, 25))
View(tail(NYSE.IBD, 25))

#write.csv(SP500.IBD, file = "SP500-IBD.csv", row.names = TRUE)
#write.csv(NAZ.IBD, file = "NAZ-IBD.csv", row.names = TRUE)
#write.csv(NYSE.IBD, file = "NYSE-IBD.csv", row.names = TRUE)

write.csv(Combo, file = "IBD-FT-Dist.csv", row.names = TRUE)


