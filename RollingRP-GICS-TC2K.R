library(quantmod)
library(PerformanceAnalytics)
library(fPortfolio)
library(fMultivar)

Sys.setenv(TZ = "EST5EDT")

#### reads/loads CSV into R ####
Datalist <- read.table('MomPlus', sep = ",")
DLzoo <- read.zoo(Datalist, sep = ",", format = "%m/%d/%Y", split = 3)
DLxts <- as.xts(DLzoo)

SP500.adj <- DLxts[,26]
SP500.ret <- na.omit(Return.calculate(SP500.adj, method = 'discrete'))

# par(xpd = FALSE)
# plot(SP500.adj['2017:/'])
# vert_line <- which(index(SP500.adj) == as.Date('2017-08-25'))
# vert_line2 <- which(index(SP500.adj) == as.Date('2017-09-25'))
# abline(v = .index(SP500.adj)[vert_line], col = 'red')
# abline(v = .index(SP500.adj)[vert_line2], col = 'blue')

# #### Plot with Vertical Lines ####
# xt <- xts(rep(FALSE, nrow(SP500.adj)), index(SP500.adj))
# Idea.Date <- c('2017-08-25')
# xt[Idea.Date,] <- TRUE
# 
# xt2 <- xts(rep(FALSE, nrow(SP500.adj)), index(SP500.adj))
# Buy.Date <- c('2017-09-05')
# xt2[Buy.Date,] <- TRUE
# 
# chart_Series(SP500.adj['2017-07:/'])
# add_TA(xt, on =-1, col = "orange", border = "blue")
# add_TA(xt2, on = 1, col = "darkgreen", border = "darkgreen")


Daily.adj <- na.omit(cbind(DLxts[,14], DLxts[,31:39]))
Daily.ret <- na.omit(Return.calculate(Daily.adj, method = c('discrete'))) # Calculate Daily Returns

#colors(distinct = TRUE) generates a list of colors available for graphing#
pdf(file = "C:/Users/rodney/Dropbox/RCode/1. TC2K Format/MomPlus-TC2K/RP.pdf", width = 12,
    height = 8.5, paper = 'USr')
chart.RelativePerformance(Daily.ret$XLF['2017'],Daily.ret[,c(1:3,5:10)]['2017'],colorset = c(rich8equal,'olivedrab'), legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLY['2017'],Daily.ret[,1:9]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLI['2017'],Daily.ret[,c(1:4,6:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLB['2017'],Daily.ret[,c(1,3:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLE['2017'],Daily.ret[,c(1:2,4:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLV['2017'],Daily.ret[,c(1:8,10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLP['2017'],Daily.ret[,c(1:6,8:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLU['2017'],Daily.ret[,c(1:7,9:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$IYZ['2017'],Daily.ret[,2:10]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(Daily.ret$XLK['2017'],Daily.ret[,c(1:5,7:10)]['2017'],colorset = c(rich8equal,'olivedrab'),legend.loc = 'left')
chart.RelativePerformance(SP500.ret['2017'],Daily.ret[,1:10]['2017'],colorset = c(rich8equal,'olivedrab', 'cyan4'),legend.loc = 'left')
dev.off()
