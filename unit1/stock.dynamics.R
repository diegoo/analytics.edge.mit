Sys.setlocale("LC_ALL", "C")

IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=1)
abline(v=as.Date(c("1983-06-01")), lwd=1)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="black", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="orange", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=1)

abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-10-01")), lwd=1)

abline(v=as.Date(c("2004-01-01")), lwd=1)
abline(v=as.Date(c("2005-12-31")), lwd=1)

sort(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
mean(CocaCola$StockPrice)

sort(tapply(IBM$StockPrice, months(IBM$Date), mean))
mean(IBM$StockPrice)

sort(tapply(GE$StockPrice, months(GE$Date), mean))
mean(GE$StockPrice)

sort(tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean))
mean(ProcterGamble$StockPrice)

sort(tapply(Boeing$StockPrice, months(Boeing$Date), mean))
mean(Boeing$StockPrice)

