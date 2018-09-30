# Load mvt data
setwd("C:\\R\\Analytics Edge")
getwd()

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
str(IBM)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#Summary statistics
str(IBM)
str(GE)
str(ProcterGamble)
str(CocaCola)
str(Boeing)

summary(IBM)
summary(GE)
summary(ProcterGamble)
summary(CocaCola)
summary(Boeing)

sd(ProcterGamble$StockPrice)

# Visualization
plot(CocaCola$Date,CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)


# Trends
tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE) #Average IBM StockPrice sorted by month
summary(IBM)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean, na.rm=TRUE) #Average CocaCola StockPrice sorted by month
summary(CocaCola)
tapply(GE$StockPrice, months(GE$Date), mean, na.rm=TRUE) #Average GE StockPrice sorted by month
summary(GE)