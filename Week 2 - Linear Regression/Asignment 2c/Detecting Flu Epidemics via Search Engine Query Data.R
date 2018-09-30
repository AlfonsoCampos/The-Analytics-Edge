FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)
subset(FluTrain, ILI == max(ILI))
subset(FluTrain, Queries == max(Queries))

hist(FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
cor = cor(log(FluTrain$ILI), FluTrain$Queries)
cor^2


FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
summary(PredTest1)
Estimated_ILI = PredTest1[FluTest$Week == "2012-03-11 - 2012-03-17"]
Observed_ILI = subset(FluTest, Week == "2012-03-11 - 2012-03-17", ILI)
(Observed_ILI - Estimated_ILI)/Observed_ILI

SSE = sum((PredTest1 - FluTest$ILI)^2)
SSE

RMSE = sqrt(SSE/nrow(FluTest))
RMSE

install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
plot(log(ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
summary(FluTrend1)


ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(ILILag2)

str(FluTrain)
str(FluTest)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
summary(FluTest)

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2
