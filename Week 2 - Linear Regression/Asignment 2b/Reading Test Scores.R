pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)
summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore= lm(readingScore ~., data = pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
SSE

RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

predTest=predict(lmScore, newdata=pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SSE

RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - bias)^2)

SST = sum((mean(pisaTrain$readingScore) -pisaTest$readingScore)^2)
SST
1 - SSE/SST
