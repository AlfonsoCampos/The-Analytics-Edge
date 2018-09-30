Sys.setlocale("LC_ALL", "C")
setwd("C:\\R\\Analytics Edge")
getwd()

stocks = read.csv("StocksCluster.csv")
str(stocks)
table(stocks$PositiveDec)[2]/nrow(stocks)
sort(cor(stocks))
summary(stocks)

##
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ . , data = stocksTrain, family = binomial)
summary(StocksModel)

predictTest = predict(StocksModel, type ="response")
t=table(stocksTrain$PositiveDec, predictTest > 0.5)
(t[1,1]+t[2,2])/nrow(stocksTrain)

predictTest = predict(StocksModel, type ="response", newdata=stocksTest)
t=table(stocksTest$PositiveDec, predictTest > 0.5 )
(t[1,1]+t[2,2])/nrow(stocksTest)
(t[2,1]+t[2,2])/nrow(stocksTest)

##
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain)
summary(normTest)

set.seed(144)
km = kmeans(normTrain, 3)
table(km$cluster)

install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

##
summary(stocksTrain)
stockTrain1 <- subset(stocksTrain, clusterTrain == 1)
stockTrain2 <- subset(stocksTrain, clusterTrain == 2)
stockTrain3 <- subset(stocksTrain, clusterTrain == 3)

stockTest1 <- subset(stocksTest, clusterTest == 1)
stockTest2 <- subset(stocksTest, clusterTest == 2)
stockTest3 <- subset(stocksTest, clusterTest == 3)

tapply(stocksTrain$PositiveDec, clusterTrain, mean)

stocksModel1 <- glm(PositiveDec ~., data = stockTrain1, family = binomial)
stocksModel2 <- glm(PositiveDec ~., data = stockTrain2, family = binomial)
stocksModel3 <- glm(PositiveDec ~., data = stockTrain3, family = binomial)

predictTest1 <- predict(stocksModel1, newdata = stockTest1, type = "response")
predictTest2 <- predict(stocksModel2, newdata = stockTest2, type = "response")
predictTest3 <- predict(stocksModel3, newdata = stockTest3, type = "response")

t=table(stockTest1$PositiveDec, predictTest1 >= 0.5)
(t[1,1]+t[2,2])/nrow(stockTest1)

t=table(stockTest2$PositiveDec, predictTest2 >= 0.5)
(t[1,1]+t[2,2])/nrow(stockTest2)

t=table(stockTest3$PositiveDec, predictTest3 >= 0.5)
(t[1,1]+t[2,2])/nrow(stockTest3)


allPredictions = c(predictTest1, predictTest2, predictTest3)
allOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)
t=table(allOutcomes, allPredictions>= 0.5)
(t[1,1]+t[2,2])/(nrow(stockTest1)+nrow(stockTest2)+nrow(stockTest3))


