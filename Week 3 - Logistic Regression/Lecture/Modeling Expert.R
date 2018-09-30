setwd("C:\\R\\Analytics Edge")
getwd()
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

baseline = 98/131

install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type ="response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type ="response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)


table(qualityTrain$PoorCare, predictTrain > 0.5)
sensitivity = 10/25
sensitivity
specificity = 70/74
specificity

table(qualityTrain$PoorCare, predictTrain > 0.7)
sensitivity = 8/25
sensitivity
specificity = 73/74
specificity

table(qualityTrain$PoorCare, predictTrain > 0.2)
sensitivity = 16/25
sensitivity
specificity = 54/74
specificity

install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


predictTest = predict(QualityLog, type ="response", newdata=qualityTest)
summary(predictTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
