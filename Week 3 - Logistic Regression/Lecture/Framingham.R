setwd("C:\\R\\Analytics Edge")
getwd()
framingham = read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
framinghamLog = glm(TenYearCHD ~ . , data = train, family = binomial)
summary(framinghamLog)
predictTest = predict(framinghamLog, type ="response", newdata=test)
table(test$TenYearCHD, predictTest > 0.5)
model= (1069+11)/(1069+6+187+11)
model
baseline = (1069+6)/(1069+6+187+11)
baseline


library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
AUC=as.numeric(performance(ROCRpred, "auc")@y.values)
AUC

sensitivity = 11/198
sensitivity
specificity = 1069/1075
specificity



tapply(predictTrain, qualityTrain$PoorCare, mean)
tapply(predictTrain, qualityTrain$PoorCare, mean)

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


predictTest = predict(QualityLog, type ="response", newdata=qualityTest)
summary(predictTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
