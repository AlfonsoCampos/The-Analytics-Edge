# Read in the data
setwd("C:\\R\\Analytics Edge")
getwd()
Census = read.csv("census.csv")
str(Census)

# Split the data
library(caTools)
set.seed(2000)

spl = sample.split(Census$over50k, SplitRatio = 0.6)
CensusTrain = subset(Census, spl==TRUE)
CensusTest = subset(Census, spl==FALSE)

CensusLog = glm(over50k ~ ., data = CensusTrain, family = binomial)
summary(CensusLog)

predictTest = predict(CensusLog, type ="response", newdata=CensusTest)
table(CensusTest$over50k, predictTest > 0.5)
(9051+1888)/(9051+662+1190+1888)

table(CensusTest$over50k)[1]/nrow(CensusTest)

library(ROCR)
ROCRpred = prediction(predictTest, CensusTest$over50k)
AUC=as.numeric(performance(ROCRpred, "auc")@y.values)
AUC

library(rpart)
library(rpart.plot)

# CART model
StevensTree = rpart(over50k ~ ., data = CensusTrain, method="class")
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = CensusTest, type = "class")
table(CensusTest$over50k, PredictCART)
(9243+1596)/nrow(CensusTest)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = CensusTest)
pred = prediction(PredictROC[,2], CensusTest$over50k)
perf = performance(pred, "tpr", "fpr")  #tpr: true positive rate. 

# Compute the AUC
as.numeric(performance(pred, "auc")@y.values)


set.seed(1)
trainSmall = CensusTrain[sample(nrow(CensusTrain), 2000), ]


library(randomForest)
# Build random forest model
StevensForest = randomForest(over50k ~ ., data = trainSmall)

# Make predictions
PredictForest = predict(StevensForest, newdata = CensusTest)
t=table(CensusTest$over50k, PredictForest)
(t[1,1]+t[2,2])/nrow(CensusTest)


vu = varUsed(StevensForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(StevensForest$forest$xlevels[vusorted$ix]))


varImpPlot(StevensForest)


library(caret)
library(e1071)
set.seed(2)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
tr <- train(over50k ~ ., data = CensusTrain, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
tr
# Build CART model with cp=0.02
CART4 = rpart(over50k ~ ., data=CensusTrain, cp=0.002)
prp(CART4)
# Accuracy
pred6 <- predict(CART4, newdata = CensusTest, type="class")
t4 <- table(CensusTest$over50k, pred6)
(t4[1,1] + t4[2,2])/nrow(CensusTest)

# Use only Area as the predictor
set.seed(111)
tr1 <- train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
CART5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.01)
prp(CART5)
# SSE
pred7 <- predict(CART5)
sum((pred7 - statedata$Life.Exp)^2)