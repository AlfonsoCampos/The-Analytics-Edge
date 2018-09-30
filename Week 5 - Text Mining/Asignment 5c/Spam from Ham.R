Sys.setlocale("LC_ALL", "C")
setwd("C:\\R\\Analytics Edge")
getwd()

emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
summary(emails$spam==1)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(emails$text))
corpus
corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
corpus[[2]]

min(nchar(emails$text))
nchar(emails$text)==13

corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse ))
str(emailsSparse)

colnames(emailsSparse[which.max(colSums(emailsSparse))])
emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, emailsSparse$spam == FALSE)))
sort(colSums(subset(emailsSparse, emailsSparse$spam == TRUE)))


emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

#Model
library(rpart)
library(rpart.plot)
library(randomForest)
spamLog = glm(spam ~ . , data = train, family = binomial)
summary(spamLog)
predictions = predict(spamLog, type="response")
sum(predictions < 0.00001)
sum(predictions > 0.99999)
sum(predictions > 0.00001 & predictions < 0.99999 )
summary(spamLog)
t = table(train$spam, predictions > 0.5)
(t[1,1]+t[2,2])/nrow(train)

library(ROCR)
ROCRpred <- prediction(predictions, train$spam)
auclog <- as.numeric(performance(ROCRpred, "auc")@y.values)
auclog


spamCART = rpart(spam ~., data = train, method ="class")
prp(spamCART)
predictCART = predict(spamCART,  type="class")
t2 = table(train$spam, predictCART)
(t2[1,1]+t2[2,2])/nrow(train)

predictCART = predict(spamCART)
ROCRpred <- prediction(predictCART[,2], train$spam)
auclog <- as.numeric(performance(ROCRpred, "auc")@y.values)
auclog

set.seed(123)
spamRF = randomForest(spam ~., data = train)
predictRF = predict(spamRF, type = 'class')
t3 = table(train$spam, predictRF)
(t3[1,1]+t3[2,2])/nrow(train)

predictRF = predict(spamRF, type = 'prob')
ROCRpred <- prediction(predictRF[,2], train$spam)
auclog <- as.numeric(performance(ROCRpred, "auc")@y.values)
auclog

#Test
t4 = table(predict(spamLog, newdata = test) > 0.5, test$spam)
(t4[1,1]+t4[2,2])/nrow(test)

predicted <- predict(spamLog, type="response", newdata=test)
ROCRpredTest = prediction(predicted, test$spam)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

predicted <- predict(spamCART, newdata=test, type = "class")
sum(predicted == test$spam) / length(test$spam)

predicted <- predict(spamCART, newdata = test)
r <- prediction(predicted[,2], test$spam)
auc <- as.numeric(performance(r, "auc")@y.values)
auc

predicted <- predict(spamRF, newdata =test)
sum(predicted == test$spam)/length(test$spam)

predicted <- predict(spamRF, newdata=test, type = 'prob')
r <- prediction(predicted[,2], test$spam)
auc <- as.numeric(performance(r, "auc")@y.values)
auc
