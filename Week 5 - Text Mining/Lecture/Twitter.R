Sys.setlocale("LC_ALL", "C")

setwd("C:\\R\\Analytics Edge")
getwd()
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#Preprocess
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

Sys.getlocale()

#Word Frequency
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515]) #Sparse data = many 0s

findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

findFreqTerms(frequencies, lowfreq=100)

#Model
library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~., data = trainSparse, metho ="class")
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type ="class")
t = table(testSparse$Negative, predictCART)
(t[1,1]+t[2,2])/nrow(testSparse)

t2 = table(testSparse$Negative)
t2[1]/nrow(testSparse)

#Forest
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~., data = trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
t3 = table(testSparse$Negative, predictRF)
(t3[1,1]+t3[2,2])/nrow(testSparse)

#Logistic Regression
tweetLog = glm(Negative ~ . , data = trainSparse, family = binomial)
predictions = predict(tweetLog, newdata=testSparse, type="response")
t4 = table(testSparse$Negative, predictions > 0.5)
(t4[1,1]+t4[2,2])/nrow(testSparse)
