Sys.setlocale("LC_ALL", "C")

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
##str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
##table(tweets$Negative)

library(tm)
library(SnowballC)
 
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)

##stopwords("english")[1:10]

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
##frequencies
##inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995)
##sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
##prp(tweetCART)
predictCART = predict(tweetCART, newdata=testSparse, type="class")
##table(testSparse$Negative, predictCART)
##(294+18)/(294+6+37+18)
##table(testSparse$Negative)
##300/(300+55)

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
##table(testSparse$Negative, predictRF)
##(293+21)/(293+7+34+21)

tweetLog <- glm(Negative ~ ., data = trainSparse, family = "binomial")
predictions.lr = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions.lr > 0.5)
##       FALSE TRUE
## FALSE   257   43
## TRUE     21   34

## issue de logistic regression para este problema:
## The model fits the training set really well, but does not perform well on the test set. A logistic regression model with a large number of variables is particularly at risk for overfitting.