emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

## 1.5

max(nchar(emails$text))

## 1.6

which.min(nchar(emails$text))

## 2.1

library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
corpus <- tm_map(corpus,stemDocument)
dtm <- DocumentTermMatrix(corpus)

## 2.2

spdtm <- removeSparseTerms(dtm, 0.95)

## 2.3

## data frame contains the number of times each word stem (columns) appeared in each email (rows)
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
rownames(emailsSparse) <- c(1:5728)
colSums(emailsSparse)

sort(colSums(emailsSparse))
which.max(colSums(emailsSparse))
    
## 2.4

emailsSparse$spam <- emails$spam

ham <- subset(emailsSparse, spam == 0)
table(colSums(ham[,c(1:330)]) > 5000)

# o bien:
sort(colSums(ham), decreasing=TRUE)[1:10]

## 2.5

spam <- subset(emailsSparse, spam == 1)
table(colSums(spam[,c(1:330)]) > 1000)

## 3.1

emailsSparse$spam <- as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

spamLog <- glm(spam ~ ., data = train, family = "binomial")
spamLog.predictions <- predict(spamLog, type = "response")
table(spamLog.predictions < 0.00001)
table(spamLog.predictions > 0.99999)
table(spamLog.predictions > 0.99999)

sum(which(spamLog.predictions < 0.99999 & spamLog.predictions > 0.00001))

## 3.3

library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = train, method = "class")
spamCART.predictions <- predict(spamCART)[,2]
table(train$spam, spamCART.predictions)

## 3.4

table(train$spam, spamLog.predictions > 0.5)

## 3.5

library(ROCR)
spamLog.roc = prediction(spamLog.predictions, train$spam)
perf = performance(spamLog.roc, "tpr", "fpr")
as.numeric(performance(spamLog.roc, "auc")@y.values)

## 3.6

sum(diag(table(train$spam, spamCART.predictions > 0.5))) / nrow(train)
## 0.942394

## 3.7
    
library(ROCR)
spamCART.roc = prediction(spamCART.predictions, train$spam)
perf = performance(spamCART.roc, "tpr", "fpr")
as.numeric(performance(spamCART.roc, "auc")@y.values)
## 0.9696044

## 3.8

library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)
spamRF.predictions <- predict(spamRF, type="prob")[,2]
table(train$spam, spamRF.predictions)
spamRF.roc = prediction(spamRF.predictions, train$spam)
perf = performance(spamRF.roc, "tpr", "fpr")
as.numeric(performance(spamRF.roc, "auc")@y.values)

## 4.1

spamLog.predictions.test <- predict(spamLog, newdata = test, type = "response")
sum(diag(table(test$spam, spamLog.predictions.test > 0.5))) / nrow(test)
## 0.9505239

## 4.2

spamLog.roc.test = prediction(spamLog.predictions.test, test$spam)
perf = performance(spamLog.roc.test, "tpr", "fpr")
as.numeric(performance(spamLog.roc.test, "auc")@y.values)
## 0.9627517

## 4.3

spamCART.predictions.test <- predict(spamCART, newdata = test)[,2]
sum(diag(table(test$spam, spamCART.predictions.test > 0.5))) / nrow(test)
## 0.9394645

## 4.4

spamCART.roc.test = prediction(spamCART.predictions.test, test$spam)
perf = performance(spamCART.roc.test, "tpr", "fpr")
as.numeric(performance(spamCART.roc.test, "auc")@y.values)
## 0.963176

## 4.5

spamRF.predictions.test <- predict(spamRF, newdata = test, type="prob")[,2]
sum(diag(table(test$spam, spamRF.predictions.test > 0.5))) / nrow(test)
## 0.9975656

## 4.6

spamRF.roc.test = prediction(spamRF.predictions.test, test$spam)
perf = performance(spamRF.roc.test, "tpr", "fpr")
as.numeric(performance(spamRF.roc.test, "auc")@y.values)
## 0.9975656

## 4.8

## Both CART and random forest had very similar accuracies on the training and testing sets. However, logistic regression obtained nearly perfect accuracy and AUC on the training set and had far-from-perfect performance on the testing set. This is an indicator of overfitting.
