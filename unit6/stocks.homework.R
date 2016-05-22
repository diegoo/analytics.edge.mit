## 1.1

stocks = read.csv("StocksCluster.csv", header=TRUE)
summary(stocks)

## 1.3

sort(cor(stocks, decreasing=TRUE))

## 2.1

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
sum(diag(table(stocksTrain$PositiveDec, predict(StocksModel, type = "response") > 0.5))) / nrow(stocksTrain)

## 2.2

sum(diag(table(stocksTest$PositiveDec, predict(StocksModel, newdata = stocksTest, type = "response") > 0.5))) / nrow(stocksTest)

## 2.3

table(stocksTest$PositiveDec)
##    0    1 
## 1577 1897 
1897 / nrow(stocksTest)
## [1] 0.5460564

## 3.1

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

## In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value. This is an important point that is sometimes mistakenly overlooked. If you use the outcome value to cluster, you might conclude your method strongly outperforms a non-clustering alternative. However, this is because it is using the outcome to determine the clusters, which is not valid.

## 3.2

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

## 3.3

## From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in January is slightly higher in the training set than in the testing set. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

## 3.4

set.seed(144)
k = 3
km <- kmeans(normTrain, centers = k)

## 3.5

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

## 3.6


## manera oficial:
## stocksTrain1 = subset(stocksTrain, clusterTrain == 1)

stocksTrain1 <- stocksTrain[which(clusterTrain == 1),]
stocksTrain2 <- stocksTrain[which(clusterTrain == 2),]
stocksTrain3 <- stocksTrain[which(clusterTrain == 3),]

stocksTest1 <- stocksTest[which(clusterTest == 1),]
stocksTest2 <- stocksTest[which(clusterTest == 2),]
stocksTest3 <- stocksTest[which(clusterTest == 3),]

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

mean(stocksTest1$PositiveDec)
mean(stocksTest2$PositiveDec)
mean(stocksTest3$PositiveDec)

## 4.2

StocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")
StocksModel1
StocksModel2
StocksModel3

## 4.3

PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")

sum(diag(table(stocksTest1$PositiveDec, PredictTest1 > 0.5))) / nrow(stocksTest1)
sum(diag(table(stocksTest2$PositiveDec, PredictTest2 > 0.5))) / nrow(stocksTest2)
sum(diag(table(stocksTest3$PositiveDec, PredictTest3 > 0.5))) / nrow(stocksTest3)

## 4.4

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
sum(diag(table(AllOutcomes, AllPredictions > 0.5))) / length(AllOutcomes)
