trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)

## 1.1

summary(nchar(trials$abstract))

## 1.2

table(nchar(trials$abstract) == 0)
sum(nchar(trials$abstract) == 0)
dim(subset(trials, nchar(abstract) == 0))

## 1.3

trials[which.min(nchar(trials$title))]

## 2.1

library(tm)
library(SnowballC)

corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords,stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords,stopwords("english"))

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

## Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
sparseTitle <- removeSparseTerms(dtmTitle, 0.95)
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95)

## 2.2

## Because titles are so short, a word needs to be very common to appear in 5% of titles. Because abstracts have many more words, a word can be much less common and still appear in 5% of abstracts.

## 2.3

wordsTitle <- as.data.frame(as.matrix(sparseTitle))
colnames(wordsTitle) = make.names(colnames(wordsTitle))
colSums(wordsTitle)

wordsAbstract <- as.data.frame(as.matrix(sparseAbstract))
colnames(wordsAbstract) = make.names(colnames(wordsAbstract))
colSums(wordsAbstract)

which.max(colSums(wordsAbstract))

## 3.1

rownames(wordsTitle) <- c(1:1860)
rownames(wordsAbstract) <- c(1:1860)

colnames(wordsTitle) = paste0("T", colnames(wordsTitle))
colnames(wordsAbstract) = paste0("A", colnames(wordsAbstract))

## 3.2

dtm <- cbind(wordsTitle, wordsAbstract)
dtm$trial <- trials$trial

## 3.3

library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

table(train$trial)
##   0   1 
## 730 572 
730 / nrow(train)
## [1] 0.5606759

## 3.4

library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data=train, method="class")
## prp(trialCART)

## 3.5 / 3.6

summary(predict(trialCART)[2])
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.8636  0.8636  0.8636  0.8636  0.8636  0.8636

summary(predict(trialCART, newdata=test)[2])

## Because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes compared to data points, we expect exactly the same maximum predicted probability.

## 3.7

trialCART.predictions.training <- predict(trialCART, type="class")
table(train$trial, trialCART.predictions.training)
sum(diag(table(train$trial, trialCART.predictions.training))) / nrow(train)
## [1] 0.8233487

table(train$trial, trialCART.predictions.training)
##  trialCART.predictions.training
##     0   1
## 0 631  99
## 1 131 441

## sensitivity = TP / TP + FN
441 / (441 + 131)
## specificity = TN / TN + FP
631 / (631 + 99)

## 4.1

trialCART.predictions <- predict(trialCART, newdata=test, type="class")
table(test$trial, trialCART.predictions)
## trialCART.predictions
##       0   1
##   0 261  52
##   1  83 162
sum(diag(table(test$trial, trialCART.predictions))) / nrow(test)
## [1] 0.7580645

## 4.2

library(ROCR)
trialCART.testing.roc = prediction(predict(trialCART, newdata=test)[,2], test$trial)
perf = performance(trialCart.testing.roc, "tpr", "fpr")
## plot(perf)
as.numeric(performance(trialCART.testing.roc, "auc")@y.values)
## [1] 0.8371063

## 1.6


## 1.7

##prp(model.1.6)

## 1.8

## There is no reason to think there was anything wrong with the split. CART did not overfit, which you can check by computing the accuracy of the model on the training set. Over-sparsification is plausible but unlikely, since we selected a very high sparsity parameter. The only conclusion left is simply that bag of words didn't work very well in this case.

## 2.1

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

## 2.2

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

model.2.2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
model.2.2.predictions = predict(model.2.2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, model.2.2.predictions)

## 2.3

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2$NumWordsAdded)
   ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   ## 0.00    0.00    1.00    4.05    3.00  259.00 

## 2.4

train = subset(wikiWords2, split==TRUE)
test = subset(wikiWords2, split==FALSE)

model.2.4 = rpart(Vandal ~ ., data=train, method="class")
model.2.4.predictions = predict(model.2.4, newdata=test, type="class")
sum(diag(table(test$Vandal, model.2.4.predictions)))/nrow(test)
## [1] 0.6552021

## 3.1

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train = subset(wikiWords3, split==TRUE)
test = subset(wikiWords3, split==FALSE)

model.3.1 = rpart(Vandal ~ ., data=train, method="class")
model.3.1.predictions = predict(model.3.1, newdata=test, type="class")
sum(diag(table(test$Vandal, model.3.1.predictions)))/nrow(test)

## 3.2

## sólo 3 splits, ninguno es de palabras. el árbol sigue siendo simple, y lo que más ayudó fue la metadata, no el texto.
