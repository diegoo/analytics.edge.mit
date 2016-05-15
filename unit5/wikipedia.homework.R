wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

## 1.2

library(tm)
library(SnowballC)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded,removeWords,stopwords("english"))
corpusAdded <- tm_map(corpusAdded,stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)

## 1.3

## Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)

## 1.4

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

## 1.5

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

## 1.6

library(rpart)
library(rpart.plot)
model.1.6 = rpart(Vandal ~ ., data=train, method="class")
model.1.6.predictions = predict(model.1.6, newdata=test, type="class")
table(test$Vandal, model.1.6.predictions)

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
