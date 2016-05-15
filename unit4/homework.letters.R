letters <- read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, spl == TRUE)
test <- subset(letters, spl == FALSE)

## 1.1
0.754172

## 1.2

CARTb = rpart(isB ~ . - letter, data=train, method="class")
CARTb.predictions <- predict(CARTb, newdata = test, type = "class")
table(test$isB, CARTb.predictions)
  ##      CARTb.predictions
  ##       FALSE TRUE
  ## FALSE  1118   57
  ## TRUE     43  340
(1118 + 340) / nrow(test)
## [1] 0.9358151

## 1.3

library(randomForest)
set.seed(1000)
model.isB.rf <- randomForest(isB ~ . - letter, data = train)
model.isB.rf.predictions <- predict(model.isB.rf, newdata = test, type = "class")
table(test$isB, model.isB.rf.predictions)
##      model.isB.rf.predictions
##       FALSE TRUE
## FALSE  1165   10
## TRUE      9  374
(1165 + 374) / nrow(test)
## [1] 0.9878049

## 2.1

letters$letter = as.factor(letters$letter)
set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, spl == TRUE)
test <- subset(letters, spl == FALSE)

## 0.2573813

model.2.2 <- rpart(letter ~ . - isB, data=train, method="class")
model.2.2.predictions <- predict(model.2.2, newdata = test, type = "class")
table(test$letter, model.2.2.predictions)
## model.2.2.predictions
##      A   B   P   R
##  A 348   4   0  43
##  B   8 318  12  45
##  P   2  21 363  15
##  R  10  24   5 340
(348 + 318 + 363 + 340) / nrow(test)
## [1] 0.8786906

## 2.3

set.seed(1000)
model.2.3 <- randomForest(letter ~ . - isB, data = train)
model.2.3.predictions <- predict(model.2.3, newdata = test, type = "class")
table(test$letter, model.2.3.predictions)
##     A   B   P   R
## A 390   0   3   2
## B   0 380   1   2
## P   0   5 393   3
## R   3  12   0 364
(390+ 380 + 393 + 364) / nrow(test)
## [1] 0.9801027

