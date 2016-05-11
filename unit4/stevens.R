library(rpart)
library(rpart.plot)
library(caTools)
library(ROCR)

stevens = read.csv("stevens.csv")
str(stevens)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)
## prp(StevensTree)
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

StevensTree.5mb = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
## prp(StevensTree.5mb)

StevensTree.100mb = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
## prp(StevensTree.100mb)

## --------------------------------------------------------------------------------

## install.packages("randomForest")
library(randomForest)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)

## --------------------------------------------------------------------------------

# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

