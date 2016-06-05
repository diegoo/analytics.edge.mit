library(randomForest)

set.seed(1234)
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

train.complete <- train[complete.cases(train),]
test.complete <- na.roughfix(test)

model.rf = randomForest(Party ~ . -USER_ID, data=train.complete)
model.rf.predictions.test <- predict(model.rf, newdata = test.complete) ## no hace falta type = "class" cuando se usa threshold 0.5

submission = data.frame(USER_ID = test.complete$USER_ID, Predictions = model.rf.predictions.test)
write.csv(submission, "submission.2.rf.csv", row.names=FALSE)

library(caret)
varImpPlot(model.rf.predictions.test)

## "EducationLevel"
## "Income"
## "YOB"
## "HouseholdStatus"
## "Q115611"
## "Q113181"
## "Q98197"
## "Q121011"
## "Q118892"
## "Q110740"
## "Gender"
