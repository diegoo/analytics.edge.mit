train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

## --------------------------------------------------------------------------------

relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q101596","Q105840","Q106042","Q106272","Q106388","Q107491","Q108343","Q109244","Q109367","Q110740","Q114152","Q113181","Q112478","Q115899","Q115611","Q115390","Q115195","Q116953","Q118892","Q119851","Q120379","Q120472","Q121699","Q122771","Q124122","Q98059","Q98869","Q98197","Q99480")

## relevant.variables <- c("USER_ID","EducationLevel","Income","YOB","HouseholdStatus","Gender","Q98197","Q98059","Q108343","Q109244","Q109367","Q115611","Q122771")

train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## --------------------------------------------------------------------------------

train.relevant <- train.relevant[-c(4830,4864,5054),]
train.relevant <- subset(train.relevant, YOB <= 2013 & YOB >= 1920)

## --------------------------------------------------------------------------------

library(mice)
test.yob = test.relevant
set.seed(1234)
imputed <- complete(mice(test.yob))
test.relevant$YOB = imputed$YOB

sapply(train.relevant, function(x) sum(is.na(x)))
sapply(test.relevant, function(x) sum(is.na(x)))

## --------------------------------------------------------------------------------

model.log = glm(Party ~ . -USER_ID, data=train.relevant, family=binomial)
model.log.predictions.test <- predict(model.log, newdata=test.relevant, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(model.log.predictions.test < threshold, "Democrat", "Republican"))
submission3 = data.frame(USER_ID = test.relevant$USER_ID, Predictions = PredTestLabels)
## write.csv(submission3, "submission.11.3.csv", row.names=FALSE)
## --------------------------------------------------------------------------------

library(randomForest)

model.rf = randomForest(Party ~ EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q108343+Q109244+Q109367+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98059+Q98869+Q98197+Q99480, data=train.relevant)
model.rf.predictions.test <- predict(model.rf, newdata = test.relevant) ## no hace falta type = "class" cuando se usa threshold 0.5
submission4 = data.frame(USER_ID = test.relevant$USER_ID, Predictions = model.rf.predictions.test)

## --------------------------------------------------------------------------------

library(e1071)

model.nb <- naiveBayes(Party ~ EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q108343+Q109244+Q109367+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98059+Q98869+Q98197+Q99480, data = train.relevant)

model.nb.predictions.test <- predict(model.nb, newdata=test.relevant)
submission8 = data.frame(USER_ID = test.relevant$USER_ID, Predictions = model.nb.predictions.test)

## --------------------------------------------------------------------------------

## library(dismo)
## train.relevant$Party = as.numeric(train.relevant$Party) - 1
## model.gbm = gbm.step(train.relevant, c(1:35), 36)
## predictions = predict(model.gbm, newdata = test.relevant, type = "response")

## --------------------------------------------------------------------------------

library(caret)

fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
set.seed(1234)
gbmFit <- train(Party ~ EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q108343+Q109244+Q109367+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98059+Q98869+Q98197+Q99480, data = train.relevant, method = "gbm", trControl = fitControl, verbose = FALSE)
gbm_pred <- predict(gbmFit, test, type= "prob")
PredTestLabels <- as.factor(ifelse(gbm_pred[,2]<threshold, "Democrat", "Republican"))

## --------------------------------------------------------------------------------

rowmode <- function(d) { apply(d, 1, function(idx) which.max(tabulate(idx))) }

submissions <- cbind(submission3$Predictions, submission4$Predictions, submission8$Predictions)
prediction.labels = as.factor(ifelse(rowmode(submissions) == 1, "Democrat", "Republican"))

submission = data.frame(USER_ID = submission3$USER_ID, Predictions = prediction.labels)
write.csv(submission, "submission.11.csv", row.names=FALSE)
