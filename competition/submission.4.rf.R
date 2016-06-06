library(randomForest)

set.seed(1234)
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q98197","Q109244","Q115611","Q113181")
train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## --------------------------------------------------------------------------------

train.relevant[4830,]$YOB = 1980
train.relevant[4864,]$YOB = 1981
train.relevant[5054,]$YOB = 1996

train.relevant <- subset(train.relevant, YOB <= 2016)
train.relevant$YOB <- cut(train.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))
test.relevant[which(is.na(test.relevant$YOB))]$YOB <- 0
test.relevant$YOB <- cut(test.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))
test.relevant[which(is.na(test.relevant$YOB)), c("YOB")] <- as.factor("UNKNOWN")

## --------------------------------------------------------------------------------

model.rf = randomForest(Party ~ EducationLevel + Income + YOB + HouseholdStatus + Gender + Q98197 + Q109244 + Q115611 + Q113181, data=train.relevant)
model.rf.predictions.test <- predict(model.rf, newdata = test.relevant) ## no hace falta type = "class" cuando se usa threshold 0.5

submission = data.frame(USER_ID = test.relevant$USER_ID, Predictions = model.rf.predictions.test)
write.csv(submission, "submission.4.rf.csv", row.names=FALSE)

## --------------------------------------------------------------------------------
