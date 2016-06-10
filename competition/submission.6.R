train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

## --------------------------------------------------------------------------------

relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q101596","Q105840","Q106042","Q106272","Q106388","Q107491","Q109244","Q110740","Q114152","Q113181","Q112478","Q115899","Q115611","Q115390","Q115195","Q116953","Q118892","Q119851","Q120379","Q120472","Q121699","Q122771","Q124122","Q98869","Q98197","Q99480")
train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## --------------------------------------------------------------------------------

train.relevant[4830,]$YOB = 1980
train.relevant[4864,]$YOB = 1981
train.relevant[5054,]$YOB = 1996

train.relevant <- subset(train.relevant, YOB <= 2003 & YOB > 1900)
train.relevant$YOB <- cut(train.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))
test.relevant[which(is.na(test.relevant$YOB))]$YOB <- 0
test.relevant$YOB <- cut(test.relevant$YOB, breaks = c(0, 1900, 1960, 1970, 1980, 1990, 2000, 2020), labels = c("UNKNOWN", "1900-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2020"))
test.relevant[which(is.na(test.relevant$YOB)), c("YOB")] <- as.factor("UNKNOWN")

sapply(train.relevant, function(x) sum(is.na(x)))
sapply(test.relevant, function(x) sum(is.na(x)))

## --------------------------------------------------------------------------------

set.seed(1234)

model.log <- glm(Party ~ USER_ID+EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q109244+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98869+Q98197+Q99480, data=train.relevant, family=binomial)
model.log.predictions.test <- predict(model.log, newdata=test.relevant, type="response")
threshold <- 0.5
PredTestLabels <- as.factor(ifelse(model.log.predictions.test < threshold, "Democrat", "Republican"))

submission = data.frame(USER_ID = test.relevant$USER_ID, Predictions = PredTestLabels)
write.csv(submission, "submission.6.csv", row.names=FALSE)

## --------------------------------------------------------------------------------
