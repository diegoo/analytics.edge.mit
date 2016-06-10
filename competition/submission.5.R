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

## democrats <- subset(train.relevant, Party == "Democrat")
## republicans <- subset(train.relevant, Party == "Republicans")

## --------------------------------------------------------------------------------

library(randomForest)

set.seed(1234)

model.rf = randomForest(Party ~ USER_ID+EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q109244+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98869+Q98197+Q99480, data=train.relevant)
model.rf.predictions.test <- predict(model.rf, newdata = test.relevant) ## no hace falta type = "class" cuando se usa threshold 0.5

submission = data.frame(USER_ID = test.relevant$USER_ID, Predictions = model.rf.predictions.test)
write.csv(submission, "submission.5.csv", row.names=FALSE)

## --------------------------------------------------------------------------------

plot(EducationLevel ~ Party, data = train.relevant)
plot(Income ~ Party, data = train.relevant)
plot(YOB ~ Party, data = train.relevant)
plot(HouseholdStatus ~ Party, data = train.relevant)
plot(Gender ~ Party, data = train.relevant)
plot(Q98197 ~ Party, data = train.relevant)
plot(Q109244 ~ Party, data = train.relevant)
plot(Q115611 ~ Party, data = train.relevant)
plot(Q113181 ~ Party, data = train.relevant)

prop.table(table(train.relevant$EducationLevel, train.relevant$Party), 1)
prop.table(table(train.relevant$Gender, train.relevant$Party), 1)
prop.table(table(train.relevant$HouseholdStatus, train.relevant$Party), 1)
prop.table(table(train.relevant$Income, train.relevant$Party), 1)
prop.table(table(train.relevant$YOB, train.relevant$Party), 1)
prop.table(table(train.relevant$Q101596, train.relevant$Party), 1)
prop.table(table(train.relevant$Q105840, train.relevant$Party), 1)
prop.table(table(train.relevant$Q106042, train.relevant$Party), 1)
prop.table(table(train.relevant$Q106272, train.relevant$Party), 1)
prop.table(table(train.relevant$Q106388, train.relevant$Party), 1)
prop.table(table(train.relevant$Q107491, train.relevant$Party), 1)
prop.table(table(train.relevant$Q109244, train.relevant$Party), 1)
prop.table(table(train.relevant$Q110740, train.relevant$Party), 1)
prop.table(table(train.relevant$Q114152, train.relevant$Party), 1)
prop.table(table(train.relevant$Q113181, train.relevant$Party), 1)
prop.table(table(train.relevant$Q112478, train.relevant$Party), 1)
prop.table(table(train.relevant$Q115899, train.relevant$Party), 1)
prop.table(table(train.relevant$Q115611, train.relevant$Party), 1)
prop.table(table(train.relevant$Q115390, train.relevant$Party), 1)
prop.table(table(train.relevant$Q115195, train.relevant$Party), 1)
prop.table(table(train.relevant$Q116953, train.relevant$Party), 1)
prop.table(table(train.relevant$Q118892, train.relevant$Party), 1)
prop.table(table(train.relevant$Q119851, train.relevant$Party), 1)
prop.table(table(train.relevant$Q120379, train.relevant$Party), 1)
prop.table(table(train.relevant$Q120472, train.relevant$Party), 1)
prop.table(table(train.relevant$Q121699, train.relevant$Party), 1)
prop.table(table(train.relevant$Q122771, train.relevant$Party), 1)
prop.table(table(train.relevant$Q124122, train.relevant$Party), 1)
prop.table(table(train.relevant$Q98869, train.relevant$Party), 1)
prop.table(table(train.relevant$Q98197, train.relevant$Party), 1)
prop.table(table(train.relevant$Q99480, train.relevant$Party), 1)

## --------------------------------------------------------------------------------
