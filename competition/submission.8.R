train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

## --------------------------------------------------------------------------------

## 3: "Q98059","Q108343","Q109244","Q109367","Q115611"

relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q101596","Q105840","Q106042","Q106272","Q106388","Q107491","Q108343","Q109244","Q109367","Q110740","Q114152","Q113181","Q112478","Q115899","Q115611","Q115390","Q115195","Q116953","Q118892","Q119851","Q120379","Q120472","Q121699","Q122771","Q124122","Q98059","Q98869","Q98197","Q99480")
train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## test.yob = c(34,65,69,100,122,148,178,215,221,299,341,424,432,540,581,674,677,689,747,760,822,823,830,837,840,847,850,867,907,927,939,945,951,968,976,992,994,995,996,1016,1025,1044,1046,1050,1073,1081,1086,1095,1096,1099,1112,1124,1135,1136,1142,1143,1149,1154,1185,1187,1198,1204,1215,1230,1232,1238,1243,1245,1256,1272,1274,1278,1291,1292,1295,1298,1351,1369,1374,1379,1382,1392)

## --------------------------------------------------------------------------------

train.relevant[4830,]$YOB = 1980
train.relevant[4864,]$YOB = 1981
train.relevant[5054,]$YOB = 1996

train.relevant <- subset(train.relevant, YOB <= 2013 & YOB >= 1920)
test.relevant[which(is.na(test.relevant$YOB)),]$YOB <- 0

sapply(train.relevant, function(x) sum(is.na(x)))
sapply(test.relevant, function(x) sum(is.na(x)))

## --------------------------------------------------------------------------------

library(e1071)

model.nb <- naiveBayes(Party ~ EducationLevel+Income+YOB+HouseholdStatus+Gender+Q101596+Q105840+Q106042+Q106272+Q106388+Q107491+Q108343+Q109244+Q109367+Q110740+Q114152+Q113181+Q112478+Q115899+Q115611+Q115390+Q115195+Q116953+Q118892+Q119851+Q120379+Q120472+Q121699+Q122771+Q124122+Q98059+Q98869+Q98197+Q99480, data = train.relevant)

model.nb.predictions.test <- predict(model.nb, newdata=test.relevant)
submission = data.frame(USER_ID = test.relevant$USER_ID, Predictions = model.nb.predictions.test)
write.csv(submission, "submission.8.csv", row.names=FALSE)

## --------------------------------------------------------------------------------

rowmode <- function(d) { apply(d, 1, function(idx) which.max(tabulate(idx))) }

submission3 = read.csv("submission.3.log.csv")
submission6 = read.csv("submission.6.csv")
submission8 = read.csv("submission.8.csv")

submissions <- cbind(submission3$Predictions, submission6$Predictions, submission8$Predictions)
prediction.labels = as.factor(ifelse(rowmode(submissions) == 1, "Democrat", "Republican"))

submission = data.frame(USER_ID = submission3$USER_ID, Predictions = prediction.labels)
write.csv(submission, "3.6.8.mode.csv", row.names=FALSE)
