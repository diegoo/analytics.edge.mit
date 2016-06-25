train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

## --------------------------------------------------------------------------------

## relevant.variables <- c("USER_ID", "EducationLevel","Income","YOB","HouseholdStatus","Gender","Q101596","Q105840","Q106042","Q106272","Q106388","Q107491","Q108343","Q109244","Q109367","Q110740","Q114152","Q113181","Q112478","Q115899","Q115611","Q115390","Q115195","Q116953","Q118892","Q119851","Q120379","Q120472","Q121699","Q122771","Q124122","Q98059","Q98869","Q98197","Q99480")

relevant.variables <- c("USER_ID","EducationLevel","Income","YOB","HouseholdStatus","Gender","Q98197","Q98059","Q108343","Q109244","Q109367","Q115611")
train.relevant <- train[c(relevant.variables, "Party")]
test.relevant <- test[relevant.variables]

## --------------------------------------------------------------------------------

train.relevant[4830,]$YOB = 1980
train.relevant[4864,]$YOB = 1981
train.relevant[5054,]$YOB = 1996

train.relevant <- subset(train.relevant, YOB <= 2013 & YOB >= 1920)
test.relevant[which(is.na(test.relevant$YOB)),]$YOB <- 0

sapply(train.relevant, function(x) sum(is.na(x)))
sapply(test.relevant, function(x) sum(is.na(x)))

## --------------------------------------------------------------------------------

rowmode <- function(d) { apply(d, 1, function(idx) which.max(tabulate(idx))) }

submission3 = read.csv("submission.3.log.csv")
submission6 = read.csv("submission.6.csv")
submission8 = read.csv("submission.8.csv")

submissions <- cbind(submission3$Predictions, submission6$Predictions, submission8$Predictions)
prediction.labels = as.factor(ifelse(rowmode(submissions) == 1, "Democrat", "Republican"))

submission = data.frame(USER_ID = submission3$USER_ID, Predictions = prediction.labels)
write.csv(submission, "submission.10.csv", row.names=FALSE)
