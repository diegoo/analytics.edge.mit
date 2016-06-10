submission3 = read.csv("submission.3.log.csv")
submission6 = read.csv("submission.6.csv")
submission7 = read.csv("submission.7.csv")

todos <- cbind(submission3$Predictions, submission6$Predictions, submission7$Predictions)
todos.mode <- apply(todos, 1, function(idx) which.max(tabulate(idx)))
PredTestLabels = as.factor(ifelse(todos.mode == 1, "Democrat", "Republican"))

submission = data.frame(USER_ID = submission3$USER_ID, Predictions = PredTestLabels)
write.csv(submission, "3.6.7.mode.csv", row.names=FALSE)
