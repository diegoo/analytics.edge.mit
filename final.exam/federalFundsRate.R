fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)

table(fedFunds$RaisedFedFunds)
##   0   1 
## 291 294 

294 / (291+294)

fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)

## randomForest necesita response variable como factor si queremos hacer classification

library(caTools)
set.seed(201)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

train <- subset(fedFunds, spl == TRUE)
test <- subset(fedFunds, spl == FALSE)

model.5 <- glm(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, family = binomial)

## Imagine you are an analyst at a bank and your manager has asked you to predict whether the federal funds rate will be raised next month. You know that the rate has been lowered for 3 straight months (Streak = -3) and that the previous month's rate was 1.7%. The unemployment rate is 5.1% and the homeownership rate is 65.3%. The current U.S. president is a Republican and the next election will be held in 18 months. According to the logistic regression model you built in Problem 5, what is the predicted probability that the interest rate will be raised?

(Intercept)          9.121012
PreviousRate        -0.003427
Streak               0.157658
Unemployment        -0.047449
HomeownershipRate   -0.136451
DemocraticPres1      0.347829
MonthsUntilElection -0.006931

9.121012 + (-0.003427*(1.7)) + (0.157658*(-3)) + (-0.047449*(5.1)) + (-0.136451*(65.3)) + (0.347829*(0)) + (-0.006931*(18))

PreviousRate=-0.003427,Streak=0.157658,Unemployment=-0.047449,HomeownershipRate=-0.136451,DemocraticPres1=0.347829,MonthsUntilElection=-0.006931

question.6 <- data.frame(PreviousRate=1.7,Streak=-3,Unemployment=5.1,HomeownershipRate=65.3,DemocraticPres=as.factor(0),MonthsUntilElection=18)
predict(model.5, newdata=question.6, type="response")
## 0.3463297
## lo mismo se puede hacer aplicando la fórmula a mano: -0.6347861 => Then you need to plug this into the logistic response function to get the predicted probability.

## The coefficients of the model are the log odds associated with that variable; so we see that the odds of being sold are exp(0.347829)=1.41599 those of an otherwise identical month. This means the month is predicted to have 41.6% higher odds of being sold.

model.5.test.predictions <- predict(model.5, newdata=test, type="response")
table(test$RaisedFedFunds, model.5.test.predictions > 0.5)
##   FALSE TRUE
## 0    60   27
## 1    31   57

library(ROCR)
model.5.roc.test <- prediction(model.5.test.predictions, test$RaisedFedFunds)
as.numeric(performance(model.5.roc.test, "auc")@y.values)

## respuesta oficial: The AUC is the proportion of time the model can differentiate between a randomly selected true positive and true negative.

model.5.test.perf = performance(model.5.roc.test, "tpr", "fpr") # true positive rate, false positive rate
plot(model.5.test.perf, colorize=TRUE)

## A model with threshold 0 predicts 1 for all observations, yielding a 100% true positive rate and a 100% false positive rate. => logistic regression threshold = 0

library(caret)
set.seed(201)
train.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = seq(0.001,0.05, 0.001))
best.cp.selection <- train(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, method = "rpart", trControl = train.control, tuneGrid = cp.grid)

model.15 <- rpart(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, method="class", cp = 0.016)
library(rpart.plot)
prp(cart)

model.15.predictions <- predict(model.15, newdata = test, type = "class")
table(test$RaisedFedFunds, model.15.predictions)
