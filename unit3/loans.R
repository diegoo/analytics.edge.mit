loans <- read.csv("loans.csv")

## 1.2

colnames(loans)[colSums(is.na(loans)) > 0]
# or
summary(loans) # muestra los NA!

## 1.3

length(which(rowSums(is.na(loans)) > 0))
# 62

table(loans$not.fully.paid)
##    0    1 
## 8045 1533 

table(loans[which(rowSums(is.na(loans)) > 0),]$not.fully.paid)
##  0  1 
## 50 12 

## > 1533 / 8045
## [1] 0.1905531
## > 12 / 50
## [1] 0.24

## respuesta oficial:
## Answering this question requires analyzing the loans with missing data. We can build a data frame limited to observations with some missing data with the following command:

## missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

## From nrow(missing), we see that only 62 of 9578 loans have missing data; removing this small number of observations would not lead to overfitting. From table(missing$not.fully.paid), we see that 12 of 62 loans with missing data were not fully paid, or 19.35%. This rate is similar to the 16.01% across all loans, so the form of biasing described is not an issue. However, to predict risk for loans with missing data we need to fill in the missing values instead of removing the observations.

## 1.4

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

## respuesta oficial: Imputation predicts missing variable values for a given observation using the variable values that are reported. We called the imputation on a data frame with the dependent variable not.fully.paid removed, so we predicted the missing values using only other independent variables.

## 2.1

loans.imputed <- read.csv("loans_imputed.csv")

set.seed(144)
library(caTools)
split = sample.split(loans.imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans.imputed, split == TRUE)
test = subset(loans.imputed, split == FALSE)

model.loans.2.1 <- glm(not.fully.paid ~ ., data=train, family=binomial)
summary(model.loans.2.1)

# fico = -9.317e-03 == -0.009317

700 * (-0.009317) - 710 * (-0.009317)
## log(O(A)) - log(O(B)) == 0.09317
## sabiendo que: log(x/y) = log(x) - log(y)
## log(O(A)/O(B)) == 0.09317
## O(A)/O(B) == exp(0.09317)
## 1.097648

## respuesta oficial: Using the answer from the previous question, the predicted odds of loan A not being paid back in full are exp(0.09317) = 1.0976 times larger than the predicted odds for loan B. Intuitively, it makes sense that loan A should have higher odds of non-payment than loan B, since the borrower has a worse credit score.

## 2.3

predicted.risk <- predict(model.loans.2.1, newdata=test, type="response")

table(test$not.fully.paid, predicted.risk > 0.5)
##   FALSE TRUE
## 0  2400   13
## 1   457    3

(2400 + 3) / (2400 + 3 + 457 + 13)
## [1] 0.8364079

table(test$not.fully.paid)
##    0    1 
## 2413  460 

2413 / (2413 + 460)
## [1] 0.8398886

## 2.4

library(ROCR)
roc.test <- prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(roc.test, "auc")@y.values)

## 3.1

model.loans.3.1 <- glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(model.loans.3.1)

## Decreased significance between a bivariate and multivariate model is typically due to correlation. From cor(train$int.rate, train$fico), we can see that the interest rate is moderately well correlated with a borrower's credit score.

## 3.2

predicted.risk.3.2 <- predict(model.loans.3.1, newdata=test, type="response")
which.max(predicted.risk.3.2)
## 5869 
## 1780

summary(predicted.risk.3.2)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.06196 0.11550 0.15080 0.15960 0.18930 0.42660 

table(test$not.fully.paid, predicted.risk.3.2 > 0.5) 
  ##   FALSE
  ## 0  2413
  ## 1   460

## 3.3

roc.test.3.2 <- prediction(predicted.risk.3.2, test$not.fully.paid)
as.numeric(performance(roc.test.3.2, "auc")@y.values)
## 0.6239081

## 4.1

## $c investment in a loan that has an annual interest rate r over a period of t years.
## continuous compounding of interest: payback = c * exp(r*t) dollars by the end of the t years

10 * exp(0.06 * 3)
## 11.97217

## 5.1

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

## 10 * (exp(0.2121 * 3) - 1)

## 6.1

highInterest <- subset(test, int.rate >= 0.15)

## average profit of a $1 investment in one of these high-interest loans
## 0.2251

table(highInterest$not.fully.paid)
## 0.2517162

## 6.2

library(mice)
library(caTools)

loans <- read.csv("loans_imputed.csv")

set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

model.6.2 <- glm(not.fully.paid ~ ., data=train, family=binomial)
test$predicted.risk <- predict(model.6.2, newdata=test, type="response")

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

highInterest <- subset(test, int.rate >= 0.15)
cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk < as.numeric(cutoff))
str(selectedLoans)
sum(selectedLoans$profit)

## 31.28
## 18

## respuesta oficial:
## selectedLoans can be constructed with the following code:
## selectedLoans = subset(highInterest, predicted.risk <= cutoff)
## sum(selectedLoans$profit)
## table(selectedLoans$not.fully.paid)

