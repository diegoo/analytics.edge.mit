gerber <- read.csv("gerber.csv")

## 1.2

nrow(subset(gerber, voting == 1 & civicduty == 1)) / nrow(gerber)
##[1] 0.03493624
nrow(subset(gerber, voting == 1 & hawthorne == 1)) / nrow(gerber)
##[1] 0.03579359
nrow(subset(gerber, voting == 1 & self == 1)) / nrow(gerber)
##[1] 0.03833657
nrow(subset(gerber, voting == 1 & neighbors == 1)) / nrow(gerber)
##[1] 0.04196068

## tapply: mean value of "voting", sorted by whether or not the people were in each group:
## tapply(gerber$voting, gerber$civicduty, mean)
## tapply(gerber$voting, gerber$hawthorne, mean)
## tapply(gerber$voting, gerber$self, mean)
## tapply(gerber$voting, gerber$neighbors, mean)
## The variable with the largest value in the "1" column has the largest fraction of people voting in their group - this is the Neighbors group.

## 1.3

model.logr <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")

## 1.4

table(gerber$voting, predict(model.logr, type = "response") > 0.3)
(134513 + 51966) / nrow(gerber)
## [1] 0.5419578

## 1.5

table(gerber$voting, predict(model.logr, type = "response") > 0.5)

## 1.6

library(ROCR)
model.logr.predictions <- predict(model.logr, type = "response")
model.logr.roc <- prediction(model.logr.predictions, gerber$voting)
as.numeric(performance(model.logr.roc, "auc")@y.values)
## 0.5308461

## 2.1

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

## 2.2

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

## 2.3

nrow(subset(gerber, civicduty == 1 & voting == 1))
##[1] 12021
nrow(subset(gerber, civicduty == 1 & voting == 0))
##[1] 26197
nrow(subset(gerber, civicduty == 1))
##[1] 38218
12021 / 38218
##[1] 0.3145377

## You can find this answer by reading the tree - the people in the civic duty group correspond to the bottom right split, which has value 0.31 in the leaf.

## 2.4

CARTmodel.2.4 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel.2.4)

nrow(subset(gerber, voting == 1 & control == 1 & sex == 1))
##[1] 27715
nrow(subset(gerber, voting == 1 & control == 1 & sex == 0))
##[1] 29015

nrow(subset(gerber, voting == 1 & civicduty == 1 & sex == 1))
##[1] 5856
nrow(subset(gerber, voting == 1 & civicduty == 1 & sex == 0))
##[1] 6165

## gráficamente:
## control group = bottom left: sex = 0 (male) corresponds to a higher voting percentage.
## civic duty group = bottom right: sex = 0 (male) corresponds to a higher voting percentage.

## 3.1

CARTmodel.3.1.1 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel.3.1.1)

CARTmodel.3.1.2 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel.3.1.2, digits = 6)

## 3.2

## The first split says that if control = 1, go left. Then, if sex = 1 (female) predict 0.290456, and if sex = 0 (male) predict 0.302795. On the other side of the tree, where control = 0, if sex = 1 (female) predict 0.334176, and if sex = 0 (male) predict 0.345818. So for women, not being in the control group increases the fraction voting by 0.04372. For men, not being in the control group increases the fraction voting by 0.04302. So men and women are affected about the same.

## 3.3

model.3.3 <- glm(voting ~ control + sex, data=gerber, family="binomial")

## This means that women are less likely to vote, since women have a larger value in the sex variable, and a negative coefficient means that larger values are predictive of 0.

## 3.4

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model.3.3, newdata=Possibilities, type="response")
## ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
##         1         2         3         4 
## 0.3462559 0.3024455 0.3337375 0.2908065 

tree.woman.control.prediction <- 0.290456
lr.woman.control.prediction <- 0.2908065
## 0.0003505 diferencia absoluta

## 3.5

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

## 3.6

predict(LogModel2, newdata=Possibilities, type="response")
##        1         2         3         4 
## 0.3458183 0.3027947 0.3341757 0.2904558 
options("scipen"=100, "digits"=7)
abs(0.2904558 - 0.290456)
## [1] 0.0000002 => es 0 si restringimos a 5 decimales
