census <- read.csv("census.csv")

## 1.1

library(caTools)
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, spl == TRUE)
test <- subset(census, spl == FALSE)

## 1.2

model.1.1 <- glm(over50k ~ ., data = train, family = "binomial")
model.1.1.predictions <- predict(model.1.1, newdata = test, type = "response")
table(test$over50k, model.1.1.predictions > 0.5)
##       FALSE TRUE
## <=50K  9051  662
## >50K   1190 1888
(9051+1888) / nrow(test)
## [1] 0.8552107

## 1.3

table(test$over50k)
## <=50K   >50K 
##  9713   3078 
9713 / nrow(test)
## [1] 0.7593621

## 1.4

library(ROCR)
model.1.1.roc <- prediction(model.1.1.predictions, test$over50k)
as.numeric(performance(model.1.1.roc, "auc")@y.values)
## 0.9061598

## 2.1

library(rpart)
library(rpart.plot)
model.2.1 <- rpart(over50k ~ ., data=train, method="class")
model.2.1.predictions <- predict(model.2.1, newdata = test, type = "class")
table(test$over50k, model.2.1.predictions)

## 2.5

model.2.5.predictions <- predict(model.2.1, newdata = test)

library(ROCR)
model.2.5.roc <- prediction(model.2.5.predictions[,2], test$over50k)
as.numeric(performance(model.2.5.roc, "auc")@y.values)
## 0.8470256

perf.2.5 = performance(model.2.5.roc, "tpr", "fpr") # true positive rate, false positive rate
plot(perf.2.5)

## Plot the ROC curve for the CART model you have estimated. Observe that compared to the logistic regression ROC curve, the CART ROC curve is less smooth than the logistic regression ROC curve. Which of the following explanations for this behavior is most correct? (HINT: Think about what the ROC curve is plotting and what changing the threshold does.)

## Choice 1 is on the right track, but is incorrect, because the number of variables that you use in a model does not determine how the ROC curve looks. In particular, try fitting logistic regression with hourperweek as the only variable; you will see that the ROC curve is very smooth.

## Choice 2 is not correct. The smoothness of the ROC curve will generally depend on the number of data points, but in the case of the particular CART model we have estimated, varying the amount of testing set data will not change the qualitative behavior of the ROC curve.

## Choice 3 is the correct answer. The breakpoints of the curve correspond to the false and true positive rates when the threshold is set to the five possible probability values.

## Choice 4 is also not correct. In logistic regression, the continuity of an independent variable means that you will have a large range of predicted class probabilities in your test set data; this, in turn, means that you will see a large range of true and false positive rates as you change the threshold for generating predictions. In CART, the continuity of the variables does not at all affect the continuity of the predicted class probabilities; for our CART tree, there are only five possible probability values.

## 3.1

library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
model.3.1 <- randomForest(over50k ~ ., data = trainSmall)
model.3.1.predictions <- predict(model.3.1, newdata = test) ## no hace falta type = "class" cuando se usa threshold 0.5
table(test$over50k, model.3.1.predictions)
##      model.3.1.predictions
##        <=50K  >50K
## <=50K   9614    99
## >50K    2028  1050
(9614+1050)/nrow(test)
## 0.8337112

## 3.2

## interpretabilidad de random forest: ver, sobre todos los árboles, la cantidad de veces que una variable se usa como split (valor en eje x del siguiente plot)

vu = varUsed(model.3.1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(model.3.1$forest$xlevels[vusorted$ix]))

## 3.3

## otra medida: cuán impuros son los buckets. importancia de variable: promedio de reducción de impureza de todas las veces que la variable se usa para split. la que más reduce impureza no necesariamente es la misma que en la medida anterior. 

varImpPlot(model.3.1)

## 4.1

library(caret)
library(e1071)

set.seed(2)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.002

## 4.2

model.4.1 = rpart(over50k ~ ., data = train, method="class", cp = 0.002)
model.4.1.predictions = predict(model.4.1, newdata = test)
table(test$over50k, model.4.1.predictions[,2] > 0.5)
##       FALSE TRUE
## <=50K  9178  535
## >50K   1240 1838
sum(diag(table(test$over50k, model.4.1.predictions[,2] > 0.5))) / nrow(test)
## [1] 0.8612306

## 4.3

## This highlights one important tradeoff in building predictive models. By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated. 
