parole <- read.csv("parole.csv")

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

## > str(parole)
## 'data.frame':   675 obs. of  9 variables:
##  $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
##  $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
##  $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
##  $ state            : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
##  $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
##  $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
##  $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ crime            : Factor w/ 4 levels "1","2","3","4": 4 3 3 1 1 4 3 1 3 2 ...
##  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

## 3.2

model.4.1 <- glm(violator ~ ., data=train, family=binomial)

## coefficient c for a variable: log odds (or Logit) are increased by c for a unit increase in the variable == odds are multiplied by e^c for a unit increase in the variable.

## For parolees A and B who are identical other than A having committed multiple offenses, the predicted log odds of A is 1.61 more than the predicted log odds of B. Then we have:

## ln(odds of A) = ln(odds of B) + 1.61
## exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)
## exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)
## odds of A = exp(1.61) * odds of B
## odds of A= 5.01 * odds of B

## In the second step we raised e to the power of both sides. In the third step we used the exponentiation rule that e^(a+b) = e^a * e^b. In the fourth step we used the rule that e^(ln(x)) = x.

## 4.3

## logistic function
p(y=1) = 1 / (1 + exp(-(b0 + b1*x1 + b2*x2 + ... + bn*xn)))
## valores positivos => clase 1, valores negativos => clase 0

## odds
odds = p(y=1) / p(y=0)
## odds > 1 => clase 1 más probable, odds < 1 => clase 0 más probable

## odds & log(odds) functions
odds = exp(b0 + b1*x1 + b2*x2 + ... + bn*xn)
log(odds) = b0 + b1*x1 + b2*x2 + ... + bn*xn
## log(odds) == "logit". se ve y funciona como regresión lineal: mayor log(odds), mayor probabilidad de clase 1


## male=1,race=white,age=50,state=Maryland,sentence=3months,maximum.sentence=12months,multiple.offenses=false,larceny
parolee <- data.frame(c(1),c(1),c(50),c(3),c(12),c(1),c(2),c(0))
names(parolee) <- c("male","race","age","time.served","max.sentence","state","crime","multiple.offenses")
parolee$state = factor(parolee$state, levels = 1:4)
parolee$crime = factor(parolee$crime, levels = 1:4)

probabilidad.de.violator.4.3 <- predict(model.4.1, newdata=parolee, type="response")
## resultado de predict() usando type=response es _probabilidad_
##        1 
## 0.154383 

probabilidad.de.violator.4.3 <- predict(model.4.1, newdata=parolee)
## resultado de predict() sin type=response es _log(odds)__
##        1 
## -1.70063 

## calculando a mano con los coeficientes:

intercept           = -4.2411574
c.male              = 0.3869904
c.race              = 0.8867192
c.age               = -0.0001756
c.time.served       = -0.1238867
c.max.sentence      = 0.0802954
c.multiple.offenses = 1.6119919
c.crime2            = 0.6837143

## log(odds) de que sea violator:
log.odds.4.3 <- intercept + 1*c.male + 1*c.race + 50*c.age + 3*c.time.served + 12*c.max.sentence + 1*c.crime2
## -1.700629

## odds de que sea violator:
odds.4.3 <- exp(log.odds.4.3)
## [1] 0.1825687

## p(y=1) == probabilidad de que sea violator:
1 / (1 + exp(-1 * log.odds.4.3))
## 0.1543832


## respuesta oficial:

## log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4

## This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that log(odds) = -1.700629.

## odds = exp(-1.700629) = 0.183
## predicted probability of violation is 1/(1+exp(1.700629)) = 0.154.


## 5.1

predictions.parole.test <- predict(model.4.1 newdata=test, type="response")
summary(predictions.parole.test)

## 5.2

table(test$violator, as.numeric(predictions.parole.test >= 0.5))
##   FALSE TRUE
## 0   167   12
## 1    11   12

sensitivity <- 12 / (11 + 12)
specificity <- 167 / (167 + 12)
accuracy <- (167 + 12) / (167 + 12 + 12 + 11)

## > sensitivity
## [1] 0.5217391
## > specificity
## [1] 0.9329609
## > accuracy
## [1] 0.8861386

## 5.3

## accuracy de baseline model es: predicciones correctas / (predicciones correctas + predicciones incorrectas)
## 179 / 202 == 0.8861386

## 5.4

## false positive == modelo dice que será violator pero no lo es
## false negative == modelo dice que no será violator pero lo es
## false negative es peor para board => poner threshold más bajo que 0.5 => va a predecir que será violator más seguido

## respuesta oficial:
## If the board used the model for parole decisions, a negative prediction would lead to a prisoner being granted parole, while a positive prediction would lead to a prisoner being denied parole. The parole board would experience more regret for releasing a prisoner who then violates parole (a negative prediction that is actually positive, or false negative) than it would experience for denying parole to a prisoner who would not have violated parole (a positive prediction that is actually negative, or false positive). Decreasing the cutoff leads to more positive predictions, which increases false positives and decreases false negatives. Meanwhile, increasing the cutoff leads to more negative predictions, which increases false negatives and decreases false positives. The parole board assigns high cost to false negatives, and therefore should decrease the cutoff.

## 5.5

## respuesta oficial: The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 23 false negatives. Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 is likely of value to the board. From the previous question, the parole board would likely benefit from decreasing the logistic regression cutoffs, which decreases the false negative rate while increasing the false positive rate.

## 5.6

library(ROCR)
roc.test <- prediction(predictions.parole.test, test$violator)
as.numeric(performance(roc.test, "auc")@y.values)
## 0.8945834

## 5.7

## curva ROC: The AUC deals with differentiating between a randomly selected positive and negative example. It is independent of the regression cutoff selected.

## 6.1

## While expanding the dataset to include the missing parolees and labeling each as violator=0 would improve the representation of non-violators, it does not capture the true outcome, since the parolee might become a violator after 2004. Though labeling these new examples with violator=NA correctly identifies that we don't know their true outcome, we cannot train or test a prediction model with a missing dependent variable. As a result, a prospective dataset that tracks a cohort of parolees and observes the true outcome of each is more desirable. Unfortunately, such datasets are often more challenging to obtain (for instance, if a parolee had a 10-year term, it might require tracking that individual for 10 years before building the model). Such a prospective analysis would not be possible using the 2004 National Corrections Reporting Program dataset.

