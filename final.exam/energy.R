energy <- read.csv("energy.csv")

summary(subset(energy, STATE == "AZ")$GenTotalRenewable)
summary(subset(energy, STATE == "CA")$GenTotalRenewable)
summary(subset(energy, STATE == "ID")$GenTotalRenewable)
summary(subset(energy, STATE == "MA")$GenTotalRenewable)

summary(subset(energy, STATE == "ID" & YEAR == 2000)$GenTotalRenewable)
summary(subset(energy, STATE == "ID" & YEAR == 2002)$GenTotalRenewable)
summary(subset(energy, STATE == "ID" & YEAR == 2007)$GenTotalRenewable)
summary(subset(energy, STATE == "ID" & YEAR == 2011)$GenTotalRenewable)

summary(subset(energy, presidential.results == 0)$AllSourcesCO2, na.rm = TRUE) # 0 republican
summary(subset(energy, presidential.results == 1)$AllSourcesCO2, na.rm = TRUE) #

summary(subset(energy, presidential.results == 0)$AllSourcesNOx, na.rm = TRUE) # 0 republican
summary(subset(energy, presidential.results == 1)$AllSourcesNOx, na.rm = TRUE) #

What is the correlation between overall CO2 emissions and energy sales made to industrial facilities? Note that the variables  contain NAs. Use the parameter: use="complete" to handle NAs in this question.


cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use="complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use="complete")
cor(energy$AllSourcesCO2, energy$EsalesCommercial, use="complete")

boxplot(EPriceTotal ~ STATE, data = energy)
table(energy$EPriceTotal, energy$STATE)

price.bystate <- aggregate(energy$EPriceTotal, by=list(energy$STATE), FUN=mean)
price.bystate[order(price.bystate$x),]

generation.bystate <- aggregate(energy$GenTotal, by=list(energy$STATE), FUN=mean)
generation.bystate[order(generation.bystate$x),]


set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

mod <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, family = "binomial")
summary(mod)

mod.predictions <- predict(mod, newdata = test, type = "response")
table(test$GenSolarBinary, mod.predictions > 0.5)

test.republicans <- subset(test, presidential.results == 0)
test.democrats <- subset(test, presidential.results == 1)

mod.predictions.republicans <- predict(mod, newdata = test.republicans, type = "response")
table(test.republicans$GenSolarBinary, mod.predictions.republicans > 0.5)

mod.predictions.democrats <- predict(mod, newdata = test.democrats, type = "response")
table(test.democrats$GenSolarBinary, mod.predictions.democrats > 0.5)

vars <- c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")
train.limited <- train[,vars]
test.limited <- test[,vars]


library(caret)
library(flexclust)

preproc.train <- preProcess(train.limited)
train.norm <- predict(preproc.train, train.limited)
set.seed(144)
km <- kmeans(train.norm, centers = 2, iter.max = 1000)
km.kcca = as.kcca(km, train.norm)
## cluster sizes:
##   1   2 
## 308 181 
clusterTrain = predict(km.kcca)

train1 <- train[which(clusterTrain == 1),]
train2 <- train[which(clusterTrain == 2),]

table(train1$presidential.results)
table(train2$presidential.results)

mod1 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train1, family = "binomial")
mod2 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train2, family = "binomial")

preproc.test <- preProcess(test.limited)
test.norm <- predict(preproc.test, test.limited)
set.seed(144)
km <- kmeans(test.norm, centers = 2, iter.max = 1000)
km.kcca = as.kcca(km, test.norm)
clusterTest = predict(km.kcca)
test1 <- test[which(clusterTest == 1),]
test2 <- test[which(clusterTest == 2),]

predictions <- predict(mod, newdata = test1, type = "response")
table(test1$GenSolarBinary, predictions > 0.5)

## clusterTest = predict(km.kcca, newdata=normTest)
