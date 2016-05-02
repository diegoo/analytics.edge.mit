1.1

FluTrain <- read.csv("FluTrain.csv")
FluTest <- read.csv("FluTest.csv")

which.max(FluTrain$ILI)
which.max(FluTrain$Queries)

--------------------------------------------------------------------------------

1.3

plot(log(FluTrain$ILI), FluTrain$Queries)

--------------------------------------------------------------------------------

2.2

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

--------------------------------------------------------------------------------

2.3

para regresión lineal de una sola variable, r-squared = cuadrado de la correlación entre dependiente y predictora.

> cor(log(FluTrain$ILI), FluTrain$Queries)
[1] 0.8420333
> cor(log(FluTrain$ILI), FluTrain$Queries)^2
[1] 0.7090201
> log(1/cor(log(FluTrain$ILI), FluTrain$Queries))
[1] 0.1719357
> exp(-0.5 * cor(log(FluTrain$ILI), FluTrain$Queries))
[1] 0.6563792

summary(FluTrend1)$r.squared
0.7090201

--------------------------------------------------------------------------------

3.1

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")

--------------------------------------------------------------------------------

3.2

(Observed ILI - Estimated ILI) / Observed ILI

(FluTest$ILI[11] - PredTest1[11]) / FluTest$ILI[11]
0.04623827

--------------------------------------------------------------------------------

3.3

SSE.testing <- sum((PredTest1 - FluTest$ILI)^2)
RMSE.testing <- sqrt(SSE.testing / nrow(FluTest))

--------------------------------------------------------------------------------

4.1

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

summary(FluTrain$ILILag2)
# o
sum(is.na(FluTrain$ILILag2))

--------------------------------------------------------------------------------

4.2

plot(log(ILILag2),log(FluTrain$ILI))

--------------------------------------------------------------------------------

4.3

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

--------------------------------------------------------------------------------

5.1

FluTest$ILILag2 = coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))
summary(FluTest$ILILag2)

--------------------------------------------------------------------------------

5.3

> FluTrain$ILI[416]
[1] 1.852736
> FluTrain$ILI[417]
[1] 2.12413

FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]

--------------------------------------------------------------------------------

5.4

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))
SSE.testing <- sum((PredTest2 - FluTest$ILI)^2)
RMSE.testing <- sqrt(SSE.testing / nrow(FluTest))
# or
RMSE.testing <- sqrt(mean((PredTest2 - FluTest$ILI)^2))

--------------------------------------------------------------------------------

5.5

FluTrend1 0.7490645 vs FluTrend2 0.2942029

--------------------------------------------------------------------------------
