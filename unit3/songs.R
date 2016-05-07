songs <- read.csv("songs.csv")

## songs2010 <- subset(songs, year == 2010)
table(songs$year)

## 1.2

MichaelJackson = subset(songs, artistname == "Michael Jackson")

## 1.4

table(songs$timesignature)

## 1.5

songs[which.max(songs$tempo),]

## 2.1

SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
dim(SongsTrain)
dim(SongsTest)

## 2.2

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

## 2.3

## coeficiente positivo => más probabilidad de que valor de predicción sea 1

## If you look at the output summary(model), where model is the name of your logistic regression model, you can see that the coefficient estimates for the confidence variables (timesignature_confidence, key_confidence, and tempo_confidence) are positive. This means that higher confidence leads to a higher predicted probability of a Top 10 hit.

## 3.1

cor(songs$loudness, songs$energy)

## 3.2

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

## 3.3

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

## Looking at the output of summary(SongsLog3), we can see that loudness has a positive coefficient estimate, meaning that our model predicts that songs with heavier instrumentation tend to be more popular. This is the same conclusion we got from Model 2.

## 4.1

predictions.SongTest <- predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictions.SongTest >= 0.45)

  ##   FALSE TRUE
  ## 0   309    5
  ## 1    40   19

## accuracy = (TN + TP) / number of observations == (TN + TP) / (TN + TP + FN + FP)

predictions.SongTest.accuracy <- (309 + 19) / 373
## 0.8793566

## 4.2

## baseline model = constante FALSE para todos (373 registros)
## > table(SongsTest$Top10)
##   0   1 
## 314  59 
## accuracy de baseline model = aciertos / total de registros == 314 / 373 == 0.8418231

## 4.3

## 19 hits correctamente predichos
## 5 hits incorrectamente predichos


## 4.4

## sensitivity = TP / (TP + FN) = 19 (19 + 40) = 0.3220339
## specificity = TN / TN + FP = 309 / (309 + 5) = 0.9840764

## 4.5

## Model 3 has a very high specificity, meaning that it favors specificity over sensitivity. While Model 3 only captures less than half of the Top 10 songs, it still can offer a competitive edge, since it is very conservative in its predictions.
