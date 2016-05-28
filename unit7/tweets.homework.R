## 1.1

tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
rownames(allTweets) <- c(1:1181)

## 2.3

wordcloud(colnames(allTweets), colSums(allTweets), max.words = 100, random.order = FALSE, scale=c(2, 0.25))

## apple

## 2.4

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeWords,c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
rownames(allTweets) <- c(1:1181)

wordcloud(colnames(allTweets), colSums(allTweets), max.words = 100, random.order = FALSE, scale=c(2, 0.25))

## 3.1

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

## 3.2

wordcloud(colnames(allTweets), colSums(allTweets), random.order = TRUE)

## 4.1

library(RColorBrewer)
wordcloud(colnames(allTweets), colSums(allTweets), random.order = FALSE, colors=brewer.pal(9, "Blues"))

