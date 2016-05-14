Claims = read.csv("ClaimsData.csv")
str(Claims)
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

summary(ClaimsTrain)
104672 / nrow(ClaimsTrain)

# --------------------------------------------------------------------------------

# The values in the penalty matrix are selected to reflect "how bad it is" to make a prediction and be wrong. We chose the values. In particular, the chosen prediction matrix shows that we feel that "predicting 5 when the reality is 1" is 2 times worse than "predicting 5 when reality is 3". The penalty matrix is convenient when "being wrong" is not equally bad for every (prediction, reality) pair.

# For the second question, we are looking for the average penalty using the baseline method (i.e. predicting 1 for everyone). In the video, we saw in the 5-by-5 penalty matrix the penalties for various situations. For example, we receive a penalty of 6 for predicting 1 when the correct answer was 4. That should point you in the right direction.

# PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
# > PenaltyMatrix
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    2    3    4
# [2,]    2    0    1    2    3
# [3,]    4    2    0    1    2
# [4,]    6    4    2    0    1
# [5,]    8    6    4    2    0  <- 6 == (abajo outcome 4 - arriba predicted 1) * 2

# para test de quick question:

table(ClaimsTest$bucket2009)
#      1      2      3      4      5 
# 122978  34840  16390   7937   1057

PenaltyVector <- c(0, (2-1)*2, (3-1)*2, (4-1)*2, (5-1)*2)
# c(0,2,4,6,8)

(34840 * 2 + 16390 * 4 + 7937 * 6 + 1057 * 8) / nrow(ClaimsTest)

# --------------------------------------------------------------------------------


