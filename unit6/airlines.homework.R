## 1.1

airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

## 1.3

library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

## 2.1

dists = dist(airlinesNorm, method="euclidean")
hier.clusters = hclust(dists, method="ward.D")
plot(hier.clusters)

## If you run a horizontal line down the dendrogram, you can see that there is a long time that the line crosses 2 clusters, 3 clusters, or 7 clusters. However, it it hard to see the horizontal line cross 6 clusters. This means that 6 clusters is probably not a good choice.

## 2.2

clusterGroups = cutree(hier.clusters, k = 5)
table(clusterGroups)

## 2.3

tapply(airlines$Balance, clusterGroups, mean)
##        1         2         3         4         5 
## 57866.90 110669.27 198191.57  52335.91  36255.91 
tapply(airlines$QualMiles, clusterGroups, mean)
##         1            2            3            4            5 
## 0.6443299 1065.9826590   30.3461538    4.8479263    2.5111773 
tapply(airlines$BonusMiles, clusterGroups, mean)
##         1         2         3         4         5 
## 10360.124 22881.763 55795.860 20788.766  2264.788 
tapply(airlines$BonusTrans, clusterGroups, mean)
##         1         2         3         4         5 
## 10.823454 18.229287 19.663968 17.087558  2.973174 
tapply(airlines$FlightMiles, clusterGroups, mean)
##        1          2          3          4          5 
## 83.18428 2613.41811  327.67611  111.57373  119.32191 
tapply(airlines$FlightTrans, clusterGroups, mean)
##         1         2         3         4         5 
## 0.3028351 7.4026975 1.0688259 0.3444700 0.4388972 
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
##        1        2        3        4        5 
## 6235.365 4402.414 5615.709 2840.823 3060.081 

## otra manera

colMeans(subset(airlines, clusterGroups == 1))
colMeans(subset(airlines, clusterGroups == 2))
colMeans(subset(airlines, clusterGroups == 3))
colMeans(subset(airlines, clusterGroups == 4))
colMeans(subset(airlines, clusterGroups == 5))

## otra manera
## an even more compact way of finding the centroids would be to use the function "split" to first split the data into clusters, and then to use the function "lapply" to apply the function "colMeans" to each of the clusters:

lapply(split(airlines, clusterGroups), colMeans)

## 3.1

k = 5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
clusters = KMC$cluster
table(clusters)
##   1    2    3    4    5 
## 408  141  993 1182 1275 

## 3.2

hier.clusters = clusterGroups
kmeans.clusters = KMC$cluster

## proportion of hierarchical clusters that coincide with K-means clusters
diag(prop.table(table(hier.clusters, kmeans.clusters)))
##    1           2           3           4           5 
## 0.001000250 0.034258565 0.033008252 0.007501875 0.252063016 
