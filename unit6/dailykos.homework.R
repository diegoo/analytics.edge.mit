## 1.1

dailykos = read.csv("dailykos.csv")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")

## The distance computation can take a long time if you have a lot of observations and/or if there are a lot of variables. As we saw in recitation, it might not even work if you have too many of either!

## 1.2

plot(kosHierClust)

## The choices 2 and 3 are good cluster choices according to the dendrogram, because there is a lot of space between the horizontal lines in the dendrogram in those cut off spots (draw a horizontal line across the dendrogram where it crosses 2 or 3 vertical lines). The choices of 5 and 6 do not seem good according to the dendrogram because there is very little space.

## 1.3

## Thinking about the application, it is probably better to show the reader more categories than 2 or 3. These categories would probably be too broad to be useful. Seven or eight categories seems more reasonable.

## 1.4

dailykosClusters = cutree(kosHierClust, k = 7)

table(dailykosClusters)
##    1    2    3    4    5    6    7 
## 1266  321  374  139  407  714  209 

## 1.5

HierCluster1 <- dailykos[which(dailykosClusters == 1),]
tail(sort(colMeans(HierCluster1)))
## state republican       poll   democrat      kerry       bush

## 1.6

HierCluster2 <- dailykos[which(dailykosClusters == 2),]
tail(sort(colMeans(HierCluster2)))
##     bush  democrat challenge      vote      poll  november 
## 2.847352  2.850467  4.096573  4.398754  4.847352 10.339564 

HierCluster3 <- dailykos[which(dailykosClusters == 3),]
tail(sort(colMeans(HierCluster3)))
##    elect    parties      state republican   democrat       bush 
## 1.647059   1.665775   2.320856   2.524064   3.823529   4.406417 

HierCluster4 <- dailykos[which(dailykosClusters == 4),]
tail(sort(colMeans(HierCluster4)))
## campaign    voter presided     poll     bush    kerry 
## 1.431655 1.539568 1.625899 3.589928 7.834532 8.438849 

HierCluster5 <- dailykos[which(dailykosClusters == 5),]
tail(sort(colMeans(HierCluster5)))
## american       presided administration            war           iraq 
## 1.090909       1.120393       1.230958       1.776413       2.427518 
##     bush 
## 3.941032 

HierCluster6 <- dailykos[which(dailykosClusters == 6),]
tail(sort(colMeans(HierCluster6)))
##      race      bush     kerry     elect  democrat      poll 
## 0.4579832 0.4887955 0.5168067 0.5350140 0.5644258 0.5812325 

HierCluster7 <- dailykos[which(dailykosClusters == 7),]
tail(sort(colMeans(HierCluster7)))
## democrat    clark   edward     poll    kerry     dean 
## 2.148325 2.497608 2.607656 2.765550 3.952153 5.803828 

## 2.1

k = 7
set.seed(1000)
KMC = kmeans(dailykos, centers = k) ## usa el dataframe directamente, no las distancias
clusters = KMC$cluster
table(clusters)
##   1    2    3    4    5    6    7 
## 328  576  548 1019   27  144  788

## 2.2

KCluster1 <- dailykos[which(clusters == 1),]
tail(sort(colMeans(KCluster1)))
##     state           iraq          kerry administration       presided       bush
##  1.609589       1.616438       1.636986       2.664384       2.767123  11.431507
 
KCluster2 <- dailykos[which(clusters == 2),]
tail(sort(colMeans(KCluster2)))
## primaries  democrat    edward     clark     kerry      dean 
##  2.319444  2.694444  2.798611  3.090278  4.979167  8.277778 

KCluster3 <- dailykos[which(clusters == 3),]
tail(sort(colMeans(KCluster3)))
## administration          iraqi       american           bush            war            iraq
##       1.389892       1.610108       1.685921       2.610108       3.025271        4.093863

KCluster4 <- dailykos[which(clusters == 4),]
tail(sort(colMeans(KCluster4)))
##     elect republican      kerry       poll   democrat       bush 
## 0.6010664  0.6175473  0.6495395  0.7474552  0.7891420  1.1473582 

KCluster5 <- dailykos[which(clusters == 5),]
tail(sort(colMeans(KCluster5)))
##     race     senate      state    parties republican   democrat 
## 2.484663   2.650307   3.521472   3.619632   4.638037   6.993865 

KCluster6 <- dailykos[which(clusters == 6),]
tail(sort(colMeans(KCluster6)))
## democrat      bush challenge      vote      poll  november 
## 2.899696  2.960486  4.121581  4.446809  4.872340 10.370821 

KCluster7 <- dailykos[which(clusters == 7),]
tail(sort(colMeans(KCluster7)))
## presided    voter campaign     poll     bush    kerry 
## 1.324675 1.334416 1.383117 2.788961 5.970779 6.480519 

## 2.3

hclust1 <- which(dailykosClusters == 1)
hclust2 <- which(dailykosClusters == 2)
hclust3 <- which(dailykosClusters == 3)
hclust4 <- which(dailykosClusters == 4)
hclust5 <- which(dailykosClusters == 5)
hclust6 <- which(dailykosClusters == 6)
hclust7 <- which(dailykosClusters == 7)

kclust2 <- which(clusters == 2)
length(kclust2)
## [1] 144

length(intersect(hclust7, kclust2))
## [1] 116

## respuesta oficial
## table(hierGroups, KmeansCluster$cluster)

## 2.4

kclust3 <- which(clusters == 3)
length(kclust3)
## 277
length(intersect(hclust5, kclust3))
## 171

## 2.5

kclust7 <- which(clusters == 7)
length(kclust7)

length(intersect(hclust1, kclust7))
length(intersect(hclust2, kclust7))
length(intersect(hclust3, kclust7))
length(intersect(hclust4, kclust7))
length(intersect(hclust5, kclust7))
length(intersect(hclust6, kclust7))
length(intersect(hclust7, kclust7))

## 2.6

kclust6 <- which(clusters == 6)
length(kclust6)

length(intersect(hclust2, kclust6))
