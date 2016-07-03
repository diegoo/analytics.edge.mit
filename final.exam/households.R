households <- read.csv("Households.csv")

dim(subset(households, AfternoonPct >= 100))
#13  6
dim(subset(households, MorningPct >= 100))
#4 6

summary(subset(households, AvgSalesValue >= 150)$AvgDiscount)
# 15.65

library(caret)
preproc = preProcess(households)
HouseholdsNorm = predict(preproc, households)

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

## respuesta nro. de clusters:
## Four clusters and six clusters have very little "wiggle room", which means that the additional clusters are not very distinct from existing clusters. That is, when moving from 3 clusters to 4 clusters, the additional cluster is very similar to an existing one (as well as when moving from 5 clusters to 6 clusters).

set.seed(200)
KMC = kmeans(HouseholdsNorm, centers = 10, iter.max = 1000)
clusters200 = KMC$cluster
table(clusters200)

KMC = kmeans(HouseholdsNorm, centers = 10, iter.max = 1000)
clustersNone= KMC$cluster
table(clustersNone)

set.seed(100)
KMC = kmeans(HouseholdsNorm, centers = 10, iter.max = 1000)
clusters100 = KMC$cluster
table(clusters100)


summary(subset(HouseholdsNorm, cluster == 1)$MorningPct)
summary(subset(HouseholdsNorm, cluster == 1)$AvgProdCount)

summary(subset(HouseholdsNorm, cluster == 1)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 2)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 3)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 4)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 5)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 6)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 7)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 8)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 9)$AvgProdCount)
summary(subset(HouseholdsNorm, cluster == 10)$AvgProdCount)

summary(subset(HouseholdsNorm, cluster == 1)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 2)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 3)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 4)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 5)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 6)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 7)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 8)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 9)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 10)$AvgSalesValue)["Mean"]

summary(subset(HouseholdsNorm, cluster == 1)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 2)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 3)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 4)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 5)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 6)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 7)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 8)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 9)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 10)$NumVisits)["Mean"]

set.seed(5000)
KMC = kmeans(HouseholdsNorm, centers = 5)
clusters5 = KMC$cluster
table(clusters5)
HouseholdsNorm$cluster <- clusters5
summary(subset(HouseholdsNorm, cluster == 1)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 2)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 3)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 4)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 5)$AvgSalesValue)["Mean"]
summary(subset(HouseholdsNorm, cluster == 1)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 2)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 3)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 4)$NumVisits)["Mean"]
summary(subset(HouseholdsNorm, cluster == 5)$NumVisits)["Mean"]

## The cluster centroid shows average behavior in a single cluster - it does not describe every single observation in that cluster or tell us how the cluster compares to other clusters.

boxplot(NumVisits ~ cluster, data = HouseholdsNorm)

## A box plot of NumVisits shows the distribution of the number of visits of the households, and we want to subdivide by cluster. Alternatively, ggplot with y as the cluster and x as the number of visits plots the data, but only geom_point is appropriate to show the distribution of the data.
