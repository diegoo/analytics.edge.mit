library(ggplot2)
library(maps)
library(ggmap)

edges = read.csv("edges.csv")
users = read.csv("users.csv")

## 1.1

## ver q1_1.py

## respuesta oficial: From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset who are Facebook friends. However, each pair (A, B) must be counted twice, because B is a friend of A and A is a friend of B. To think of this in simpler terms, consider a network with just new people, A and B, and a single edge (A, B). Even though there are two vertices and one edge, each user has on average one friend.
## For our network, the average number of friends per user is 292/59=4.95.
## Finally, note that in all likelihood these users have a much higher number of Facebook friends. We are computing here the average number of people in this dataset who are their friends, instead of the average total number of Facebook friends.

## 1.2

## From table(users$locale, users$school), we read that all students listed at schools A and B listed their locale as B.

## 1.3

table(users$gender, users$school)

subset(users, (gender == 'A' & school == 'A') | (gender == 'A' & school == 'AB'))
subset(users, (gender == 'B' & school == 'A') | (gender == 'B' & school == 'AB'))

## 2.1

library(igraph)

## 2.2

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

## 2.3

## no olvidar la isla grande!

## 2.4

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)

## 2.5

## From table(degree(g)) or summary(degree(g)), we see that the maximum degree of any node in the graph is 18 and the minimum degree of any node is 0. Therefore, the maximum size of any point is 18/2+2=11, and the minimum size is 0/2+2=2.

## 3.1

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
##plot(g, vertex.label=NA)

## 3.2

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

## 3.3

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
