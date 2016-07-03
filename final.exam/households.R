households <- read.data("Households.csv")

dim(subset(households, AfternoonPct >= 100))
#13  6
dim(subset(households, MorningPct >= 100))
#4 6

summary(subset(households, AvgSalesValue >= 150)$AvgDiscount)
# 15.65
