Sys.setlocale("LC_ALL", "C")

CPS <- read.csv("CPSData.csv")

## NA's por columna, sin complicarse:
summary(CPS)

## o, si hace falta saber el nro.:

colSums(is.na(CPS))

# 2.4
prop.table(table(CPS$Region,is.na(CPS$MetroAreaCode)), 1)




