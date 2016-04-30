CPS <- read.csv("CPSData.csv")

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

CPS.merge <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

CPS.merge.birth <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

CPS.merge.birth <- merge(CPS.merge.birth, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

sort(tapply(CPS.merge.birth$Country == "India", CPS.merge.birth$MetroAreaCode, sum, na.rm = TRUE))
