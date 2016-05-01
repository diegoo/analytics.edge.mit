--------------------------------------------------------------------------------
Year: the observation year.
Month: the observation month.
Temp: the difference in degrees Celsius between the average global temperature in that period and a reference value. This data comes from the Climatic Research Unit at the University of East Anglia.
CO2, N2O, CH4, CFC.11, CFC.12: atmospheric concentrations of carbon dioxide (CO2), nitrous oxide (N2O), methane  (CH4), trichlorofluoromethane (CCl3F; commonly referred to as CFC-11) and dichlorodifluoromethane (CCl2F2; commonly referred to as CFC-12), respectively. This data comes from the ESRL/NOAA Global Monitoring Division.
CO2, N2O and CH4 are expressed in ppmv (parts per million by volume  -- i.e., 397 ppmv of CO2 means that CO2 constitutes 397 millionths of the total volume of the atmosphere)
CFC.11 and CFC.12 are expressed in ppbv (parts per billion by volume). 
Aerosols: the mean stratospheric aerosol optical depth at 550 nm. This variable is linked to volcanoes, as volcanic eruptions result in new particles being added to the atmosphere, which affect how much of the sun's energy is reflected back into space. This data is from the Godard Institute for Space Studies at NASA.
TSI: the total solar irradiance (TSI) in W/m2 (the rate at which the sun's energy is deposited per unit area). Due to sunspots and other solar phenomena, the amount of energy that is given off by the sun varies substantially with time. This data is from the SOLARIS-HEPPA project website.
MEI: multivariate El Nino Southern Oscillation index (MEI), a measure of the strength of the El Nino/La Nina-Southern Oscillation (a weather effect in the Pacific Ocean that affects global temperatures). This data comes from the ESRL/NOAA Physical Sciences Division.
--------------------------------------------------------------------------------

1.1

options(scipen=10)
climate.change <- read.csv("climate_change.csv")

training <- subset(climate.change, Year <= 2006)
testing <- subset(climate.change, Year > 2006)

model.1.1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training)
model.1.1

--------------------------------------------------------------------------------

2.2.

cor(training$N2O, training$MEI)
cor(training$N2O, training$CO2)
cor(training$N2O, training$CH4)
cor(training$N2O, training$CFC.11)
cor(training$N2O, training$CFC.12)
cor(training$N2O, training$Aerosols)
cor(training$N2O, training$TSI)

cor(training$CFC.11, training$MEI)
cor(training$CFC.11, training$CO2)
cor(training$CFC.11, training$CH4)
cor(training$CFC.11, training$N2O)
cor(training$CFC.11, training$CFC.12)
cor(training$CFC.11, training$Aerosols)
cor(training$CFC.11, training$TSI)

> cor(training$N2O, training$MEI)
[1] -0.05081978
> cor(training$N2O, training$CO2)
[1] 0.9767198
> cor(training$N2O, training$CH4)
[1] 0.8998386
> cor(training$N2O, training$CFC.11)
[1] 0.5224773
> cor(training$N2O, training$CFC.12)
[1] 0.8679308
> cor(training$N2O, training$Aerosols)
[1] -0.3370546
> cor(training$N2O, training$TSI)
[1] 0.1997567
> 
> cor(training$CFC.11, training$MEI)
[1] 0.06900044
> cor(training$CFC.11, training$CO2)
[1] 0.5140597
> cor(training$CFC.11, training$CH4)
[1] 0.779904
> cor(training$CFC.11, training$N2O)
[1] 0.5224773
> cor(training$CFC.11, training$CFC.12)
[1] 0.8689852
> cor(training$CFC.11, training$Aerosols)
[1] -0.0439212
> cor(training$CFC.11, training$TSI)
[1] 0.272046

--------------------------------------------------------------------------------

3.

model.3 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = training)
summary(model.3)

--------------------------------------------------------------------------------

4.

model.4 <- step(model.3)

eliminó CH4 solamente

--------------------------------------------------------------------------------

5.

predictTest.5 <- predict(model.4, newdata = testing)
SSE = sum((testing$Temp - predictTest.5)^2)
SST = sum((testing$Temp - mean(training$Temp))^2)
1 - SSE/SST

--------------------------------------------------------------------------------

