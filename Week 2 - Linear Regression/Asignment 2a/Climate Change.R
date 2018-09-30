climate_change = read.csv("climate_change.csv")
str(climate_change)

climate_train = subset(climate_change, Year < 2007)
climate_test = subset(climate_change, Year >=2007 )

summary(climate_train)
summary(climate_test)

model1= lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train)
summary(model1)
cor(climate_change)

model2= lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_train)
summary(model2)

modelBest = step(model1)
summary(modelBest)

predictTest=predict(modelBest, newdata=climate_test)
SSE = sum((climate_test$Temp - predictTest)^2)
SST = sum((climate_test$Temp - mean(climate_train$Temp))^2)
1 - SSE/SST

summary(climate_test)

?subset
wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1= lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
SSE
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)
SSE
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE
modelX = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(modelX)
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine )
summary(model5)

wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest=predict(model4, newdata=wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)
