# Load mvt data
setwd("C:\\R\\Analytics Edge")
getwd()
mvt = read.csv("mvtWeek1.csv")
str(mvt)
summary(mvt)

# Investigate Dates
mvt$Date[77001]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Arrest, mvt$Month)

# Visualiza crime
hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest, data=mvt)

# Analyze arrests per year
table(mvt$Arrest,mvt$Year)
2152/(18517+2152)
1212/(13068+1212)
550/(13542+550)

# Analyze crime per location
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription == "GAS STATION" | 
                  LocationDescription == "DRIVEWAY - RESIDENTIAL" | 
                  LocationDescription == "STREET" | 
                  LocationDescription == "ALLEY" | 
                  LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" )

nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
summary(Top5)
table(Top5$Arrest, Top5$LocationDescription)
249/(249+2059)
132/(132+1543)
439/(439+1672)
1603/(1603+13249)
11595/(11595+144969)

table(Top5$Weekday, Top5$LocationDescription == "GAS STATION")
table(Top5$Weekday, Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL")

which.max(mvt$ID)
