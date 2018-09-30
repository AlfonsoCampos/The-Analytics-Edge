# Load mvt data
setwd("C:\\R\\Analytics Edge")
getwd()

CPS = read.csv("CPSData.csv")

#Summarizing data
str(CPS)
summary(CPS)
sort(table(CPS$State))
(116639+7073)/(116639+7073+7590)
table(CPS$Race,CPS$Hispanic)

# Evaluating NAs
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))
5609/(5609+20330)
9871/(31631+9871)
10674/(20010+10674)
8084/(25093+8084)

tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)

# Integrating MetroArea data
MetroAreaMap = read.csv("MetroAreaCodes.csv")
str(MetroAreaMap)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))
sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

# Integrating Country data
CountryMap = read.csv("CountryCodes.csv")
str(CountryMap)
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)
sort(table(CPS$Country))
sort(tapply(CPS$Country=="United States",CPS$MetroArea,mean,na.rm=TRUE))
1-0.6913397
sort(tapply(CPS$Country=="India",CPS$MetroArea,mean,na.rm=TRUE))
sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,mean,na.rm=TRUE))
sort(tapply(CPS$Country=="India",CPS$MetroArea,mean,na.rm=TRUE))
sort(tapply(CPS$Country=="Somalia",CPS$MetroArea,mean,na.rm=TRUE))