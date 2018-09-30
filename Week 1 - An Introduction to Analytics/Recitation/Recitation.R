# Load USDA data
setwd("C:\\R\\Analytics Edge")
getwd()
USDA = read.csv("USDA.csv")
str(USDA)
summary(USDA)

# Investigate Sodium
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[which.max(USDA$Sodium)]
HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)

# Visualization
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100), breaks = 2000)
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar (g)")

#Adding Variables
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)

#Relationships bewtween data
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE) #Average Iron sorted by HighProtein
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE) #Max VitaminC sorted by HighCarbs
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE) #Summary VitaminC sorted by HighCarbs