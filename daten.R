daten <- read.csv("titanic.csv", na.strings = "")
daten$Survived <- factor(daten$Survived, levels = c(0,1), labels = c("No","Yes"))
daten$Sex      <- factor(daten$Sex)
daten$Pclass   <- factor(daten$Pclass, levels = c(1,2,3), labels = c("1st", "2nd", "3rd"))
daten$Embarked <- factor(daten$Embarked, levels = c("C","Q","S"))
summary(daten[-c(1, 4, 9, 10)])

table(daten$Survived)
prop.table(table(daten$Survived))

table(daten$Sex)
table(daten$Pclass)
table(daten$Embarked)

t1 <- prop.table(table(daten$Sex, daten$Survived), margin = 1); t1
t2 <- prop.table(table(daten$Pclass, daten$Survived), margin = 1); t2
t3 <- prop.table(table(daten$Embarked, daten$Survived), margin = 1); t3

barplot(t1, beside = TRUE, legend = TRUE, main = "Survival by Sex", ylab = "Relative frequency")
barplot(t2, beside = TRUE, legend = TRUE, main = "Survival by Class", ylab = "Relative frequency")
barplot(t3, beside = TRUE, legend = TRUE, main = "Survival by Embarkation Port", ylab = "Relative frequency")
