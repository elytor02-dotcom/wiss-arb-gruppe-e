# Titanic-Datensatz
daten <- read.csv("titanic.csv", na.strings = "")

# Anrede
daten$Anrede <- sub("^.*?,\\s*([^\\.]+)\\..*$", "\\1", daten$Name)
daten$Anrede[daten$Anrede %in% c("Don", "Jonkheer")] <- "Sir"
daten$Anrede[daten$Anrede %in% c("the Countess")] <- "Lady"
daten$Anrede[daten$Anrede %in% c("Mme")] <- "Mrs"
daten$Anrede[daten$Anrede %in% c("Ms", "Mlle")] <- "Miss"
daten$Anrede <- factor(daten$Anrede)

# Survived, Sex, Embarked
daten$Survived <- factor(daten$Survived, levels = c(0,1), labels = c("Nein","Ja"))
daten$Sex      <- factor(daten$Sex)
daten$Embarked <- factor(daten$Embarked, levels = c("C","Q","S"), labels = c("Cherbourg", "Queenstown", "Southampton"))

# Pclass
daten$Pclass   <- factor(daten$Pclass, ordered = TRUE, levels = c(3,2,1))

# Age
med <- tapply(daten$Age, daten$Anrede, median, na.rm = TRUE)
daten$Age[is.na(daten$Age)] <- med[daten$Anrede[is.na(daten$Age)]]; rm(med)

# Cabin, Bord, Deck
daten$Cabin <- sub("\\s.*$", "", daten$Cabin)
daten$Bord <- as.numeric(gsub("[^0-9]", "", daten$Cabin))
daten$Bord <- ifelse(is.na(daten$Bord), NA, ifelse(daten$Bord %% 2 == 1, 1, 2))
daten$Bord <- factor(daten$Bord, levels = c(1, 2), labels = c("Steuerbord", "Backbord"))
daten$Deck <- ifelse(is.na(daten$Cabin), NA, substr(daten$Cabin, 1, 1))
daten$Deck <- factor(daten$Deck)

# PassengerId, Name, Ticket, Cabin
daten <- daten[, -which(names(daten) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]

# daten.csv
write.csv(daten, "daten.csv", row.names = FALSE, na = "")

summary(daten)

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
