# Titanic-Datensatz-Aufbereitung:

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
daten$Embarked <- factor(daten$Embarked, levels = c("C","Q","S"),
                         labels = c("Cherbourg", "Queenstown", "Southampton"))

# Pclass
daten$Pclass <- factor(daten$Pclass, ordered = TRUE, levels = c(3,2,1))

# Age: Imputation über Median je Anrede
med <- tapply(daten$Age, daten$Anrede, median, na.rm = TRUE)
daten$Age[is.na(daten$Age)] <- med[daten$Anrede[is.na(daten$Age)]]
rm(med)

# Age_class (für Analyse sinnvoll, da Alter stark interpretiert wird)
daten$Age_class <- cut(
  daten$Age,
  breaks = c(0, 12, 21, 50, 100),
  labels = c("Kinder", "Jugendliche", "Erwachsene", "Ältere"),
  right = TRUE,
  include.lowest = TRUE
)

# Fare_class (für Analyse sinnvoll, da Ticketpreis stark streut)
daten$Fare_class <- cut(
  daten$Fare,
  breaks = c(0, 30, 100, 513),
  labels = c("low", "medium", "high"),
  right = TRUE,
  include.lowest = TRUE
)

# Cabin, Bord, Deck
daten$Cabin <- sub("\\s.*$", "", daten$Cabin)

daten$Bord <- as.numeric(gsub("[^0-9]", "", daten$Cabin))
daten$Bord <- ifelse(is.na(daten$Bord), NA, ifelse(daten$Bord %% 2 == 1, 1, 2))
daten$Bord <- factor(daten$Bord, levels = c(1, 2), labels = c("Steuerbord", "Backbord"))

daten$Deck <- ifelse(is.na(daten$Cabin), NA, substr(daten$Cabin, 1, 1))
daten$Deck <- factor(daten$Deck)

# PassengerId, Name, Ticket, Cabin entfernen
daten <- daten[, -which(names(daten) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]

# daten.csv speichern
write.csv(daten, "daten.csv", row.names = FALSE, na = "")
