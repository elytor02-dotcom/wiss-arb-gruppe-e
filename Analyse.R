daten <- read.csv("daten.csv", na.strings = "")
source("funktionen.R")

daten$Age_class <- cut(
  daten$Age,
  breaks = c(0,12,21,50,100),
  labels = c("Kinder", "Jugendliche","Erwachsene","Ältere"),
)

daten$Fare_class <- cut(
  daten$Fare,
  breaks = c(0,30,100,513),
  labels = c("low", "medium","high")
)

desk_metrisch(daten, "Age")

desk_metrisch(daten, "Fare")

desk_metrisch(daten, "SibSp")
desk_kategorial(daten, "SibSp") 

desk_metrisch(daten, "Parch")
desk_kategorial(daten, "Parch")


desk_kategorial(daten, "Survived")
desk_kategorial(daten, "Pclass")
desk_kategorial(daten, "Sex")
desk_kategorial(daten, "Embarked")
desk_kategorial(daten, "Anrede")
desk_kategorial(daten, "Bord") #78% NA nicht geeignet für Analyse
desk_kategorial(daten, "Deck") #77% NA auch nicht geeignet
desk_kategorial(daten, "SibSp")
desk_kategorial(daten, "Agegroup")
desk_kategorial(daten, "Fare_class")
desk_kategorial(daten, "Age_class")




bivariat_kat(daten, "Survived", "Pclass") #4.549252e-23 
bivariat_kat(daten, "Survived", "Sex") #1.197357e-58
bivariat_kat(daten, "Survived", "Embarked") #1.769922e-06
bivariat_kat(daten, "Survived", "Fare") #1.164764e-11 
bivariat_kat(daten, "Survived", "Age")#9.73068e-05 
bivariat_kat(daten, "Fare", "Age") #1.89453e-29 
bivariat_kat(daten, "Fare", "Pclass") #1.098617e-131
bivariat_kat(daten, "Fare","Sex") #7.368883e-07 
#Fare und Embarked ergibt NaN
bivariat_kat(daten, "Sex", "Pclass")#0.0002063886 
bivariat_kat(daten, "Sex", "Age") #7.621334e-10
bivariat_kat(daten, "Sex", "Embarked") #0.001258525 
bivariat_kat(daten, "Age", "Pclass") #3.695126e-13
bivariat_kat(daten, "Age", "Embarked") #1.726494e-08
bivariat_kat(daten, "Embarked","Pclass") #8.435268e-26
bivariat_kat(daten, "Pclass", "Fare_class")
             
             
             
             
bivariat_metr_dichotom(daten, "Age", "Survived")
bivariat_metr_dichotom(daten, "Age", "Sex")
bivariat_metr_dichotom(daten, "Age", "Embarked")
bivariat_metr_dichotom(daten, "Age", "Preisklasse")
bivariat_metr_dichotom(daten, "Age", "Pclass")
bivariat_metr_dichotom(daten, "Fare", "Embarked")
bivariat_metr_dichotom(daten, "Fare", "Survived")
bivariat_metr_dichotom(daten, "Fare", "Sex")
bivariat_metr_dichotom(daten, "Fare", "Pclass")
bivariat

plot_multi(daten, "Sex","Survived", "Pclass")
plot_multi(daten, "Pclass", "Survived", "Agegroup")
plot_multi(daten, "Pclass", "Preisklasse","Agegroup")
