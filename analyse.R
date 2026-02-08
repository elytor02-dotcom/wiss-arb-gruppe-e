#Analyse:

daten <- read.csv("daten.csv", na.strings = "")
source("funktionen.R")

#Metrische Daten
desk_metrisch(daten, "Age")
desk_metrisch(daten, "Fare")
desk_metrisch(daten, "SibSp")
desk_metrisch(daten, "Parch")


desk_kategorial(daten, "Survived")
desk_kategorial(daten, "Pclass")
desk_kategorial(daten, "Sex")
desk_kategorial(daten, "Embarked")
desk_kategorial(daten, "Anrede")
desk_kategorial(daten, "Bord") #78% NA nicht geeignet für Analyse
desk_kategorial(daten, "Deck") #77% NA auch nicht geeignet
desk_kategorial(daten, "SibSp")
desk_kategorial(daten, "Fare_class")
desk_kategorial(daten, "Age_class")

#Ziel ist es herauszufinden welche Gruppen am ehesten überlebt haben
#Hypothese ist Frauen Kinder und Passagiere der ersten Klasse

bivariat_kat(daten, "Survived", "Sex")

#female male
#Ja      233  109
#Nein     81  468
#Statistischer Zusammenhang (p-Wert): 1.197357e-58
#Der Großteil der weiblichen Personen hat überlebt während der Großteil der Männlichen nicht
#Überlebt hat, die erste These scheint zu stimmen

bivariat_kat(daten, "Survived", "Pclass")

#         1   2   3
#Ja      136  87 119
#Nein     80  97 372
#Statistischer Zusammenhang (p-Wert): 4.549252e-23 
#Erste Klasse überlebt eher als das sie sterben zweite Klasse stirbt eher als 
#Zu überleben und in der dritten klasse ist der Großteil gestorben

bivariat_kat(daten, "Pclass", "Sex")

#female male
#1     94  122
#2     76  108
#3    144  347
#Statistischer Zusammenhang (p-Wert): 0.0002063886
#In der dritten Klasse gibt es jedoch auch deutlich mehr männliche als weibliche Personen
#vergleichen wir beides zusammen

plot_multi(daten, "Sex","Survived", "Pclass")

#Man kann deutlich sehen es haben mehr weibliche Personen aus der dritten Klasse
#überlebt als männliche aus der ersten, aber auch deutlich mehr männliche aus der
#ersten als männliche aus der dritten und deutlich mehr weibliche aus der erste
#als weibliche aus der dritten beides scheint also Einfluss zu haben nur das Geschlecht
#deutlich mehr

#jetzt zum Alter

bivariat_metr_dichotom(daten,"Age","Survived")

#  mean in group Ja mean in group Nein 
#  28.06629           30.21494
#Die Überlenden zum im durchschnitt jünger als die gestorbenen, aber um es nochmal
#besser zu überprüfen teilen wir sie in Kinder[1-12] Jugendliche (12-21]
#Erwachsene (21-50] und Ältere (50+) ein 

bivariat_kat(daten, "Survived", "Age_class")

#       Kinder Jugendliche Erwachsene Ältere
#Ja       42          69        209     22
#Nein     31         102        374     42
#sieht sehr danach aus ob Kinder am ehesten überleben vergleichen wir es noch
#mit den anderen Gruppen

bivariat_kat(daten, "Age_class","Pclass")

#Es kommen deutlich mehr Frauen(Prozentual) aus Queenstown und Cherbourg als aus
#Southampton und Passagiere aus Cherbourg haben einen deutlich höheren Anteil 
#an der ersten Klasse 
#                1   2   3
#Kinder          4  17  52
#Jugendliche    21  24 126
#Erwachsene     152 128 303
#Ältere          39  15  10
#Es gab tatsächlich kaum Kinder aus der ersten Klasse von daher scheint die
#Theorie auch zu stimmen, interessant ist auch, dass die meisten älteren Leute
#In der ersten Klasse waren und trotzdem eher die Minderheit von ihnen überlebt hat

bivariat_kat(daten, "Age_class","Sex")

#              female male
#Kinder          32   41
#Jugendliche     88   83
#Erwachsene     177  406
#Ältere          17   47
#Kinder und Jugendliche sind eher gemischt während Erwachsene und Ältere Größteils
#Männlich waren

#Vergleiche wir alle 3 Daten zusammen in Bezug auf Überlebensrate

plot_multi(daten, "Sex","Survived", "Pclass", "Age_class")

#Was erstmal seltsam ist, ist das Mädchen der ersten Klasse zu 100%
#gestorben sind obwohl sie eigentlich am sichersten sein sollen testen wir einmal

sum(daten$Sex == "female" & daten$Pclass == "1" & daten$Age_class == "Kinder")

#[1] 1 es gab genau eine Person auf die alle 3 eigenschaften zutrat es scheint als 
# Ein Einzelfall zu sein
#(
sum(daten$Sex == "female" & daten$Pclass == "2" & daten$Age_class == "Kinder")
#8
sum(daten$Sex == "female" & daten$Pclass == "1" & daten$Age_class == "Jugendliche")
#16
sum(daten$Sex == "female" & daten$Pclass == "2" & daten$Age_class == "Jugendliche")
#11
sum(daten$Sex == "male" & daten$Pclass == "2" & daten$Age_class == "Kinder")
#9
sum(daten$Sex == "female" & daten$Pclass == "1" & daten$Age_class == "Ältere")
#13
sum(daten$Sex == "female" & daten$Pclass == "3" & daten$Age_class == "Ältere")
#1
#)

#Bei den Kindern scheint das Geschlecht wenig Einfluss auf das Überleben zu haben
#Zwischen Jugendlich/Älteren und Erwachsenen gibt es kaum Unterschiede

bivariat_kat(daten, "Embarked", "Pclass")
bivariat_kat(daten, "Embarked", "Sex")
bivariat_kat(daten, "Embarked", "Age_class")
plot_multi(daten, "Sex", "Embarked", "Pclass")
plot_multi(daten, "Sex", "Embarked", "Age_class", "Pclass")

#man sieht hier beim Zustiegshafen, dass deutlich mehr Frauen aus Queenstown und
#Cherbourg kommen und deutlich mehr Leute aus der ersten Klasse aus Cherborg
#mögliche Unterschiede die sich beim Zustiegshafen feststellen lassen sind 
#wahrscheinlich darauf zurückzuführen machen wir den test

plot_multi(daten, "Sex","Survived", "Embarked", "Pclass")

#Es ergeben sich kaum Unterschiede außer vielleicht, dass die Überlebensrate von Frauen 
#Aus der dritten Klasse aus Southampton etwas geringer ist

#Bei den Preisen vermuten wir, je niedriger die Klasse desto teurer der Preis und 
#Eventuell zahlen Kinder etwas weniger

bivariat_metr_dichotom(daten, "Fare", "Pclass")

#  Pclass Statistiken.Mittelwert Statistiken.Median Statistiken.SD Statistiken.N
#1      1               84.15469           60.28750       78.38037     216.00000
#2      2               20.66218           14.25000       13.41740     184.00000
#3      3               13.67555            8.05000       11.77814     491.00000
#Das sieht ziemlich eindeutig aus, erste und zweite Klasse unterscheidet sich
#deutlich Zweite und Dritte dafür gar nicht so sehr

bivariat_kat(daten, "Fare_class", "Pclass")

#Es gibt jedoch auch einige aus der dritten Klasse die mehr bezahlt haben als
#einige aus der ersten es könnte also noch andere Faktoren geben
#Probieren wir es mit dem Alter

bivariat_metr_dichotom(daten, "Fare","Age_class")

#tatsächlich zahlt der Median der Kinder sogar mehr als der Erwachsene
#Vergleichen wir es zusammen mit der Klasse:

plot_multi(daten,"Age_class","Fare_class", "Pclass")

#tatsächlich zahlen Kinder in allen 3 Klassen mehr als die anderen und Ältere 
#Menschen etwas weniger
#Die Logik hierbei ist zu hinterfragen, da zum einen Kinder normalerweise nicht 
#zu teuereren Preisen Tickets bekommen sollten als Erwachsene und diese meistens 
#auch nicht alleine solche Fahrten antreten würden.
#Zu erörtern ist, ob sich die Preise von Kindern überwiegend mit denen von Erwachsenen
#decken, was bedeuten würde, dass es sich womöglich um Gruppentickets handeln muss.
#Dies wäre eine mögliche Erklärung für die unerwarteten Preise nach Personengruppe.

#Vergleich mit Verdacht auf identische Preise:
sort(table(daten$Fare), decreasing = TRUE)[1:5]
#Es scheint tatsächlich so zu sein, dass es sich um Gruppentickets handeln muss,
#da die Preise für Kinder und Erwachsene fast identisch sind.

#vergleichen wir es noch mit dem Einstieghafen und dem Geschlecht

plot_multi(daten, "Pclass","Fare_class","Sex","Embarked")

#Männer zahlen in der Regel etwas weniger in der ersten Klasse
#Und Passagiere aus Cherbourg zahlen in erster und zweiter Klasse mehr und
#Passagiere aus Southampton zahlen etwas mehr in der dritten Klasse
#In Queenstown is kaum jemand etwas anderes als 3 Klasse gefahren von daher
#Sollte man das weg lassen
#Zum Schluss testen wir noch ob mehr bezahlen unabhängig von der Klasse und 
#Geschlecht die Überlebensrate erhöht

plot_multi(daten, "Pclass","Survived","Fare_class", "Sex")

#tatsächlich sieht es eher so aus als würde in der dritten Klasse mehr bezahlen
#die Überlebenswahrscheinlichkeit von Frauen eher verringern

sum(daten$Sex == "female" & daten$Pclass == "3" & daten$Fare_class == "medium")
sum(daten$Sex == "female" & daten$Pclass == "3" & daten$Fare_class == "low")

#Aber bei 16 Daten ist das vielleicht nicht ganz so repäsentativ.
