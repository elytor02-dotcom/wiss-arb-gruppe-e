#Funktionen-Toolbox:

source("helfer.R")
library(ggplot2)

# i. Metrische Deskription
# Wir geben Lage- und Streuungsmaße aus (Mittelwert, Median, Standardabweichung)
# sowie Quantile. Dadurch erhält man einen schnellen Überblick über die 
# Verteilung und deren Streuung.
desk_metrisch <- function(df, spalte) {
  stats <- get_stats(df[[spalte]]) # Aufruf der Helfer-Funktion
  cat("--- Deskription metrisch (", spalte, ") ---\n")
  print(as.data.frame(stats))
  cat("\nQuantile:\n")
  print(quantile(df[[spalte]], na.rm = TRUE))
  cat("\n\n")
}

# ii. Kategoriale Deskription
# Für kategoriale Variablen sind Häufigkeitstabellen und relative Anteile am
# sinnvollsten, weil sie die Verteilung der Kategorien direkt zeigen.
desk_kategorial <- function(df, spalte) {
  tab <- table(df[[spalte]], useNA = "ifany")
  prop <- prop.table(tab) * 100
  res <- cbind(Anzahl = tab, Prozent = round(prop, 2))
  cat("--- Deskription kategorial (", spalte, ") ---\n")
  print(res)
  cat("\n\n")
}

# iii. Bivariat: Zwei kategoriale Variablen
# Bei zwei kategorialen Variablen nutzen wir eine Kreuztabelle zur gemeinsamen 
# Verteilung. Wir verwenden den Chi-Quadrat-Test, um zu prüfen, ob die Variablen
# unabhängig sind.
bivariat_kat <- function(df, var1, var2) {
  tab <- table(df[[var1]], df[[var2]])
  test <- chisq.test(tab)
  cat("--- Bivariat kategorial (", var1, " vs ", var2, ") ---\n")
  print(tab)
  cat("\nStatistischer Zusammenhang (p-Wert):", test$p.value, "\n\n")
}

# iv. Bivariat: Metrisch & Gruppierungsvariable
# Wenn die Gruppierungsvariable genau 2 Gruppen hat -> t-Test
# Wenn die Gruppierungsvariable mehr als 2 Gruppen hat -> ANOVA
# um zu prüfen, ob sich die Mittelwerte zwischen den Gruppen unterscheiden.
bivariat_metr_dichotom <- function(df, metr_var, dich_var) {
  
  # Nur die zwei relevanten Spalten nehmen
  tmp <- df[, c(metr_var, dich_var)]
  
  # NA entfernen
  tmp <- tmp[!is.na(tmp[[metr_var]]) & !is.na(tmp[[dich_var]]), ]
  
  # Anzahl Gruppen bestimmen
  gruppen <- unique(tmp[[dich_var]])
  gruppen <- gruppen[!is.na(gruppen)]
  n_gruppen <- length(gruppen)
  
  # Gruppenstatistiken ausgeben
  ergebnis <- aggregate(tmp[[metr_var]] ~ tmp[[dich_var]], data = tmp,
                        FUN = function(x) unlist(get_stats(x)))
  colnames(ergebnis) <- c(dich_var, "Statistiken")
  
  cat("--- Bivariat metrisch (", metr_var, " nach ", dich_var, ") ---\n")
  print(ergebnis)
  
  # Test abhängig von Gruppenanzahl
  if (n_gruppen == 2) {
    cat("\nT-Test zum Mittelwertvergleich:\n")
    print(t.test(tmp[[metr_var]] ~ tmp[[dich_var]], data = tmp))
    
  } else if (n_gruppen > 2) {
    cat("\nANOVA (mehr als 2 Gruppen):\n")
    modell <- aov(tmp[[metr_var]] ~ tmp[[dich_var]], data = tmp)
    print(summary(modell))
    
  } else {
    cat("\nKein Test möglich: weniger als 2 Gruppen vorhanden.\n")
  }
  
  cat("\n\n")
}

# v. Visualisierung: 3-4 kategoriale Variablen
# Mit geom_bar(position="fill") visualisieren wir Anteile
# (relative Häufigkeiten), damit Gruppen mit unterschiedlichen Stichprobengrößen
# vergleichbar sind.
plot_multi <- function(df, x_var, fill_var, facet_var1, facet_var2 = NULL) {
  p <- ggplot(df, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Anteil",
      title = paste("Analyse von", x_var, "nach", fill_var),
      subtitle = paste(
        "Gruppiert nach:",
        facet_var1,
        ifelse(is.null(facet_var2), "", paste("und", facet_var2))
      )
    ) +
    theme_minimal()
  
  # Bedingung für 3 oder 4 Variablen (facet_wrap vs facet_grid)
  if (is.null(facet_var2)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var1)))
  } else {
    p <- p + facet_grid(as.formula(paste(facet_var1, "~", facet_var2)))
  }
  
  return(p)
}
