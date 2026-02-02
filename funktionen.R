source("helfer.R")
library(ggplot2)

# i. Metrische Deskription
desk_metrisch <- function(df, spalte) {
  stats <- get_stats(df[[spalte]]) # Nutzt den Helfer
  cat("Deskription für", spalte, ":\n")
  print(as.data.frame(stats))
  cat("\nQuantile:\n")
  print(quantile(df[[spalte]], na.rm = TRUE))
}

# ii. Kategoriale Deskription
desk_kategorial <- function(df, spalte) {
  tab <- table(df[[spalte]], useNA = "ifany")
  prop <- prop.table(tab) * 100
  res <- cbind(Anzahl = tab, Prozent = round(prop, 2))
  print(res)
}

# iii. Bivariat: Zwei kategoriale Variablen
bivariat_kat <- function(df, var1, var2) {
  tab <- table(df[[var1]], df[[var2]])
  
  # Chi-Quadrat Test, statistischer Zusammenhang
  test <- chisq.test(tab)
  print(tab)
  cat("\nStatistischer Zusammenhang (p-Wert):", test$p.value, "\n")
}

# iv. Bivariat: Metrisch & Dichotom
bivariat_metr_dichotom <- function(df, metr_var, dich_var) {
  
  # Nutzt den Helfer get_stats für jede Gruppe
  ergebnis <- aggregate(df[[metr_var]] ~ df[[dich_var]], data = df, 
                        FUN = function(x) unlist(get_stats(x)))
  colnames(ergebnis) <- c(dich_var, "Statistiken")
  print(ergebnis)
  
  # T-Test für den Unterschied
  print(t.test(df[[metr_var]] ~ df[[dich_var]], data = df))
}

# v. Visualisierung: 3-4 kategoriale Variablen
plot_multi <- function(df, x_var, fill_var, facet_var1, facet_var2 = NULL) {
  p <- ggplot(df, aes_string(x = x_var, fill = fill_var)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Anteil", title = paste("Analyse von", x_var, "nach", fill_var)) +
    theme_minimal()
  
  if (is.null(facet_var2)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var1)))
  } else {
    p <- p + facet_grid(as.formula(paste(facet_var1, "~", facet_var2)))
  }
  return(p)
}

