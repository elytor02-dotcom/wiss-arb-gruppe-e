# Helfer-Funktion: 
# Berechnet Lage- und Streuungsmaße gleichzeitig
# Das spart Code in den Funktionen i und iv

get_stats <- function(vektor) {
  res <- list(
    Mittelwert = mean(vektor, na.rm = TRUE),
    Median = median(vektor, na.rm = TRUE),
    SD = sd(vektor, na.rm = TRUE),
    N = sum(!is.na(vektor))
  )
  return(res)
}
