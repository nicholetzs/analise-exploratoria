# Dados das classes
classes <- c("2.35-2.55", "2.55-2.75", "2.75-2.95", "2.95-3.15", "3.15-3.35", "3.35-3.55")
freq <- c(1, 3, 2, 4, 5, 6)

# Limites inferiores e superiores
lim_inf <- c(2.35, 2.55, 2.75, 2.95, 3.15, 3.35)
lim_sup <- c(2.55, 2.75, 2.95, 3.15, 3.35, 3.55)
amplitude <- lim_sup - lim_inf

# Total de observações
n <- sum(freq)

# Frequência acumulada
freq_acum <- cumsum(freq)

# Função para calcular quartis com interpolação
calcula_quartil <- function(posicao) {
  for (i in 1:length(freq)) {
    if (posicao <= freq_acum[i]) {
      L <- lim_inf[i]
      F_ant <- ifelse(i == 1, 0, freq_acum[i-1])
      f_i <- freq[i]
      h <- amplitude[i]
      Q <- L + ((posicao - F_ant) / f_i) * h
      return(round(Q, 2))
    }
  }
}

# Quartis (posições: Q1 = n/4, Q2 = n/2, Q3 = 3n/4)
Q1 <- calcula_quartil(n / 4)
Q2 <- calcula_quartil(n / 2)
Q3 <- calcula_quartil(3 * n / 4)

cat("Q1 =", Q1, "\nQ2 =", Q2, "\nQ3 =", Q3, "\n")

# Construção do histograma
midpoints <- (lim_inf + lim_sup) / 2
barplot(height = freq, names.arg = paste(lim_inf, lim_sup, sep=" - "), 
        col = "skyblue", main = "Histograma - Nível de Potássio", 
        xlab = "Intervalos", ylab = "Frequência", border = "black")
