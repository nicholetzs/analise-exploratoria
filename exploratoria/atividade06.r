# Dados brutos agrupados
classes <- c("0-2", "2-4", "4-8", "8-12", "12-16", "16-22")
lim_inf <- c(0, 2, 4, 8, 12, 16)
lim_sup <- c(2, 4, 8, 12, 16, 22)
amplitude <- lim_sup - lim_inf
midpoints <- (lim_inf + lim_sup) / 2

# Frequências
freq_antes <- c(442, 200, 130, 34, 10, 3)
freq_depois <- c(80, 200, 280, 179, 43, 8)

# Função para estimar medidas em dados agrupados
medidas_agrupadas <- function(lim_inf, lim_sup, freq) {
  n <- sum(freq)
  freq_acum <- cumsum(freq)
  h <- lim_sup - lim_inf
  midpoints <- (lim_inf + lim_sup) / 2
  
  # Média
  media <- sum(midpoints * freq) / n
  
  # Quartil (Interpolação)
  calcula_quartil <- function(posicao) {
    for (i in 1:length(freq)) {
      if (posicao <= freq_acum[i]) {
        L <- lim_inf[i]
        F_ant <- ifelse(i == 1, 0, freq_acum[i-1])
        f_i <- freq[i]
        h_i <- h[i]
        Q <- L + ((posicao - F_ant) / f_i) * h_i
        return(round(Q, 2))
      }
    }
  }
  
  Q1 <- calcula_quartil(n / 4)
  Q2 <- calcula_quartil(n / 2)  # Mediana
  Q3 <- calcula_quartil(3 * n / 4)
  
  return(list(media = round(media, 2), Q1 = Q1, Q2 = Q2, Q3 = Q3))
}

# Cálculos
antes <- medidas_agrupadas(lim_inf, lim_sup, freq_antes)
depois <- medidas_agrupadas(lim_inf, lim_sup, freq_depois)

# Resultados
cat("ANTES do treinamento:\n")
print(antes)
cat("\nDEPOIS do treinamento:\n")
print(depois)

# Histogramas
par(mfrow = c(1, 2))  # Dois gráficos lado a lado

barplot(height = freq_antes, names.arg = classes, col = "tomato", 
        main = "Antes do Treinamento", xlab = "Km Corridos", ylab = "Frequência", border = "black")

barplot(height = freq_depois, names.arg = classes, col = "steelblue", 
        main = "Depois do Treinamento", xlab = "Km Corridos", ylab = "Frequência", border = "black")
