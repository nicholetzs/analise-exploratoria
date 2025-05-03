# Dados do Ensino Médio (distribuição de frequência)
tempo_medio <- c(
  rep(350, 14),   # 300-400 (ponto médio 350)
  rep(450, 46),   # 400-500
  rep(550, 58),   # 500-600
  rep(650, 76),   # 600-700
  rep(750, 68),   # 700-800
  rep(850, 62),   # 800-900
  rep(950, 48),   # 900-1000
  rep(1050, 22),  # 1000-1100
  rep(1150, 6)    # 1100-1200
)

# Dados Universitários (amostra individual)
universitarios <- c(350, 560, 580, 710, 945, 880, 760, 640, 660, 820,
                    775, 910, 920, 850, 810, 790, 890, 685, 730, 850,
                    745, 640, 1010, 420, 770, 850, 915, 840, 930, 895)

# Função para calcular estatísticas descritivas
calcular_stats <- function(dados, nome) {
  media <- mean(dados)
  mediana <- median(dados)
  dp <- sd(dados)
  variancia <- var(dados)
  q1 <- quantile(dados, 0.25)
  q3 <- quantile(dados, 0.75)
  
  cat("\nEstatísticas para", nome, ":\n")
  cat("Média:", round(media, 2), "minutos\n")
  cat("Mediana:", mediana, "minutos\n")
  cat("Desvio Padrão:", round(dp, 2), "minutos\n")
  cat("Variância:", round(variancia, 2), "\n")
  cat("1º Quartil:", q1, "minutos\n")
  cat("3º Quartil:", q3, "minutos\n")
  
  return(c(media, mediana, dp, variancia, q1, q3))
}

# Calculando estatísticas
stats_medio <- calcular_stats(tempo_medio, "Estudantes do Ensino Médio")
stats_uni <- calcular_stats(universitarios, "Estudantes Universitários")

# Comparação numérica
cat("\nDiferenças entre grupos:\n")
cat("Diferença de médias (Médio - Uni):", round(stats_medio[1] - stats_uni[1], 2), "minutos\n")
cat("Razão de variâncias (Médio/Uni):", round(stats_medio[4]/stats_uni[4], 2), "\n")

# Visualização com histogramas comparativos
par(mfrow = c(1, 2))  # Divide a área de plotagem em 1 linha e 2 colunas

# Histograma para ensino médio
hist(tempo_medio, breaks = seq(300, 1200, by = 100),
     main = "Ensino Médio (n=400)",
     xlab = "Minutos por semana",
     ylab = "Frequência",
     col = "lightblue",
     xlim = c(300, 1200),
     ylim = c(0, 80))

# Histograma para universitários
hist(universitarios, breaks = seq(300, 1200, by = 100),
     main = "Universitários (n=30)",
     xlab = "Minutos por semana",
     ylab = "Frequência",
     col = "lightgreen",
     xlim = c(300, 1200),
     ylim = c(0, 80))

par(mfrow = c(1, 1))  # Retorna ao layout normal

# Boxplot comparativo
boxplot(list(Ensino_Médio = tempo_medio, Universitários = universitarios),
        main = "Comparação do Tempo Assistindo TV",
        ylab = "Minutos por semana",
        col = c("lightblue", "lightgreen"))