# Dados do Exercício 3
dados <- data.frame(
  S = c(4.5, 5.0, 4.2, 3.7, 3.9, 4.1, 2.9, 2.7, 3.5, 3.2, 4.7, 3.4, 4.8, 4.2, 3.4, 4.4, 3.7, 5.5, 2.8, 2.5, 2.9),
  T = c("M", "T", "M", "M", "T", "T", "T", "T", "T", "T", "M", "T", "M", "M", "M", "M", "M", "M", "T", "T", "T"),
  Z = c("N", "M", "M", "M", "T", "N", "M", "M", "T", "N", "N", "M", "T", "M", "N", "T", "T", "T", "M", "M", "T")
)

# (a) Análise por tipo de indústria (M vs T)
cat("(a) Análise por Tipo de Indústria\n")

# Separando os dados
moderna <- subset(dados, T == "M")
tradicional <- subset(dados, T == "T")

# Função para calcular estatísticas
calcular_stats <- function(df, nome) {
  media <- mean(df$S)
  mediana <- median(df$S)
  variancia <- var(df$S)
  
  cat("\n", nome, ":\n", sep = "")
  cat("Média:", round(media, 2), "mil reais\n")
  cat("Mediana:", mediana, "mil reais\n")
  cat("Variância:", round(variancia, 2), "\n")
  
  return(c(media, mediana, variancia))
}

# Calculando para cada tipo
stats_M <- calcular_stats(moderna, "Indústria Moderna (M)")
stats_T <- calcular_stats(tradicional, "Indústria Tradicional (T)")

# Comparação
cat("\nComparação:\n")
cat("Diferença de médias (M - T):", round(stats_M[1] - stats_T[1], 2), "mil reais\n")
cat("Razão de variâncias (M/T):", round(stats_M[3]/stats_T[3], 2), "\n")

# (b) Análise por período de trabalho (M, T, N)
cat("\n\n(b) Análise por Período de Trabalho\n")

# Separando os dados
manha <- subset(dados, Z == "M")
tarde <- subset(dados, Z == "T")
noite <- subset(dados, Z == "N")

# Calculando para cada período
stats_manha <- calcular_stats(manha, "Período da Manhã (M)")
stats_tarde <- calcular_stats(tarde, "Período da Tarde (T)")
stats_noite <- calcular_stats(noite, "Período da Noite (N)")

# Criando tabela comparativa
tabela_comparacao <- data.frame(
  Período = c("Manhã", "Tarde", "Noite"),
  Média = c(stats_manha[1], stats_tarde[1], stats_noite[1]),
  Mediana = c(stats_manha[2], stats_tarde[2], stats_noite[2]),
  Variância = c(stats_manha[3], stats_tarde[3], stats_noite[3])
)

cat("\nTabela Comparativa:\n")
print(tabela_comparacao)

# Visualização gráfica
boxplot(S ~ Z, data = dados, 
        main = "Renda por Período de Trabalho",
        xlab = "Período",
        ylab = "Renda (mil reais)",
        col = c("lightblue", "lightgreen", "pink"))