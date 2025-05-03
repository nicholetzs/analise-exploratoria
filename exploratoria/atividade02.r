# Dados do Exercício 2
idade <- c(40, 45, 50, 55, 60, 65)
peso <- list(
  c(55, 50, 68, 65, 62),    # 40 anos
  c(58, 56, 62, 65, 63),    # 45 anos
  c(60, 74, 70, 78, 76),    # 50 anos
  c(77, 78, 70, 72, 80),    # 55 anos
  c(70, 76, 74, 83, 85),    # 60 anos
  c(65, 82, 72, 82, 80)     # 65 anos
)

# (a) Cálculo das estatísticas para cada grupo de idade
cat("(a) Estatísticas por grupo de idade:\n")

# Função para calcular e mostrar estatísticas
calcular_estatisticas <- function(dados, idade) {
  media <- mean(dados)
  mediana <- median(dados)
  dp <- sd(dados)
  variancia <- var(dados)
  
  cat("Idade:", idade, "anos\n")
  cat("Média:", round(media, 2), "kg\n")
  cat("Mediana:", mediana, "kg\n")
  cat("Desvio Padrão:", round(dp, 2), "kg\n")
  cat("Variância:", round(variancia, 2), "kg²\n\n")
  
  return(c(media, mediana, dp, variancia))
}

# Calculando para cada grupo de idade
estatisticas <- list()
for (i in 1:length(idade)) {
  estatisticas[[i]] <- calcular_estatisticas(peso[[i]], idade[i])
}

# (b) Análise do comportamento do peso com a idade
cat("(b) Comportamento do peso com o aumento da idade:\n")

# Criando um data frame com as médias de cada idade
dados_medias <- data.frame(
  idade = idade,
  media = sapply(estatisticas, function(x) x[1])
)

# Gráfico para visualizar a relação entre idade e peso médio
plot(dados_medias$idade, dados_medias$media, 
     type = "b", 
     pch = 19, 
     col = "blue",
     xlab = "Idade (anos)", 
     ylab = "Peso Médio (kg)",
     main = "Relação entre Idade e Peso Médio")

# Regressão linear simples para avaliar a tendência
modelo <- lm(media ~ idade, data = dados_medias)
abline(modelo, col = "red")

# Coeficiente angular (indica a tendência)
coef_angular <- coef(modelo)[2]
cat("Coeficiente angular da regressão:", round(coef_angular, 2), "kg/ano\n")

# Conclusão baseada no coeficiente
if (coef_angular > 0) {
  cat("Conclusão: O peso tende a aumentar com a idade (", round(coef_angular, 2), "kg por ano).\n")
} else if (coef_angular < 0) {
  cat("Conclusão: O peso tende a diminuir com a idade (", round(coef_angular, 2), "kg por ano).\n")
} else {
  cat("Conclusão: Não há tendência clara de mudança de peso com a idade.\n")
}

# Mostrando as estatísticas completas para análise
cat("\nResumo das estatísticas por idade:\n")
print(data.frame(
  Idade = idade,
  Média = sapply(estatisticas, function(x) round(x[1], 2)),
  Mediana = sapply(estatisticas, function(x) x[2]),
  "Desvio Padrão" = sapply(estatisticas, function(x) round(x[3], 2)),
  "Variância" = sapply(estatisticas, function(x) round(x[4], 2))
), row.names = FALSE)