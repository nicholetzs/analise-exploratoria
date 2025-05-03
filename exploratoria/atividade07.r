# Dados das rendas mensais (em mil reais)
rendas <- c(2.90, 2.90, 2.95, 2.95, 3.10, 3.10, 3.15, 3.20, 3.20, 3.25, 
            3.30, 3.40, 3.45, 3.45, 3.50, 3.65, 3.65, 3.80, 3.90, 3.90, 
            4.00, 5.00, 5.20, 5.50, 6.40)

# Cálculo das estatísticas descritivas
summary_stats <- summary(rendas)
q1 <- quantile(rendas, 0.25)
q3 <- quantile(rendas, 0.75)
iqr <- IQR(rendas)

cat("Estatísticas Descritivas:\n")
cat(sprintf("Mínimo: %.2f\n", summary_stats[1]))
cat(sprintf("1º Quartil (Q1): %.2f\n", q1))
cat(sprintf("Mediana: %.2f\n", summary_stats[3]))
cat(sprintf("Média: %.2f\n", summary_stats[4]))
cat(sprintf("3º Quartil (Q3): %.2f\n", q3))
cat(sprintf("Máximo: %.2f\n", summary_stats[6]))
cat(sprintf("Intervalo Interquartil (IQR): %.2f\n", iqr))

# Identificação de outliers
limite_inferior <- q1 - 1.5 * iqr
limite_superior <- q3 + 1.5 * iqr
outliers <- rendas[rendas < limite_inferior | rendas > limite_superior]

cat("\nLimites para outliers:\n")
cat(sprintf("Limite inferior: %.2f\n", limite_inferior))
cat(sprintf("Limite superior: %.2f\n", limite_superior))
cat("Outliers identificados:", outliers, "\n")

# Construção do boxplot
boxplot(rendas, 
        horizontal = TRUE,
        main = "Distribuição das Rendas Mensais dos Ingressantes",
        xlab = "Renda Mensal (mil reais)",
        col = "lightblue",
        notch = FALSE,
        outline = TRUE)

# Adicionando pontos da média e outliers
points(mean(rendas), 1, pch = 18, col = "red", cex = 2)
text(mean(rendas), 1.2, "Média", col = "red")

# Adicionando linhas de referência
abline(v = c(q1, summary_stats[3], q3), 
       col = c("blue", "green", "blue"), 
       lty = 2)

# Legenda
legend("topright", 
       legend = c("Mediana", "Quartis Q1/Q3"), 
       col = c("green", "blue"), 
       lty = 2, 
       cex = 0.8)