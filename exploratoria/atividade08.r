# Instala e carrega o pacote necessário
if (!require("modeest")) install.packages("modeest", dependencies = TRUE)
library(modeest)

# Criação dos dados
dados <- data.frame(
  Secao = c("Pessoal", "Pessoal", "Pessoal", "Pessoal", "Pessoal", "Pessoal", "Pessoal",
            "Técnica", "Técnica", "Técnica", "Técnica", "Técnica", "Técnica", "Técnica",
            "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas", "Vendas"),
  Direito = rep(9, 25),
  Politica = c(9.0, 6.5, 9.0, 6.0, 6.5, 6.5, 9.0,
               6.0, 9.0, 9.0, 7.0, 5.5, 6.0, 8.0,
               7.0, 9.0, 10.0, 5.5, 7.0, 6.0, 6.5, 6.0, 9.0, 6.5, 7.0),
  Estatistica = c(9, 9, 8, 8, 9, 10, 8,
                  8, 9, 8, 10, 7, 7, 9,
                  8, 7, 8, 9, 2, 7, 7, 8, 9, 8, 7)
)

# Função para estatísticas
estatisticas <- function(x) {
  media <- mean(x)
  moda <- mfv(x)
  mediana <- median(x)
  desvio <- sd(x)
  cv <- sd(x) / mean(x) * 100
  return(c(Media = media, Moda = moda, Mediana = mediana, Desvio_Padrao = desvio, Coef_Variacao = cv))
}

# Aplicação das estatísticas
estatisticas_direito <- estatisticas(dados$Direito)
estatisticas_politica <- estatisticas(dados$Politica)
estatisticas_estatistica <- estatisticas(dados$Estatistica)

# Exibição dos resultados
cat("\n📚 Estatísticas - Direito:\n"); print(estatisticas_direito)
cat("\n📚 Estatísticas - Política:\n"); print(estatisticas_politica)
cat("\n📚 Estatísticas - Estatística:\n"); print(estatisticas_estatistica)

# Boxplot comparativo das disciplinas
boxplot(dados$Direito, dados$Politica, dados$Estatistica,
        names = c("Direito", "Política", "Estatística"),
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Boxplot Comparativo por Disciplina",
        ylab = "Notas")

# Boxplot da disciplina Estatística por Seção
boxplot(Estatistica ~ Secao, data = dados,
        col = c("tomato", "gold", "skyblue"),
        main = "Notas em Estatística por Seção",
        ylab = "Nota")
