# Criando os dados
empresa <- 1:15
meses <- c(8, 9, 4, 5, 3, 6, 8, 6, 6, 8, 5, 5, 6, 4, 4)
setor <- c("C", "C", "I", "I", "I", "C", "C", "I", "I", "C", "C", "I", "C", "I", "I")
tamanho <- c("G", "M", "G", "M", "M", "P", "G", "M", "P", "M", "P", "P", "M", "M", "G")

dados <- data.frame(empresa, meses, setor, tamanho)

# (a) Comparação entre setores (comércio e indústria)
cat("\n(a) Comparação entre setores:\n")

# Separando os dados por setor
comercio <- subset(dados, setor == "C")
industria <- subset(dados, setor == "I")

# Calculando média e mediana para cada setor
media_C <- mean(comercio$meses)
mediana_C <- median(comercio$meses)

media_I <- mean(industria$meses)
mediana_I <- median(industria$meses)

cat("Comércio (C):\n")
cat("Média:", media_C, "\n")
cat("Mediana:", mediana_C, "\n\n")

cat("Indústria (I):\n")
cat("Média:", media_I, "\n")
cat("Mediana:", mediana_I, "\n\n")

# (b) Desvio padrão e variância por setor
cat("(b) Medidas de dispersão por setor:\n")

dp_C <- sd(comercio$meses)
var_C <- var(comercio$meses)

dp_I <- sd(industria$meses)
var_I <- var(industria$meses)

cat("Comércio (C):\n")
cat("Desvio Padrão:", dp_C, "\n")
cat("Variância:", var_C, "\n\n")

cat("Indústria (I):\n")
cat("Desvio Padrão:", dp_I, "\n")
cat("Variância:", var_I, "\n\n")

# Verificando qual grupo é mais homogêneo
if(dp_C < dp_I) {
  cat("O grupo do Comércio (C) é mais homogêneo (menor desvio padrão).\n\n")
} else {
  cat("O grupo da Indústria (I) é mais homogêneo (menor desvio padrão).\n\n")
}

# (c) Análise por tamanho da empresa
cat("(c) Análise por tamanho da empresa:\n")

# Separando por tamanho
pequenas <- subset(dados, tamanho == "P")
medias <- subset(dados, tamanho == "M")
grandes <- subset(dados, tamanho == "G")

# Função para calcular e mostrar estatísticas
calcular_estatisticas <- function(df, nome) {
  media <- mean(df$meses)
  mediana <- median(df$meses)
  dp <- sd(df$meses)
  variancia <- var(df$meses)
  
  cat(nome, ":\n")
  cat("Média:", media, "\n")
  cat("Mediana:", mediana, "\n")
  cat("Desvio Padrão:", dp, "\n")
  cat("Variância:", variancia, "\n\n")
  
  return(c(media, mediana, dp, variancia))
}

# Calculando para cada tamanho
estat_P <- calcular_estatisticas(pequenas, "Pequenas (P)")
estat_M <- calcular_estatisticas(medias, "Médias (M)")
estat_G <- calcular_estatisticas(grandes, "Grandes (G)")

# Comparando as médias
cat("Comparação das médias por tamanho:\n")
cat("Pequenas:", estat_P[1], "\n")
cat("Médias:", estat_M[1], "\n")
cat("Grandes:", estat_G[1], "\n\n")

# Verificando relação entre tamanho e meses de crescimento
if(estat_G[1] > estat_M[1] && estat_M[1] > estat_P[1]) {
  cat("Parece existir uma relação positiva: quanto maior a empresa, mais meses com crescimento.\n")
} else if(estat_P[1] > estat_M[1] && estat_M[1] > estat_G[1]) {
  cat("Parece existir uma relação negativa: quanto menor a empresa, mais meses com crescimento.\n")
} else {
  cat("Não parece haver uma relação clara entre tamanho da empresa e meses com crescimento.\n")
}