# Dados do exercício
dados <- data.frame(
  Estado = c("Amazonas", "Pará", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte",
             "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais",
             "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina",
             "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"),
  V = c(333, 2655, 71, 882, 8874, 5989, 1469, 9134, 924, 951,
        2234, 17089, 2653, 39503, 172229, 4364, 34335, 64851, 59, 32, 1196, 99),
  P = c(527, 2035, 271, 1290, 13776, 9816, 2499, 12720, 1031, 961,
        4154, 30002, 4402, 49256, 195756, 7619, 28949, 91813, 222, 83, 2415, 239)
)

# Cálculo dos logaritmos decimais
dados$logV <- log10(dados$V)
dados$logP <- log10(dados$P)

# (a) Gráfico de dispersão log(V) vs log(P)
plot(dados$logP, dados$logV,
     xlab = "log10(P) - Pessoas Ocupadas (log)",
     ylab = "log10(V) - Produção Industrial (log)",
     main = "Dispersão entre log(P) e log(V)",
     pch = 19, col = "steelblue")

# Adiciona linha de tendência
abline(lm(logV ~ logP, data = dados), col = "red", lwd = 2)

# (b) Coeficiente de correlação de Pearson
correlacao <- cor(dados$logV, dados$logP, method = "pearson")
cat(sprintf("\n📊 Coeficiente de Correlação de Pearson: %.4f\n", correlacao))
