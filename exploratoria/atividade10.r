# Parâmetros
media <- 8
dp <- 1.5

# P(X > 10) = 1 - P(X ≤ 10)
prob_ex11 <- 1 - pnorm(10, mean = media, sd = dp)
cat(sprintf("Exercício 11: P(X > 10) = %.4f\n", prob_ex11))
