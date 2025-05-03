# (a)
prob_13a <- pnorm(170000, mean = 150000, sd = 5000)
cat(sprintf("Exercício 13(a): P(X < 170000) = %.4f\n", prob_13a))


# (b)
prob_13b <- pnorm(165000, mean = 150000, sd = 5000) - pnorm(140000, mean = 150000, sd = 5000)
cat(sprintf("Exercício 13(b): P(140000 ≤ X ≤ 165000) = %.4f\n", prob_13b))
