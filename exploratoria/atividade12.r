#(a)
prob_12a <- 1 - pnorm(13, mean = 10, sd = 2)
cat(sprintf("Exercício 12(a): P(X > 13) = %.4f\n", prob_12a))

# (b)
prob_12b <- pnorm(11, mean = 10, sd = 2) - pnorm(9, mean = 10, sd = 2)
cat(sprintf("Exercício 12(b): P(9 ≤ X ≤ 11) = %.4f\n", prob_12b))
