rm(list = ls())
gmp <- read.table("data/gmp.dat")
gmp$pop <- round(gmp$gmp / gmp$pcgmp)
# 1
plot(gmp$pcgmp ~ gmp$pop, log = "x",
     xlab = "population", ylab = "per capita GMP",
     main = "US Metropolitan Areas, 2006")
curve(6611*x^(1/8), add = TRUE, col = "blue")
curve(6611*x^(0.1), add = TRUE, col = "red")
curve(6611*x^(0.15), add = TRUE, col = "green")

# 2
mse <- function(x, N = gmp$pop, Y = gmp$pcgmp) {
  return(sum((Y - x[1]*N^x[2]) ^ 2) / length(N))
}
mse(c(6611, 0.15))
mse(c(5000, 0.10))

# 4
nlm(mse, c(y0=6611,a=1/8))
nlm(mse, c(y0=6611,a=0.1))
nlm(mse, c(y0=6611,a=0.15))

# 5
plm <- function(y0, a, N = gmp$pop, Y = gmp$pcgmp) {
  res <- nlm(mse, c(y0, a), N, Y)
  return(c(res$estimate[1], res$estimate[2], res$minimum))
}
plm(6611, 0.15)
plm(5000, 0.10)

# 6
# a
# built-in function
n <- length(gmp$pcgmp)
mean(gmp$pcgmp)
sd(gmp$pcgmp) / sqrt(n)
# b
mean_except <- function(i) {
  return(mean(gmp$pcgmp[-i]))
}
# c
jackknifed.means <- c()
for (city in 1:n) {
  jackknifed.means <- c(jackknifed.means, mean_except(city))
}
# d
sqrt(((n-1)^2/n) * var(jackknifed.means))