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

# 7
plm.jackknife <- function(y0, a, N = gmp$pop, Y = gmp$pcgmp) {
  y0.estimate <- c()
  a.estimate <- c()
  n <- length(N)
  for (city in 1:n) {
    temp_res <- plm(y0, a, N[-city], Y[-city])
    y0.estimate <- c(y0.estimate, temp_res[1])
    a.estimate <- c(y0.estimate, temp_res[2])
  }
  y0.std_err <- sqrt(((n-1)^2/n) * var(y0.estimate))
  a.std_err <- sqrt(((n-1)^2/n) * var(a.estimate))
  return(c(y0.std_err, a.std_err))
}
plm.jackknife(6611, 1/8)

# 8
# the file "gmp-2013.dat" is downloaded in
# https://www.stat.cmu.edu/~cshalizi/statcomp/14/hw/04/gmp-2013.dat
gmp.2013 <- read.table("data/gmp-2013.dat")
gmp.2013$pop <- round(gmp.2013$gmp / gmp.2013$pcgmp)
# plm(6611, 0.15)
# ## [1] 6.611000e+03 1.263182e-01 6.185706e+07
plm(6611, 1/8, gmp.2013$pop, gmp.2013$pcgmp)
# plm.jackknife(6611, 1/8)
# ## [1] 1.136653e-08 6.583823e+03
plm.jackknife(6611, 1/8, gmp.2013$pop, gmp.2013$pcgmp)

plot(gmp.2013$pcgmp ~ gmp.2013$pop, log = "x",
     xlab = "population", ylab = "per capita GMP",
     main = "US Metropolitan Areas, 2013")
curve(6.611000e+03*x^(1.263182e-01), add = TRUE, col = "blue")
curve(6.611000e+03*x^(1.433688e-01), add = TRUE, col = "red")

rm(list = ls())