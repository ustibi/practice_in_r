rm(list = ls())
# 1
percentile_ratio_discrepancies <- function(a, P99, P99.5, P99.9) {
  n1 <- ((P99/P99.9) ^ (1-a) - 10) ^ 2
  n2 <- ((P99.5/P99.9) ^ (1-a) - 5) ^ 2
  n3 <- ((P99/P99.5) ^ (1-a) - 2) ^ 2
  return(n1 + n2 + n3)
}
percentile_ratio_discrepancies(2, 1e6, 2e6, 1e7)

# 2
exponent.multi_ratios_est <- function(P99, P99.5, P99.9) {
  a <- 1 - log(10)/log(P99/P99.9)
  res <- nlm(percentile_ratio_discrepancies, a, P99, P99.5, P99.9)
  return(res$estimate)
}
exponent.multi_ratios_est(1e6, 2e6, 1e7)

# 3
wtid <- read.csv("data/wtid-report.csv")
wtid <- data.frame("year" = wtid$Year, "P99" = wtid$P99.income.threshold,
                   "p99.5" = wtid$P99.5.income.threshold,
                   "p99.9" = wtid$P99.9.income.threshold)
for (i in 1:dim(wtid)[1]) {
  wtid$a.est[i] <- exponent.multi_ratios_est(wtid$P99[i], wtid$p99.5[i],
                                             wtid$p99.9[i])
}
plot(wtid$a.est ~ wtid$year, xlab = "year", ylab = "estimate of a",
     main = "Estimate of a in US from 1913 to 2012")

# 4
wtid$a.est2 <- 1 - log(10)/log(wtid$P99/wtid$p99.9)
plot(wtid$a.est2 ~ wtid$a.est,
     xlab = "estimate from problem 3", ylab = "estimate from (4)",
     main = "Scatter-plot of two estimates")
curve(x^1, add = TRUE, col = "blue")