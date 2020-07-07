rm(list = ls())
## 1. Loading and cleaning
# a
library(tidyverse)
ca_pa <- read.csv("data/calif_penn_2011.csv", header = TRUE, sep = ",")
#ca_pa <- read_csv("data/calif_penn_2011.csv")
# b
rows <- dim(ca_pa)[1]
columns <- dim(ca_pa)[2]
c(rows, columns)
# c
colSums(apply(ca_pa,c(1,2),is.na))
# d
ca_pa <- na.omit(ca_pa)
# e
rows - dim(ca_pa)[1]
# f
sum(colSums(apply(ca_pa,c(1,2),is.na)))

## 2
# a
plot(ca_pa$Median_house_value, ca_pa$Built_2005_or_later,
     xlab = "median house prices",
     ylab = "percentage of houses built since 2005")
# b
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 6],
     ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6],
     xlab = "median house prices",
     ylab = "percentage of houses built since 2005",
     main = "Houses in California")
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 42],
     ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42],
     xlab = "median house prices",
     ylab = "percentage of houses built since 2005",
     main = "Houses in Pennsylvania")

## 3
# a
Vacancy_rate <- ca_pa$Vacant_units / ca_pa$Total_units
ca_pa <- data.frame(ca_pa, Vacancy_rate)
max(Vacancy_rate)
min(Vacancy_rate)
mean(Vacancy_rate)
median(Vacancy_rate)
# b
plot(ca_pa$Median_house_value, ca_pa$Vacancy_rate,
     xlab = "median house prices", ylab = "vacancy rate")
# c
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 6],
     ca_pa$Vacancy_rate[ca_pa$STATEFP == 6],
     xlab = "median house prices", ylab = "vacancy rate",
     main = "Houses in California")
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 42],
     ca_pa$Vacancy_rate[ca_pa$STATEFP == 42],
     xlab = "median house prices", ylab = "vacancy rate",
     main = "Houses in Pennsylvania")

## 4
# a
acca <- c()
for (tract in 1:nrow(ca_pa)) {
  if (ca_pa$STATEFP[tract] == 6) {
    if (ca_pa$COUNTYFP[tract] == 1) {
      acca <- c(acca, tract)
    }
  }
}
accamhv <- c()
for (tract in acca) {
  accamhv <- c(accamhv, ca_pa[tract,10])
}
median(accamhv)
# b
median(ca_pa$Median_house_value[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1])
# c
# Alameda
mean(ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1])
# Santa Clara
mean(ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85])
# Allegheny
mean(ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3])
# d
# (i) the whole data
cor(ca_pa$Median_house_value, ca_pa$Built_2005_or_later)
# (ii) all of California
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6])
# (iii) all of Pennsylvania
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 42],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42])
# (iv) Alameda
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1])
# (v) Santa Clara
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85])
# (vi) Allegheny
cor(ca_pa$Median_house_value[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3],
    ca_pa$Built_2005_or_later[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3])
# e
#Alameda
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1],
     ca_pa$Median_household_income[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 1],
     xlab = "median house values", ylab = "median income",
     main = "Houses in Alameda")
# Santa Clara
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85],
     ca_pa$Median_household_income[ca_pa$STATEFP == 6 & ca_pa$COUNTYFP == 85],
     xlab = "median house values", ylab = "median income",
     main = "Houses in Santa Clara")
# Allegheny
plot(ca_pa$Median_house_value[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3],
     ca_pa$Median_household_income[ca_pa$STATEFP == 42 & ca_pa$COUNTYFP == 3],
     xlab = "median house values", ylab = "median income",
     main = "Houses in Allegheny")