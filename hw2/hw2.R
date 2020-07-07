rm(list = ls())
## 1. Loading and cleaning
# a
library(tidyverse)
#ca_pa <- read.csv("data/calif_penn_2011.csv", header = TRUE, sep = ",")
ca_pa <- read_csv("data/calif_penn_2011.csv")
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
