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