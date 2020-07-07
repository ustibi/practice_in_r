rm(list = ls())
## 1
# a
iowa.df <- read.csv("data/iowa.csv", header=TRUE, sep = ";")
iowa.df
# b
length(row.names(iowa.df))
length(names(iowa.df))
# c
names(iowa.df)
# d
iowa.df[5, 7]
# e
iowa.df[2, ]

## 2
# a
vector1 <- c("5", "12", "7", "32")
max(vector1)
sort(vector1)
#sum(vector1) #error

# b
vector2 <- c("5",7,12)
#vector2[2] + vector2[3] #error

dataframe3 <- data.frame(z1="5",z2=7,z3=12)
dataframe3[1,2] + dataframe3[1,3]

list4 <- list(z1="6", z2=42, z3="49", z4=126)
list4[[2]]+list4[[4]]
#list4[2]+list4[4] #error

## 3
# a
seq(1, 1000, by = 372)
seq(1, 1000, length.out = 50)
# b
rep(1:3, times = 3)
rep(1:3, each = 3)

## MB.Ch1.2
#library(lattice) #RStudio can also do this step
library(DAAG)
orings <- DAAG::orings
dataframe1 <- orings[c(1, 2, 4, 11, 13), ]
plot(dataframe1[, 1], dataframe1[, 4],
     xlab = "Temperature", ylab = "Total incidents")
plot(orings[, 1], orings[, 4],
     xlab = "Temperature", ylab = "Total incidents")

## MB.Ch1.4
#library(lattice) #RStudio can also do this step
library(DAAG)
ais <- DAAG::ais
str(ais)
# determine whether any of the columns hold missing values
sum(is.na(ais))
# make a table 
table(ais$sex, ais$sport)

## MB.Ch1.6
#                elevation  area
# Winnipeg             217 24387
# Winnipegosis         254  5374
# Manitoba             248  4624
# SouthernIndian       254  2247
# Cedar                253  1353
# Island               227  1223
# Gods                 178  1151
# Cross                207   755
# Playgreen            217   657
elevation <- c(217, 254, 248, 254, 253, 227, 178, 207, 217)
area <- c(24387, 5374, 4624, 2247, 1353, 1223, 1151, 755, 657)
names <- c("Winnipeg", "Winnipegosis", "Manitoba", "SouthernIndian",
           "Cedar", "Island", "Gods", "Cross", "Playgreen")
Manitoba.lakes <- data.frame("elevation" = elevation, "area" = area)
row.names(Manitoba.lakes) <- names
# a
attach(Manitoba.lakes)
plot(log2(area) ~ elevation, pch=16, xlim=c(170,280))
# NB: Doubling the area increases log2(area) by 1.0
text(log2(area) ~ elevation, labels=row.names(Manitoba.lakes), pos=4)
text(log2(area) ~ elevation, labels=area, pos=2) 
title("Manitoba’s Largest Lakes")
# b
plot(area ~ elevation, pch=16, xlim=c(170,280), ylog=T)
text(area ~ elevation, labels=row.names(Manitoba.lakes), pos=4, ylog=T)
text(area ~ elevation, labels=area, pos=2, ylog=T) 
title("Manitoba’s Largest Lakes")

## MB.Ch1.7
# (a)
dotchart(area, xlab = "area", ylab = "lakes", labels = names)
title("(a)the areas of the Manitoba lakes on a linear scale")
# (b)
dotchart(log2(area), xlab = "log2(area)", ylab = "lakes", labels = names)
title("(b)the areas of the Manitoba lakes on a logarithmic scale")

## MB.Ch1.8
sum(area)
detach(Manitoba.lakes)
rm(list = ls())