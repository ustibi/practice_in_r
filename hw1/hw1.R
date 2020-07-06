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
vector2[2] + vector2[3]

dataframe3 <- data.frame(z1="5",z2=7,z3=12)
dataframe3[1,2] + dataframe3[1,3]

list4 <- list(z1="6", z2=42, z3="49", z4=126)
list4[[2]]+list4[[4]]
list4[2]+list4[4]