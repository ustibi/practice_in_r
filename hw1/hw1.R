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

