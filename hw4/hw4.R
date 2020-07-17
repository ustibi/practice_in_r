rm(list = ls())
library(tidyverse)
ckm_nodes <- read_csv("data/ckm_nodes.csv")
ckm_network <- read_table("data/ckm_network.dat", col_names = FALSE)
# 1
valid_nodes <- !is.na(ckm_nodes$adoption_date)
ckm_network <- ckm_network[valid_nodes, valid_nodes]
ckm_nodes <- ckm_nodes[valid_nodes, ]

# 2
doc.info <- data.frame("doctor" = rep(seq(1, 125), each = 17),
                       "month" = rep(seq(1, 17), time = 125))
doc.info <- doc.info %>% 
  mutate(begin = ckm_nodes$adoption_date[doc.info$doctor] == doc.info$month)%>%
  mutate(before = ckm_nodes$adoption_date[doc.info$doctor] < doc.info$month)
contact <- ckm_network[rep(1:125, each = 17), ]
m1 <- matrix(ckm_nodes$adoption_date[doc.info$doctor] < doc.info$month,
             nrow = 17)
m2 <- matrix(ckm_nodes$adoption_date[doc.info$doctor] <= doc.info$month,
             nrow = 17)
doc.info <- doc.info %>% 
  mutate(num_strict_before = rowSums(m1[rep(seq(1,17), time = 125), ] &
                                       contact)) %>% 
  mutate(num_begin_before = rowSums(m2[rep(seq(1,17), time = 125), ] &
                                      contact))

# max.k <- 0
# for (person in 1:125) {
#   contacts <- which(ckm_network[person, ] == TRUE)
#   max.k <- max(max.k, length(contacts))
#   for (month in 1:17) {
#     doc.info$num_strict_before[17*(person-1)+month] <-
#       sum(ckm_nodes$adoption_date[contacts] < month)
#     doc.info$num_begin_before[17*(person-1)+month] <-
#       sum(ckm_nodes$adoption_date[contacts] <= month)
#   }
# }

# 3
# a
max(apply(ckm_network, 1, sum))
s <- apply(ckm_network, 1, sum)
summary(s)
# b
pk <- qk <- c()
for (k in 0:20) {
  obs <- doc.info %>% filter(num_strict_before == k)
  pk[k+1] <- sum(obs$begin) / dim(obs)[1]
  obs <- doc.info %>%
    filter(num_begin_before - num_strict_before == k)
  qk[k+1] <- sum(obs$begin) / dim(obs)[1]
}
# c
prop <- data.frame(k = 0:20, pk, qk)
prop %>% 
  ggplot(aes(x = k, y = pk)) +
  geom_point() + 
  labs(x = "the number of prior-adoptee contacts k",
       y = "the probabilities pk", 
       title = "pk ~ k") +
  theme_bw()
prop %>% 
  ggplot(aes(x = k, y = qk)) +
  geom_point() + 
  labs(x = "the number of prior-or-contemporary-adoptee contacts k",
       y = "the probabilities qk", 
       title = "qk ~ k") +
  theme_bw()

# 4
# a
prop <- prop %>% 
  filter(!is.na(pk)) %>% 
  select(-qk)
mse1 <- function(parameters, x = prop$k, y = prop$pk) {
  a <- parameters[1]
  b <- parameters[2]
  y.estimate = a + b * x
  return(sum((y - y.estimate) ^ 2) / length(x))
}
par1 <- nlm(mse1, c(0, 0))
par1$estimate[1]
par1$estimate[2]
# b
mse2 <- function(parameters, x = prop$k, y = prop$pk) {
  a <- parameters[1]
  b <- parameters[2]
  y.estimate = exp(a + b * x) / (1 + exp(a + b * x))
  return(sum((y - y.estimate) ^ 2) / length(x))
}
par2 <- nlm(mse2, c(0, 0))
par2$estimate[1]
par2$estimate[2]
# c
a <- par1$estimate[1]
b <- par1$estimate[2]
prop1 <- data.frame(k = rep(prop$k, time = 2),
                    pk = c(prop$pk, a+b*prop$k),
                    group = rep(c("original", "estimate"),
                                each=length(prop$k)))
prop1 %>%
  ggplot(aes(x = k, y = pk)) +
  geom_point(aes(colour = group)) +
  labs(x = "the number of prior-adoptee contacts k",
       y = "the probabilities pk",
       title = "pk ~ k") +
  theme_bw()

a <- par2$estimate[1]
b <- par2$estimate[2]
prop2 <- data.frame(k = rep(prop$k, time = 2),
                    pk = c(prop$pk, exp(a+b*prop$k)/(1+exp(a+b*prop$k))),
                    group = rep(c("original", "estimate"),
                                each=length(prop$k)))
prop2 %>%
  ggplot(aes(x = k, y = pk)) +
  geom_point(aes(colour = group)) +
  labs(x = "the number of prior-adoptee contacts k",
       y = "the probabilities pk",
       title = "pk ~ k") +
  theme_bw()

par1$minimum
par2$minimum

# Extra
pk <- Vk <- c()
for (k in 0:20) {
  obs <- doc.info %>% filter(num_strict_before == k)
  pk[k+1] <- sum(obs$begin) / dim(obs)[1]
  Vk[k+1] <- pk[k+1] * (1 - pk[k+1]) / dim(obs)[1]
}
prop <- data.frame(k = 0:20, pk, Vk)
prop <- prop %>% 
  filter(!is.na(pk)) %>% 
  filter(Vk != 0)

mse3 <- function(parameters, x = prop$k, y = prop$pk, V = prop$Vk) {
  a <- parameters[1]
  b <- parameters[2]
  y.estimate = a + b * x
  return(sum((y - y.estimate) ^ 2 / V) / length(x))
}
par3 <- nlm(mse3, c(0, 0))
a <- par3$estimate[1]
b <- par3$estimate[2]

prop3 <- data.frame(k = rep(prop$k, time = 2),
                    pk = c(prop$pk, a+b*prop$k),
                    group = rep(c("original", "estimate"),
                                each=length(prop$k)))
prop3 %>%
  ggplot(aes(x = k, y = pk)) +
  geom_point(aes(colour = group)) +
  labs(x = "the number of prior-adoptee contacts k",
       y = "the probabilities pk",
       title = "pk ~ k") +
  theme_bw()


mse4 <- function(parameters, x = prop$k, y = prop$pk, V = prop$Vk) {
  a <- parameters[1]
  b <- parameters[2]
  y.estimate = exp(a + b * x) / (1 + exp(a + b * x))
  return(sum((y - y.estimate) ^ 2 / V) / length(x))
}
par4 <- nlm(mse4, c(0, 0))
a <- par4$estimate[1]
b <- par4$estimate[2]

prop4 <- data.frame(k = rep(prop$k, time = 2),
                    pk = c(prop$pk, exp(a+b*prop$k)/(1+exp(a+b*prop$k))),
                    group = rep(c("original", "estimate"),
                                each=length(prop$k)))
prop4 %>%
  ggplot(aes(x = k, y = pk)) +
  geom_point(aes(colour = group)) +
  labs(x = "the number of prior-adoptee contacts k",
       y = "the probabilities pk",
       title = "pk ~ k") +
  theme_bw()
