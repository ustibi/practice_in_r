rm(list = ls())

library(tidyverse)
library(corrplot)
train <- read_csv("final/train.csv")
test <- read_csv("final/test.csv")

str(train)

# 保留数值项
numeric_col <- names(train[, sapply(train, is.numeric) == TRUE])
train_values <- train %>% 
  select(all_of(numeric_col))

# 清除NA
train_values$num_na = apply(is.na(train_values), 1, sum)
train_values <- train_values %>% 
  filter(num_na == 0) %>% 
  select(-num_na)

train %>% ggplot(aes(x = SalePrice)) + 
  geom_histogram(bins = 100, alpha = 0.8)

# 考察协方差
correlations <- cor(train_values %>% select(-Id))
cor_sorted <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
corrplot.mixed(correlations[cor_high, cor_high], tl.col="black", tl.pos = "lt")

# 用协方差较大的两项做预测
linear.model.1 <- lm(SalePrice ~ OverallQual, data = train_values)
summary(linear.model.1)

par(mfrow = c(2,2))
plot(linear.model.1)

linear.model.2 <- lm(SalePrice ~ GrLivArea, data = train_values)
summary(linear.model.2)

par(mfrow = c(2,2))
plot(linear.model.2)

# 同时考察两项
linear.model.3 <- lm(SalePrice ~ OverallQual + GrLivArea, data = train_values)
summary(linear.model.3)

par(mfrow = c(2,2))
plot(linear.model.3)

# 全部
linear.model.4 <- lm(SalePrice ~ . -Id, data = train_values)
summary(linear.model.4)

par(mfrow = c(2,2))
plot(linear.model.4)


pre <- predict(linear.model.3, test, interval = "prediction", level = 0.95)

sub <- data.frame(Id = test$Id, SalePrice = pre[, 1])

predict(linear.model.4, test, interval="prediction", level=0.95)


# 预测

# coef <- linear.model.4$coefficients
# valid_coef <- names(coef[!is.na(coef)])[-1]
# coef <- coef[valid_coef]
# #valid_coef[1] <- "Intercept"
# valid_coef[12] <- "1stFlrSF"
# valid_coef[13] <- "2ndFlrSF"
# valid_coef[29] <- "3SsnPorch"
# 
# train_values <- train_values %>% 
#   select(all_of(c(valid_coef, "SalePrice"))) %>% 
#   mutate(predict = apply(coef * train_values, 1, sum))
# 
# cor(train_values$predict, train_values$SalePrice)
# 
# test_values <- test %>% 
#   select(all_of(valid_coef))
# 
# test_values[is.na(test_values)] <- 0
# 
# test_values$SalePrice <- apply(coef * test_values, 1, sum)

coef <- linear.model.3$coefficients
submission2 <- test %>% 
  mutate(predict = test$OverallQual*coef[2] + test$GrLivArea*coef[3]) %>% 
  select(Id, predict)

train <- train %>% 
  mutate(predict = train$OverallQual*coef[2] + train$GrLivArea*coef[3])

evaluate <- function(x, y) {
  mse <- sum((x-y)^2)/length(x)
  cor <- cor(x, y)
  return(data.frame(mse, cor))
}

evaluate(train$SalePrice, train$predict)

write.csv(test, file = "final/submission1.csv")

submission1 <- data.frame(test$Id, test_values$SalePrice)
write.csv(submission1, file = "final/submission1.csv")