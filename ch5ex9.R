library(ISLR)
library(MASS)

summary(Boston)

mean_hat <- mean(Boston$medv)
mean_hat
mean_hat/sqrt(length(Boston$medv))

meanv <- function(data, index) {
  mean(data[index])
}

library(boot)
boot.meanhat <- boot(data = Boston$medv, statistic = meanv, R = 10)
boot.meanhat
c(22.53281 - 2*0.3650401, 22.53281 + 2*0.3650401)
t.test(Boston$medv)

median_hat <- median(Boston$medv)
median_hat

medianv <- function(data, index) {
  median(data[index])
}
boot.medianhat <- boot(Boston$medv, medianv, 10)
boot.medianhat

?quantile
quantile(Boston$medv, c(.1))
quantilev <- function(data, index) {
  quantile(Boston$medv[index], c(.1))
}
boot.10thdecilehat <- boot(Boston$medv, quantilev, 10)
boot.10thdecilehat
