library(e1071)
library(MASS)

set.seed(1)

x0 <- mvrnorm(50, rep(0,10), diag(10))
x1 <- mvrnorm(50, c(rep(1,5), rep(0,5)), diag(10))
tst <- data.frame(x0, y = as.factor(rep(0, nrow(x0))))
tst <- rbind(tst, data.frame(x1, y = as.factor(rep(1, nrow(x1)))))

loops <- 30
success <- double(loops)

for (t in 1:loops) {
  x0 <- mvrnorm(100, rep(0,10), diag(10))
  x1 <- mvrnorm(100, c(rep(1,5), rep(0,5)), diag(10))
  dat <- data.frame(x0, y = as.factor(rep(0, nrow(x0))))
  dat <- rbind(dat, data.frame(x1, y = as.factor(rep(1, nrow(x1)))))
  svm.fit <- svm(y~., data = dat)
  svm.pred <- predict(svm.fit, tst)
  success[t] <- (table(svm.pred, tst$y)[1,1] + table(svm.pred, tst$y)[2,2]) / 100
}

print(1 - mean(success))

for (t in 1:loops) {
  x0 <- mvrnorm(100, rep(0,10), diag(10))
  x1 <- mvrnorm(100, c(rep(1,5), rep(0,5)), diag(10))
  dat <- data.frame(x0, y = as.factor(rep(0, nrow(x0))))
  dat <- rbind(dat, data.frame(x1, y = as.factor(rep(1, nrow(x1)))))
  svm.fit <- svm(y~., data = dat, kernel = "linear")
  svm.pred <- predict(svm.fit, tst)
  success[t] <- (table(svm.pred, tst$y)[1,1] + table(svm.pred, tst$y)[2,2]) / 100
}

print(1 - mean(success))

for (t in 1:loops) {
  x0 <- mvrnorm(100, rep(0,10), diag(10))
  x1 <- mvrnorm(100, c(rep(1,5), rep(0,5)), diag(10))
  dat <- data.frame(x0, y = as.factor(rep(0, nrow(x0))))
  dat <- rbind(dat, data.frame(x1, y = as.factor(rep(1, nrow(x1)))))
  glm.fit <- glm(y~., data = dat, family = "binomial")
  glm.pred <- as.integer(predict(glm.fit, tst, type = "response") > .5)
  success[t] <- (table(glm.pred, tst$y)[1,1] + table(glm.pred, tst$y)[2,2]) / 100
}

print(1 - mean(success))
