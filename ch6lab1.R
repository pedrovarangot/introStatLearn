library(ISLR)
?Hitters
View(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)

library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full <- regsubsets(Salary~., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq

par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

plot(regfit.full, scale = "bic")
par(mfrow = c(1,1))
coef(regfit.full, 6)

regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19,
                         method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19,
                         method = "backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

set.seed(1)
train <- sample(c(T,F), nrow(Hitters), rep=T)
train
test <- (!train)
test

regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)
?model.matrix
test.mat <- model.matrix(Salary~., data = Hitters[test,])
val.errors <- rep(NA, 19)
for(i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred) ^ 2)
}
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

mean((predict(regfit.best, Hitters[test,], 10) - Hitters[test,]$Salary)^2)
val.errors

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(j in 1:k) {
  best.fit <- regsubsets(Salary~., data = Hitters[folds != j,],
                         nvmax = 19)
  for(i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j,], id = i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds == j]- pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)
