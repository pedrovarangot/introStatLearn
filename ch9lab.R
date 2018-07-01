set.seed (1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
x[y==1,] <- x[y==1,] + 1
plot(x, col=y+3, pch=19)

library(e1071)

dat <- data.frame(x=x, y=as.factor(y))
svm.fit <- svm(y~., data = dat, kernel = "linear", cost = .1, scale = F)
print(svm.fit)
plot(svm.fit, dat)
svm.fit$index

set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(.001, .01, .1, 1.5, 10, 100)))
summary(tune.out)
tune.best <- tune.out$best.model
summary(tune.best)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=T)
xtest[ytest == 1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))
ypred <- predict(tune.best, testdat)
table(predict=ypred, truth=testdat$y)
svmfit <- svm(y~., data = dat, kernel = "linear", cost = .01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)


