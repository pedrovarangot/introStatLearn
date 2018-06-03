library(ISLR)
summary(Auto)
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg >= median(Auto$mpg)] <- 1
Auto$mpg01 <- mpg01
pairs(Auto)
boxplot(Auto$weight~Auto$mpg01)
boxplot(Auto$horsepower~Auto$mpg01)
boxplot(Auto$displacement~Auto$mpg01)

set.seed(1)
train = sample(c(T,F), length(Auto), replace = T)
library(MASS)
lda.fit = lda(mpg01~cylinders+weight+horsepower+displacement,
              data = Auto, subset = train)
lda.pred = predict(lda.fit, Auto[!train,], type = "response")
table(lda.pred$class, Auto[!train,]$mpg01)
mean(lda.pred$class != mpg01[!train])

qda.fit = qda(mpg01~weight+horsepower+displacement,
              data = Auto, subset = train)
qda.pred = predict(qda.fit, Auto[!train,], type = "response")
table(qda.pred$class, Auto[!train,]$mpg01)
mean(qda.pred$class != Auto[!train,]$mpg01)

glm.fit = glm(mpg01~weight+horsepower+displacement,
              data = Auto, subset = train, family = binomial)
glm.pred = predict(glm.fit, Auto[!train,], type = "response")
glm.predclass <- rep(0, length(glm.pred))
glm.predclass[glm.pred > .5] = 1
table(glm.predclass, Auto[!train,]$mpg01)
mean(glm.predclass != Auto[!train,]$mpg01)

library(class)
train.X = scale(as.matrix(Auto[train,3:5]))
train.Y = as.matrix(Auto[train,]$mpg01)
test.X = scale(as.matrix(Auto[!train,3:5]))
test.Y = as.matrix(Auto[!train,]$mpg01)
knn.pred = knn(train = train.X, test = test.X, cl = train.Y, k = 10)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
