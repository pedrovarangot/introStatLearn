library(MASS)
library(ISLR)
library(class)

attach(Smarket)

train = Year < 2005
Direction.2005 = Direction[!train]

train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
?Caravan
standarized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standarized.X[,1])
var(standarized.X[,2])
test = 1:1000
train.X = standarized.X[-test,]
test.X = standarized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

glm.fits = glm(Purchase~., data=Caravan, family = binomial,subset = -test)
glm.probs = predict(glm.fits, Caravan[test,], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
