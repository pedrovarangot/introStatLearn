library(ISLR)
attach(Weekly)
?Weekly
pairs(Weekly)
summary(Weekly)
plot(Today)

# b, logistic regression
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               family = binomial)
summary(glm.fits)
glm.fits2 = glm(Direction~Lag2,
               family = binomial)
summary(glm.fits2)

# c
glm.probs = predict(glm.fits, Weekly[2:7], type = "response")
length(glm.probs)
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction)
(557+54)/length(glm.pred)
mean(glm.pred == Direction)

train = Year < 2009
glm.fits3 = glm(Direction~Lag2, data=Weekly, 
                subset = train, family = binomial)
glm.probs3 = predict(glm.fits3, data.frame(Lag2=Lag2[!train]),
                    type = "response")
glm.pred3 = rep("Down", length(glm.probs3))
glm.pred3[glm.probs3 > .5] = "Up"
table(glm.pred3, Direction[!train])
mean(glm.pred3 == Direction[!train])

library(MASS)
lda.fit = lda(Direction~poly(Lag2+Lag3,2), data=Weekly, subset=train)
lda.pred = predict(lda.fit, data.frame(Lag2=Lag2[!train]
                                       ,Lag3=Lag3[!train]
                                       ), 
                   type="response")$class
table(lda.pred, Direction[!train])
mean(lda.pred == Direction[!train])

qda.fit = qda(Direction~Lag2, data=Weekly, subset = train)
qda.pred = predict(qda.fit, data.frame(Lag2=Lag2[!train]), type="response")
table(qda.pred$class, Direction[!train])
mean(qda.pred$class == Direction[!train])

train.X = scale(as.matrix(Weekly[train,2]))
test.X = scale(as.matrix(Weekly[!train,2]))
train.Y = Direction[train]
test.Y = Direction[!train]

library(class)
?knn
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)

var(train.X)
