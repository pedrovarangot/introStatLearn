library(MASS)
attach(Smarket)
train = Smarket$Year < 2005
Smarket.2005 = Smarket[!train,]
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
Direction.2005 = Direction[!train]
table(lda.pred$class, Direction.2005)
mean(lda.pred$class == Direction.2005)
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)
lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.pred = predict(qda.fit, Smarket.2005)
table(qda.pred$class, Direction.2005)
mean(qda.pred$class == Direction.2005)
