library(ISLR)
names(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,1:8])
attach(Smarket)
plot(Volume)
plot(Year, Volume)

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction)

# Up to here we used the same data to train and validate
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

# Remove indicators with low p value
glm.fits = glm(Direction~Lag1+Lag2,
               data = Smarket, family=binomial, subset=train )
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
predict(glm.fits, data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type="response")
