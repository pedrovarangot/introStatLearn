library(pls)
library(ISLR)

Hitters <- na.omit(Hitters)

set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale = T,
               validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

set.seed(1)
train <- sample(length(Hitters$Salary),
                length(Hitters$Salary)/2)
test <- (-train)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train,
               scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, Hitters[test,], ncomp = 7)
mean((pcr.pred - y[test])^2)

pcr.fit <- pcr(Salary~., data = Hitters, scale = T,
               ncomp = 7)
summary(pcr.fit)
