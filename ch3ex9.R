require(readr)
Auto <- read_csv("Auto.csv", header = T, na = "?")
Auto = na.omit(Auto)
View(Auto)

# a, scatterplot matrix
pairs(Auto)
?pairs

# b, matrix of correlations 
dim(Auto)

?cor
options(digits=4)
cor(Auto[,1:8])
options(digits=7)

# c, linear model of mpg with everything
fit = lm(mpg~., data = Auto[,1:8])
summary(fit)

# i) there is a relation between many predictors and response
# ii) displacement, weight, year, origin
# iii) modern cars are getting more mpg

par(mfrow=c(2,2))
plot(fit)

# e, simple model with best R^2
fit2 = lm(mpg~weight*horsepower, data=Auto[,1:8])
summary(fit2)
plot(fit2)

fit3 = lm(mpg~horsepower, data=Auto[,1:8])
summary(fit3)
fit4 = lm(mpg~horsepower+I(horsepower^2), data=Auto)
summary(fit4)

par(mfrow=c(1,1))
plot(mpg ~ horsepower)
abline(fit3, lwd = 2,col="red")
lines(sort(horsepower), predict(fit4)[order(horsepower)], lwd = 2, col = "blue")

fit5 = lm(mpg~horsepower+sqrt(horsepower), data=Auto)
summary(fit5)
lines(sort(horsepower), predict(fit5)[order(horsepower)], lwd = 2, col = "green")

par(mfrow=c(2,2))
plot(fit5)

