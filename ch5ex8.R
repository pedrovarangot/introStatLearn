set.seed(1)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)
# n = 100, p = x
plot(x,y)
data = data.frame(Y=y, X=x)
summary(data)

error_v = rep(0, 4)

require(boot)
error_m1 = rep(0, length(data$X))
for (i in 1:length(data$X)) {
  lm.fit = lm(Y~X, data=data[-i,])
  y_pred = predict(lm.fit, data[i,])
  error_m1[i] = y_pred - data[i,1]
}
mean(error_m1^2)
# Using bootstrap library
glm.fit = glm(Y~X, data=data)
cv.glm(data, glm.fit)$delta

cv.glm(data, glm(Y~poly(X,2), data=data))$delta
cv.glm(data, glm(Y~poly(X,3), data=data))$delta
cv.glm(data, glm(Y~poly(X,4), data=data))$delta

