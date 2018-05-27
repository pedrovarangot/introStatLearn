?rnorm
set.seed(1)
x = rnorm(100)
eps = rnorm(100, mean = 0, sd = 0.25^2)
y = -1 + 0.5*x + eps
lm.fit = lm(y~x)
summary(lm.fit)

plot(x,y)
abline(lm.fit, col = "red")
abline(-1, 0.5, col = "blue")
?legend
legend(x = "topleft", col = c("red", "blue"), legend = c("Least squares line", "Population line"), lwd = 1)

lm.fit2 = lm(y~poly(x,2))
summary(lm.fit2)

epsL = rnorm(100, mean = 0, sd = 0.1^2)
epsH = rnorm(100, mean = 0, sd = 0.4^2)
yL = -1 + 0.5*x + epsL
yH = -1 + 0.5*x + epsH

lm.fit3 = lm(yL~x)
lm.fit4 = lm(yH~x)
predict(lm.fit3, data.frame( x = c(0)), interval = "confidence")
predict(lm.fit4, data.frame( x = c(0)), interval = "confidence")
summary(lm.fit3)
summary(lm.fit4)
