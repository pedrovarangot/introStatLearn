set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

plot(x1, x2)
lm.fit = lm(y~x1+x2)
summary(lm.fit)

lm.fit2 = lm(y~x1)
summary(lm.fit2)

lm.fit3 = lm(y~x2)
summary(lm.fit3)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c (y, 6)

lm.fit = lm(y~x1+x2)
summary(lm.fit)

lm.fit2 = lm(y~x1)
summary(lm.fit2)

lm.fit3 = lm(y~x2)
summary(lm.fit3)

plot(lm.fit3)
plot(x2[1:100], y[1:100], xlim = c(-.1, .8), ylim = c(-.1, 7))
points(x2[101], y[101], col="red")

# new point is high leverage, not an outlier
