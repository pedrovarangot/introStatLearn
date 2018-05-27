set.seed(1)
x = rnorm(100) - 0.5
y = 2 * x * rnorm(100)

summary(lm(y~x+0))
summary(lm(x~y+0))

# if their squared sum is the same the estimates should be the same
