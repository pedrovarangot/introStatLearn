library(ISLR)
library(boot)
library(ggplot2)

degree = 1:7
rse = rep(0,7)
cv.err = rep(0,7)
fits = rep(0, 7)

for(d in degree) {
  fits[d] <- glm(wage~poly(age, d), data = Wage)
  glm.fit <- glm(wage~poly(age, d), data = Wage)
  rse[d] <- sqrt(glm.fit$deviance / glm.fit$df.residual)
  cv.err[d] <- cv.glm(Wage, glm.fit, K = 10)$delta
}

ggplot() +
  geom_point(mapping = aes(x = degree, y = cv.err))

ggplot(data = Wage) +
  geom_jitter(mapping = aes(age, wage), size = 1) +
  geom_line(mapping = aes(age, predict(glm(wage~poly(age, 4), data = Wage), 
                                       data.frame(age = Wage$age))),
             color = "red", size = 2)

val.err <- rep(NA, 10)
for(c in 2:10) {
  Wage$age.cut <- cut(Wage$age, c)
  glm.fit <- glm(wage~age.cut, data = Wage)
  val.err[c] <- cv.glm(glm.fit, data = Wage, K = 10)$delta[2]
}

ggplot(mapping = aes(2:10, val.err[2:10])) +
  geom_point()

Wage$age.cut <- cut(Wage$age, 8)
glm.fit <- glm(wage~age.cut, data = Wage)

ggplot(data = Wage) +
  geom_jitter(mapping = aes(age, wage), color = "grey", size=1) +
  geom_line(mapping = aes(age, predict(glm.fit, 
                                       data.frame(age.cut = Wage$age.cut))
                          ),
            color = "blue", size = 1)
