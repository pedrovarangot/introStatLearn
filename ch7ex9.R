library(MASS)
?Boston

# a)

glm.fit3 <- glm(nox~poly(dis, 3), data = Boston)
summary(glm.fit3)

ggplot(data = Boston, mapping = aes(x = dis, y = nox)) +
  geom_point() +
  geom_line(mapping = aes(x = dis, y = predict(glm.fit3, Boston)),
            color = "blue", size = 1)

# b and c)

degrees <- 1:10
rss <- rep(NA, 10)
cv.err <- rep(NA, 10)
for(d in degrees) {
  glm.fit <- glm(nox~poly(dis, d), data = Boston)
  rss[d] <- sum(glm.fit$residuals^2)
  cv.err[d] <- cv.glm(Boston, glm.fit, K=10)$delta[2]
}

ggplot(mapping = aes(x=degrees, y=rss)) + geom_point()
ggplot(mapping = aes(x=degrees, y=cv.err)) + geom_point()

# degree five

# d)

bs.fit <- lm(nox~bs(dis, df = 4), data = Boston)
ggplot() +
  geom_point(mapping = aes(x=degrees, y=rss), size = 1) +
  geom_point(mapping = aes(x=4, y=sum(bs.fit$residuals^2)), color = "red")

ggplot(data = Boston) +
  geom_jitter(mapping = aes(x = dis, y = nox), size = 1) +
  geom_line(mapping = aes(x = dis, y = predict(bs.fit, Boston)),
            color = "blue", size = 1) +
  geom_line(mapping = aes(x = dis, y = predict(glm.fit3, Boston)),
            color = "green", size = .5)


