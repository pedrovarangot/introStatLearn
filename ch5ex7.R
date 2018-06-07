library(ISLR)

glm.fit <-  glm(Direction~Lag1+Lag2, data=Weekly, family = binomial)

glm.fit2 <- glm(Direction~Lag1+Lag2, data=Weekly[-1,], family = binomial)

glm.pred2 <- predict(glm.fit2, Weekly[1,])
glm.pred2
Weekly[1,]$Direction

preds_success = rep(0, length(Weekly$Direction))
for (i in 1:length(Weekly$Direction)) {
  glm.fitf <- glm(Direction~Lag1+Lag2, data=Weekly[-i,], family = binomial)
  glm.predf <- predict(glm.fitf, Weekly[i,])[1]
  preds_success[i] = 1 * ifelse(glm.predf > .5,
                            Weekly[i,]$Direction == "Up",
                            Weekly[i,]$Direction == "Down")
}
mean(preds_success)

