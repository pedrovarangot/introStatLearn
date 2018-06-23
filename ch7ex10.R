library(ISLR)
library(leaps)
library(ggplot2)
library(gam)

set.seed(2)

train <- sample(c(T,F), length(College$Outstate), replace = T)
test <- !train

reg.fit <- regsubsets(Outstate~., data = College[train,], y = College[train,9],
                      method = "forward", nvmax = length(College))

ggplot(mapping = aes(x = 1:length(summary(reg.fit)$adjr2), y = summary(reg.fit)$adjr2)) +
  geom_point() +
  geom_point(mapping = aes(x = 10, y = summary(reg.fit)$adjr2[10], color = "red"),
                           show.legend = F)

rss <- rep(NA, length(College) - 1)
reg.summary <- summary(reg.fit)
for(n in 1:(length(College) -1)) {
  lm.fit <- lm(College$Outstate~., data = College[,reg.summary$which[n,]], subset = test)
  rss[n] <- sum(lm.fit$residuals^2)
}

ggplot(mapping = aes(x = 1:((length(College)) - 1), y = rss)) +
  geom_point() +
  geom_hline(yintercept = min(rss) - .33*sd(rss), color = "red", size = .25) + 
  geom_hline(yintercept = min(rss) + .33*sd(rss), color = "red", size = .25)

# Use six coefficients

coefs <- coef(reg.fit, id = 6)
names(coefs)

