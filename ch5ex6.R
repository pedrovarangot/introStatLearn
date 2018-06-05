library(ISLR)

summary(glm(default~income+balance, data=Default, family=binomial))$coeff

boot.fn <- function(data, index) {
  coefficients(glm(default~income+balance, data=data, subset=index,
                   family = binomial))
}
boot.fn(Default, 1:100)

library(boot)

boot(Default, boot.fn, 100)
