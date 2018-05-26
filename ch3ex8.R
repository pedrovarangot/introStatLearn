library(readr)
Auto <- read_csv("Auto.csv", na = "?")
Auto = na.omit(Auto)
View(Auto)
attach(Auto)
fit = lm(mpg~horsepower)
fit$coefficients
plot(horsepower, mpg)
?abline
abline(fit$coefficients[1], fit$coefficients[2], col = "red" )
# code because I didn't know about predict
mpgpred = function(hp) {
  fit$coefficients[1] + fit$coefficients[2] * hp
}
mpgpred(98)
summary(fit)
?confint
confint(fit)
?predict
# same as my own function
predict(fit, data.frame(horsepower=c(98)),
        interval = "confidence")
predict(fit, data.frame(horsepower=c(98)),
        interval = "prediction")

