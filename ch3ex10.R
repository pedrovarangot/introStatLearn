require(ISLR)

attach(Carseats)

lm.fit = lm(Sales ~ Price+Urban+US)
summary(lm.fit)

lm.fit2 = lm(Sales ~ Price+US)
summary(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

predict(lm.fit2, data.frame(Price=c(50,100,150,50,100,150), 
                            US=c("No", "No","No", "Yes", "Yes", "Yes")),
        interval = "confidence")

predict(lm.fit2, data.frame(Price=c(50,100,150,50,100,150), 
                            US=c("No", "No","No", "Yes", "Yes", "Yes")),
        interval = "prediction")
