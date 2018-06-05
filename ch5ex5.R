library(ISLR)

glm.fit <- glm(default~income+balance, data=Default, family = binomial)
glm.pred <- predict(glm.fit, Default, type= "response")
glm.resp <- rep("No", length(glm.pred))
glm.resp[glm.pred > .5] = "Yes"
table(Default$default, glm.resp)
mean(Default$default == glm.resp)

set.seed(1)
train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(default~income+balance, data=Default, family = binomial,
               subset = train)
glm.pred <- predict(glm.fit, Default[-train,], type = "response") 
glm.resp <- rep("No", length(glm.pred))
glm.resp[glm.pred > .5] = "Yes"
table(Default[-train,]$default, glm.resp)
mean(Default[-train,]$default != glm.resp)

set.seed(1)
train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(default~income+balance+student, data=Default, family = binomial,
               subset = train)
glm.pred <- predict(glm.fit, Default[-train,], type = "response") 
glm.resp <- rep("No", length(glm.pred))
glm.resp[glm.pred > .5] = "Yes"
table(Default[-train,]$default, glm.resp)
mean(Default[-train,]$default != glm.resp)
