library(randomForest)
library(MASS)
library(ggplot2)

set.seed(8)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

ntrees <- c(10, 50, seq(0, 500, 100)[-1])
mtryes <- c(3,5,6,7,9,13)
results <- data.frame(mtry=character(), ntrees=integer(),
                      terror=double(), tsd=double())

for(mtry in mtryes) {
  print(mtry)
  for(ntree in ntrees) {
    cat(ntree, " ")
    fit <- randomForest(medv~., data = Boston.train, 
                        mtry = mtry, ntree = ntree)
    yhat <- predict(fit, newdata = Boston.test)
    rss <- mean((Boston.test$medv - yhat)^2)
    tsd <- sd((Boston.test$medv - yhat))
    results <- rbind(results, data.frame(mtry = toString(mtry),
                                         ntrees = ntree,
                                         terror = rss,
                                         tsd = tsd))
  }
}

ggplot(data = results, mapping = aes(x = ntrees, y = terror, color = mtry)) +
  #geom_errorbar(mapping = aes(ymin = terror-.33*tsd, ymax = terror+.33*tsd),
  #              width = 10) +
  geom_line() + 
  geom_point(shape = 4)
