library(ISLR)

train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test <- Carseats[-train,]

library(tree)

tree.fit <- tree(Sales~., data = Carseats, subset = train)
plot(tree.fit)
text(tree.fit, pretty = 0)
tree.pred <- predict(tree.fit, newdata = Carseats.test)
mean((Carseats.test$Sales - tree.pred)^2)

?cv.tree
cv.carseats <- cv.tree(tree.fit)
summary(cv.carseats)
plot(cv.carseats)
?prune.tree
pruned.tree <- prune.tree(tree.fit, best = 10)
plot(pruned.tree)
text(pruned.tree, pretty = 0)
tree.pred <- predict(pruned.tree, newdata = Carseats.test)
mean((Carseats.test$Sales - tree.pred)^2)

library(randomForest)
?randomForest
bag.sales <- randomForest(Sales~., data = Carseats, subset = train,
                          mtry = ncol(Carseats) - 1, importance = T)
importance(bag.sales)
bag.pred <- predict(bag.sales, newdata = Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

mses <- double(ncol(Carseats) - 1)
for(m in 1:(ncol(Carseats) - 1)) {
  cat(m, " ")
  forest.sales <- randomForest(Sales~., data = Carseats, subset = train,
                               mtry = m, importance = F)
  mses[m] <- mean((Carseats.test$Sales - 
                     predict(forest.sales, newdata = Carseats.test))^2)
}

plot(1:(ncol(Carseats) - 1), mses)
forest.sales <- randomForest(Sales~., data = Carseats, subset = train,
                             mtry = 7, importance = T)
importance(forest.sales)
