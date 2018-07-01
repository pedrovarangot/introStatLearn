library(ISLR)

set.seed(5)
train <- sample(1:nrow(OJ), 800)
test <- -train

library(tree)

tree.oj <- tree(Purchase~., data = OJ, subset = train)
summary(tree.oj)

tree.oj
plot(tree.oj)
text(tree.oj, pretty = 0)

tree.pred <- predict(tree.oj, OJ[test,], type = "class")
tree.conf <- table(tree.pred, OJ[test,]$Purchase)
tree.conf
(tree.conf[1,1] + tree.conf[2,2])/nrow(OJ[test,])

tree.ojpruned <- cv.tree(tree.oj, FUN = prune.misclass)
summary(tree.ojpruned)
plot(tree.ojpruned)

clse <- double(summary(tree.oj)$size - 1)
for(b in 2:length(clse)) {
  tree <- prune.misclass(tree.oj, best = b)
  pred <- predict(tree, OJ[test,], type = "class")
  cls <- table(pred, OJ[test,]$Purchase)
  clse[b] <- (cls[1,1] + cls[2,2])/nrow(OJ[test,])
}
plot(2:(length(clse)+1), 1-clse)

tree.ojpruned <- prune.misclass(tree.oj, best = 3)
plot(tree.ojpruned)
text(tree.ojpruned, pretty = 0)
tree.ojpruned.pred <- predict(tree.ojpruned, OJ[test,], type = "class")
tree.ojpruned.conf <- table(tree.ojpruned.pred, OJ[test,]$Purchase)
tree.ojpruned.train <- predict(tree.ojpruned, OJ[train,], type = "class")
tree.ojpruned.tconf <- table(tree.ojpruned.train, OJ[train,]$Purchase)
(tree.ojpruned.conf[1,1] + tree.ojpruned.conf[2,2])/nrow(OJ[test,])
(tree.ojpruned.tconf[1,1] + tree.ojpruned.tconf[2,2])/nrow(OJ[train,])

