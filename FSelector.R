## 特征选取
##1
data(iris)
result <- cfs(Species ~ ., iris)
f <- as.simple.formula(result, "Species")

##2
library(FSelector)
subset <- hill.climbing.search(names(iris)[-5], evaluator)


##3
 library(rpart)
  data(iris)
  
  evaluator <- function(subset) {
    #k-fold cross validation
    k <- 5
    splits <- runif(nrow(iris))
    results = sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- iris[test.idx, , drop=FALSE]
      train <- iris[train.idx, , drop=FALSE]
      tree <- rpart(as.simple.formula(subset, "Species"), train)
      error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
      return(1 - error.rate)
    })
    print(subset)
    print(mean(results))
    return(mean(results))
  }
  
  subset <- forward.search(names(iris)[-5], evaluator)
  f <- as.simple.formula(subset, "Species")
  print(f)

