accuracy <- function(prediction, label) {
  label <- factor(label)
  prediction <- factor(prediction,  levels=levels(label))
  aux <- table(prediction, label)
  sum(diag(aux)) / sum(aux)
}

binarize <- function(x) {
  att <- paste(colnames(x), collapse=" + ")
  x <- stats::model.matrix(stats::formula(paste("~ 0 +", att, sep=" ")), x)
  data.frame(x)
}

balanced.accuracy <- function(prediction, label) {
  label <- factor(label)
  prediction <- factor(prediction, levels=levels(label))
  aux <- table(prediction, label)
  mean(diag(aux) / colSums(aux))
}

categorize <- function(x) {
  att <- sapply(x, is.numeric)
  x <- cbind(x[!att], infotheo::discretize(x[att]))
  data.frame(sapply(x, as.factor))
}

createFolds <- function(y, folds) {

  if(folds <= 1L | folds > min(table(y))) {
    stop("folds argument must be an integer > 1 and <= the number of examples in
      the minority class")
  }

  names(y) <- 1:length(y)
  index <- lapply(1:nlevels(y), function(i) {
    rep(1:folds, length.out=length(y[y == levels(y)[i]]))
  })

  index <- unlist(index)
  folds <- lapply(1:folds, function(i) {
    as.numeric(names(y[index == i]))
  })

  return(folds)
}

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  }, list(...))
}

concentration.coefficient <- function(x, y) {
  nij <- table(y, x) / length(x)
  isum <- rowSums(nij)
  jsum2 <- sum(colSums(nij)^2)
  nij2 <- nij^2
  
  (sum(nij2 / isum) - jsum2) / (1 - jsum2)
}

dist <- function(data) {
  as.matrix(cluster::daisy(data, metric="gower", warnBin=FALSE))
}

ds <- function(x, y, imp, test, ...) {
  data <- cbind(class=y[-test], x[-test, imp, drop=FALSE])
  rpart::rpart(stats::formula(data), data, method="class", 
    control=rpart::rpart.control(minsplit=2, minbucket=1, cp=0.001, maxdepth=1))
}

dt <- function(formula, data, ...) {
  rpart::rpart(formula, data, method="class", 
    control=rpart::rpart.control(minsplit=2, minbucket=1, cp=0.001))
}

entropy <- function(x) {
  qi <- table(x) / length(x)
  -sum(qi * sapply(qi, log2))
}

importance <- function(x, y, test) {
  data <- cbind(class=y[-test], x[-test,])
  model <- dt(stats::formula(data), data)
  model$variable.importance
}

kappa <- function(prediction, label) {
  label <- factor(label)
  prediction <- factor(prediction, levels=levels(label))
  aux <- table(prediction, label)

  pc <- sum(apply(aux, 1, sum)/sum(aux) * 
    apply(aux, 2, sum)/sum(aux))

  if(pc == 1 | is.nan(pc))
    pc <- 0

  aux <- (sum(diag(aux))/sum(aux) - pc)/(1 - pc)
  return(aux)
}
