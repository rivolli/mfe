accuracy <- function(prediction, label) {
  label <- factor(label)
  prediction <- factor(prediction,  levels=levels(label))
  aux <- table(prediction, label)
  sum(diag(aux)) / sum(aux)
}

balanced.accuracy <- function(prediction, label) {
  label <- factor(label)
  prediction <- factor(prediction, levels=levels(label))
  aux <- table(prediction, label)
  mean(diag(aux) / colSums(aux))
}

binarize <- function(x) {
  att <- paste(colnames(x), collapse=" + ")
  x <- stats::model.matrix(stats::formula(paste("~ 0 +", att, sep=" ")), x)
  data.frame(x)
}

branch <- function(x, y, l) {
  rownames(x[y == l,])
}

categorize <- function(x) {
  att <- sapply(x, is.numeric)
  x <- cbind(x[!att], infotheo::discretize(x[att]))
  data.frame(sapply(x, as.factor))
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

dist <- function(data) {
  as.matrix(cluster::daisy(data, metric="gower", warnBin=FALSE))
}

dt <- function(...) {
  UseMethod("dt")
}

dt.default <- function(x, y, maxdepth=30, ...) {
  data <- cbind(class=y, x)
  dt.formula (stats::formula(data), data, maxdepth)
}

dt.formula <- function(formula, data, maxdepth=30, ...) {
  rpart::rpart(formula, data, method="class", 
    control=rpart::rpart.control(minsplit=2, minbucket=1, cp=-1, 
      maxdepth=maxdepth))
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

inter <- function(data, dst) {

  l <- levels(data$y)
  a <- branch(data$x, data$y, l[1])
  b <- branch(data$x, data$y, l[2])
  aux <- sum(dst[a, b])/prod(dim(dst[a, b]))
  return(aux)
}

intra <- function(data, dst, c) {
  a <- branch(data$x, data$y, c)
  aux <- max(dst[a, a])
  return(aux)
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

ovo <- function(x, y) {

  aux <- utils::combn(levels(y), 2)

  tmp <- apply(aux, 2, function(i) {
    x_new <- base::subset(x, y %in% i)
    y_new <- factor(base::subset(y, y %in% i))
    list(x=x_new, y=y_new)
  })

  return(tmp)
}
