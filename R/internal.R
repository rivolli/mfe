binarize <- function(x) {
  att <- paste(colnames(x), collapse=" + ")
  x <- stats::model.matrix(stats::formula(paste("~ 0 +", att, sep=" ")), x)
  data.frame(x)
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
