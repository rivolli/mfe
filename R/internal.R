replace.nominal.columns <- function(x) {
  att <- paste(colnames(x), collapse="+")
  x <- stats::model.matrix(stats::formula(paste("~ 0 +", att, sep=" ")), x)
  return(x)
}

replace.numeric.columns <- function(x) {
  numcols <- sapply(x, is.numeric)
  #TODO trocar pelo histograma
  x <- cbind(x[!numcols], infotheo::discretize(x[numcols]))[colnames(x)]
  as.data.frame(sapply(x, as.factor))
}

validate.and.replace.nominal.attr <- function(x, transform.attr) {
  if(transform.attr) {
    numdata <- replace.nominal.columns(x)
  }else {
    numcols <- sapply(x, is.numeric)
    numdata <- x[numcols]
    if(!any(numcols)) {
      stop("dataset does not contain numerical attributes")
    }
  }

  numdata
}

validate.and.replace.numeric.attr <- function(x, transform.attr) {
  if(transform.attr) {
    catdata <- replace.numeric.columns(x)
  }else {
    numcols <- sapply(x, is.numeric)
    catdata <- x[!numcols]
    if(all(numcols)) {
      stop("dataset does not contain categorical attributes")
    }
  }

  catdata
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

