#' Landmarking Meta-features
#'
#' Landmarking meta-features include the performance of some simple and
#' efficient learning algorithms.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of methods to summarize the results as post-processing
#'  functions. See \link{post.processing} method to more information. (Default:
#'  \code{c("mean", "sd")})
#' @param map A list of decomposition strategies for multi-class problems. The
#'  options are \code{"one.vs.all"} and \code{"one.vs.one"} strategy.
#' @param folds The number of k equal size subsamples in k-fold
#'  cross-validation.
#' @param ... Optional arguments to the summary methods.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"decision.stumps"}{Represents the performance of a single node DT
#'      model induced by the most informative attribute.}
#'    \item{"elite.nearest.neighbor"}{Represents the performance of the Elite
#'      1-Nearest Neighbor classifier.}
#'    \item{"linear.discriminant"}{Represents the performance of the Linear
#'      Discriminant classifier.}
#'    \item{"naive.bayes"}{Represents the performance of the Naive Bayes
#'      classifier.}
#'    \item{"nearest.neighbor"}{Represents the performance of the 1-Nearest
#'      Neighbor classifier.}
#'    \item{"worst.node"}{Represents the performance of a single node DT
#'      model induced by the less informative attribute.}
#'  }
#' @return Each one of these meta-features generate multiple values (by fold
#'  and/or attribute) and then it is post processed by the summary methods.
#'  See the \link{post.processing} method for more details about it.
#'
#' @references
#'  Pfahringer, B., Bensusan, H., &  Giraud-Carrier, C. G. (2000). Meta-Learning
#'  by Landmarking Various Learning Algorithms. In Proceedings of the 17th
#'  International Conference on Machine Learning (pp. 743-750)
#'
#' @examples
#' ## Extract all meta-features using formula
#' mf.landmarking(Species ~ ., iris)
#'
#' ## Extract all meta-features using data.frame
#' mf.landmarking(iris[1:4], iris[5])
#'
#' ## Extract some meta-features
#' mf.landmarking(Species ~ ., iris, features=c("decision.stumps",
#' "nearest.neighbor", "linear.discriminant"))
#'
#' ## Extract all meta-features with different summary methods
#' mf.landmarking(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
mf.landmarking <- function(...) {
  UseMethod("mf.landmarking")
}

#' @rdname mf.landmarking
#' @export
mf.landmarking.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"),
                                   map=c("one.vs.all", "one.vs.one"), folds=10,
                                   ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  map <- match.arg(map)
  if(folds <= 1L | folds > nrow(x)) {
    stop("folds argument must be a integer > 1 and <= nrow(data)")
  }

  if(features[1] == "all") {
    features <- ls.landmarking()
  }
  features <- match.arg(features, ls.landmarking(), TRUE)

  data <- eval(call(map, x, y))
  split <- lapply(data, function(i) {
    createFolds(i[[2]], k=folds)
  })

  sapply(features, function(f) {
    measure <- mapply(function(data, split) {
      eval(call(f, x=data[[1]], y=data[[2]], split=split))
    }, data=data, split=split)
    post.processing(measure, summary)
  }, simplify=FALSE)
}

#' @rdname mf.landmarking
#' @export
mf.landmarking.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"),
                                   map=c("one.vs.all", "one.vs.one"), folds=10,
                                   ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  mf.landmarking.default(modFrame[, -1], modFrame[, 1], features, summary, map,
                         folds, ...)
}

#' List the Landmarking meta-features
#'
#' @return A list of Landmarking meta-features names
#' @export
#'
#' @examples
#' ls.landmarking()
ls.landmarking <- function() {
  c("decision.stumps", "elite.nearest.neighbor", "linear.discriminant",
    "naive.bayes", "nearest.neighbor", "worst.node")
}

accuracy <- function(pred, class) {
  aux <- table(class, pred)
  result <- sum(diag(aux)) / sum(aux)
  return(result)
}

dt.importance <- function(x, y, test, ...) {
  tryCatch({
    aux <- C50::C5imp(C50::C5.0(x[-test,], y[-test]))
    stats::setNames(aux$Overall, rownames(aux))
  }, error = function(e) {
    stats::setNames(rep(0, ncol(x)), colnames(x))
  })
}

one.vs.one <- function(x, y) {
  comb <- utils::combn(levels(y), 2)
  data <- apply(comb, 2, function(i) {
    n <- subset(x, y %in% i)
    p <- subset(y, y %in% i)
    list(n, factor(p))
  })

  return(data)
}

one.vs.all <- function(x, y) {
  comb <- levels(y)
  data <- lapply(comb, function(i) {
    p <- y
    p[p != i] <- setdiff(comb, i)[1]
    list(x, factor(p))
  })

  if(nlevels(y) < 3)
    return(data[1])
  return(data)
}

decision.stumps <- function(x, y, split, ...) {

  aux <- sapply(split, function(test) {
    imp <- names(dt.importance(x, y, test))[1]
    model <- C50::C5.0(x[-test, imp, drop=FALSE], y[-test])
    prediction <- stats::predict(model, x[test,])
    accuracy(prediction, y[test])
  })

  return(aux)
}

worst.node <- function(x, y, split, ...) {

  aux <- sapply(split, function(test) {
    imp <- names(dt.importance(x, y, test))[ncol(x)]
    model <- C50::C5.0(x[-test, imp, drop=FALSE], y[-test])
    prediction <- stats::predict(model, x[test,])
    accuracy(prediction, y[test])
  })

  return(aux)
}

nearest.neighbor <- function(x, y, split, ...) {

  aux <- sapply(split, function(test) {
    prediction <- class::knn(x[-test,], x[test,], y[-test], k=1)
    accuracy(prediction, y[test])
  })

  return(aux)
}

elite.nearest.neighbor <- function(x, y, split, ...) {

    aux <- sapply(split, function(test) {
      imp <- dt.importance(x, y, test)
      att <- names(which(imp != 0))
      if(all(imp == 0))
        att <- colnames(x)
      prediction <- class::knn(x[-test, att, drop=FALSE],
        x[test, att, drop=FALSE], y[-test], k=1)
      accuracy(prediction, y[test])
    })

  return(aux)
}

naive.bayes <- function(x, y, split, ...) {

  aux <- sapply(split, function(test) {
    model <- e1071::naiveBayes(x[-test,], y[-test])
    prediction <- stats::predict(model, x[test,])
    accuracy(prediction, y[test])
  })

  return(aux)
}

linear.discriminant <- function(x, y, split, ...) {

  x <- replace.nominal.columns(x)
  aux <- sapply(split, function(test) {
    tryCatch({
      model <- MASS::lda(x[-test,], grouping=y[-test])
      prediction <- stats::predict(model, x[test,])$class
      accuracy(prediction, y[test])
    }, error = function(e) {
      return(0)
    })
  })

  return(aux)
}
