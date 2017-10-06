#' Landmarking Meta-features
#'
#' Landmarking measures are simple and fast algorithms, from which performance
#' can be extracted. The measures use k-fold cross-validation and the evaluation
#' measure is accuracy.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of methods to summarize the results as post-processing
#'  functions. See \link{post.processing} method to more information. (Default:
#'  \code{c("mean", "sd")})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @param folds The number of k equal size subsamples in k-fold
#'  cross-validation.
#' @param ... Not used.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"decision.stumps"}{Construct a single DT node model induced by the
#'      most informative attribute. The single split (parallel axis) in the data
#'      has the main goal of establish the linear separability.}
#'    \item{"elite.nearest.neighbor"}{Select the most informative attributes in
#'      the dataset using the information gain ratio to induce the 1-Nearest
#'      Neighbor. With the subset of informative attributes is expected that the
#'      models induced by 1-Nearest Neighbor should be noise tolerant.}
#'    \item{"linear.discriminant"}{Apply the Linear Discriminant classifier to
#'      construct a linear split (non parallel axis) in the data to establish
#'      the linear separability.}
#'    \item{"naive.bayes"}{Evaluate the performance of the Naive Bayes
#'      classifier. It assumes that the attributes are independent and each
#'      example belongs to a certain class based on the Bayes probability.}
#'    \item{"nearest.neighbor"}{This measure evaluate the performance of the
#'      1-Nearest Neighbor classifier. It uses the euclidean distance of the
#'      nearest neighbor to determine how noisy is the data.}
#'    \item{"worst.node"}{Construct a single DT node model induced by the less
#'      informative attribute. With the "decision.stumps" measure is possible to
#'      define a baseline value of linear separability for dataset.}
#'  }
#' @return Each one of these meta-features generate multiple values (by fold
#'  and/or binary dataset) and then it is post processed by the summary methods.
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
#'
#' ## Extract all meta-features one vs one decomposition strategy
#' mf.landmarking(Species ~ ., iris)
#' @export
mf.landmarking <- function(...) {
  UseMethod("mf.landmarking")
}

#' @rdname mf.landmarking
#' @export
mf.landmarking.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), folds=10, ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.landmarking()
  }
  features <- match.arg(features, ls.landmarking(), TRUE)

  colnames(x) <- make.names(colnames(x))

  test <- createFolds(y, folds=folds)

  sapply(features, function(f) {
    measure <- mapply(function(test) {
      eval(call(f, x=x, y=y, test=test))
    }, test=test)
    post.processing(measure, summary, ls.landmarking.multiples())
  }, simplify=FALSE)
}

#' @rdname mf.landmarking
#' @export
mf.landmarking.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), folds=10, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  mf.landmarking.default(modFrame[, -1], modFrame[, 1], features, summary,
                         folds, ...)
}

#' List the Landmarking meta-features
#'
#' @return A list of Landmarking meta-features names.
#' @export
#'
#' @examples
#' ls.landmarking()
ls.landmarking <- function() {
  c("decision.stumps", "elite.nearest.neighbor", "linear.discriminant",
    "naive.bayes", "nearest.neighbor", "worst.node")
}

ls.landmarking.multiples <- function() {
  ls.landmarking()
}

accuracy <- function(prediction, label) {
  aux <- table(prediction, label)
  sum(diag(aux)) / sum(aux)
}

dt.importance <- function(x, y, test) {
  tryCatch({
    aux <- C50::C5imp(C50::C5.0(x[-test,], y[-test]))
    stats::setNames(aux$Overall, rownames(aux))
  }, error = function(e) {
    stats::setNames(rep(0, ncol(x)), colnames(x))
  })
}

decision.stumps <- function(x, y, test, ...) {
  imp <- names(dt.importance(x, y, test))[1]
  model <- C50::C5.0(x[-test, imp, drop=FALSE], y[-test])
  prediction <- stats::predict(model, x[test,])
  accuracy(prediction, y[test])
}

worst.node <- function(x, y, test, ...) {
  imp <- names(dt.importance(x, y, test))[ncol(x)]
  model <- C50::C5.0(x[-test, imp, drop=FALSE], y[-test])
  prediction <- stats::predict(model, x[test,])
  accuracy(prediction, y[test])
}

nearest.neighbor <- function(x, y, test, ...) {
  x <- binarize(x)
  data <- data.frame(class=y, x)
  prediction <- kknn::kknn(class ~. , data[-test,], data[test,-1], k=1)
  accuracy(prediction$fitted.values, y[test])
}

elite.nearest.neighbor <- function(x, y, test, ...) {
  x <- binarize(x)
  imp <- dt.importance(x, y, test)
  att <- names(which(imp != 0))
  if(all(imp == 0))
    att <- colnames(x)

  data <- data.frame(class=y, x[, att, drop=FALSE])
  prediction <- kknn::kknn(class ~ ., data[-test,], data[test,], k=1)
  accuracy(prediction$fitted.values, y[test])
}

naive.bayes <- function(x, y, test, ...) {
  model <- e1071::naiveBayes(x[-test,], y[-test])
  prediction <- stats::predict(model, x[test,])
  accuracy(prediction, y[test])
}

linear.discriminant <- function(x, y, test, ...) {
  tryCatch({
    model <- MASS::lda(x[-test,], grouping=y[-test])
    prediction <- stats::predict(model, x[test,])$class
    accuracy(prediction, y[test])
  }, error = function(e) {
    return(NA)
  })
}
