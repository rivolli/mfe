#' Landmarking Meta-features
#'
#' Landmarking measures are simple and fast learners, from which performance can
#' be extracted.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param folds The number of k equal size subsamples in k-fold 
#'  cross-validation.(Default: 10)
#' @param score The evaluation measure used to score the classification 
#'  performance. \code{c("accuracy", "balanced.accuracy", "kappa")}. 
#'  (Default: \code{"accuracy"}).
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"bestNode"}{Construct a single decision tree node model induced by 
#'    the most informative attribute to establish the linear separability 
#'    (multi-valued).}
#'    \item{"eliteNN"}{Elite nearest neighbor uses the most informative 
#'    attribute in the dataset to induce the 1-nearest neighbor. With the subset
#'    of informative attributes is expected that the models should be noise 
#'    tolerant (multi-valued).}
#'    \item{"linearDiscr"}{Apply the Linear Discriminant classifier to construct
#'    a linear split (non parallel axis) in the data to establish the linear 
#'    separability (multi-valued).}
#'    \item{"naiveBayes"}{Evaluate the performance of the Naive Bayes 
#'    classifier. It assumes that the attributes are independent and each 
#'    example belongs to a certain class based on the Bayes probability 
#'    (multi-valued).} 
#'    \item{"oneNN"}{Evaluate the performance of the 1-nearest neighbor 
#'    classifier. It uses the euclidean distance of the nearest neighbor to 
#'    determine how noisy is the data (multi-valued).}
#'    \item{"randomNode"}{Construct a single decision tree node model induced 
#'    by a random attribute. The combination with \code{"bestNode"} measure 
#'    can establish the linear separability (multi-valued).}
#'    \item{"worstNode"}{Construct a single decision tree node model induced
#'    by the worst informative attribute. The combination with 
#'    \code{"bestNode"} measure can establish the linear separability 
#'    (multi-valued).}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Bernhard Pfahringer, Hilan Bensusan, and Christophe Giraud-Carrier. 
#'  Meta-learning by landmarking various learning algorithms. In 17th 
#'  International Conference on Machine Learning (ICML), pages 743 - 750, 2000.
#'
#' @examples
#' ## Extract all meta-features using formula
#' landmarking(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' landmarking(iris[1:4], iris[5], c("bestNode", "randomNode", "worstNode"))
#'
#' ## Use another summarization function
#' landmarking(Species ~ ., iris, summary=c("min", "median", "max"))
#'
#' ## Use 2 folds and balanced accuracy
#' landmarking(Species ~ ., iris, folds=2, score="balanced.accuracy")
#' @export
landmarking <- function(...) {
  UseMethod("landmarking")
}

#' @rdname landmarking
#' @export
landmarking.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), folds=10,
                                   score="accuracy", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)
  
  if (nlevels(y) > length(y) / 10) {
    stop("y must contain classes values")
  }

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
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  test <- createFolds(y, folds=folds)

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- mapply(function(test) {
      prediction <- eval(call(fn, x=x, y=y, test=test, score=score))
      eval(call(score, prediction, y[test]))
    }, test=test)
    post.processing(measure, summary, f %in% ls.landmarking.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname landmarking
#' @export
landmarking.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), folds=10,
                                   score="accuracy", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  landmarking.default(modFrame[, -1], modFrame[, 1], features, summary,
                         folds, score, ...)
}

#' List the Landmarking meta-features
#'
#' @return A list of Landmarking meta-features names.
#' @export
#'
#' @examples
#' ls.landmarking()
ls.landmarking <- function() {
  c("bestNode", "eliteNN", "linearDiscr", "naiveBayes", "oneNN", "randomNode", 
    "worstNode")
}

ls.landmarking.multiples <- function() {
  ls.landmarking()
}

m.bestNode <- function(x, y, test, ...) {
  model <- ds(x, y, colnames(x), test)
  stats::predict(model, x[test,], type="class")
}

m.eliteNN <- function(x, y, test, ...) {
  imp <- names(importance(x, y, test))
  m.oneNN(x[, imp, drop=FALSE], y, test)
}

m.linearDiscr <- function(x, y, test, ...) {
  tryCatch({
    model <- MASS::lda(x[-test,], grouping=y[-test])
    stats::predict(model, x[test,])$class
  }, error = function(e) {
    rep(NA, length(test))
  })
}

m.naiveBayes <- function(x, y, test, ...) {
  model <- e1071::naiveBayes(x[-test,], y[-test])
  stats::predict(model, x[test,])
}

m.oneNN <- function(x, y, test, k=1, ...) {
  
  distance <- dist(x)[test, -test]
  prediction <- apply(distance, 1, function(i) {
    tmp <- names(sort(i)[1:k])
    y[rownames(x) == tmp]
  })
  
  return(prediction)
}

m.randomNode  <- function(x, y, test, ...) {
  model <- ds(x, y, sample(colnames(x), 1), test)
  stats::predict(model, x[test,], type="class")
}

m.worstNode <- function(x, y, test, ...) {
  model <- ds(x, y, utils::tail(names(importance(x, y, test)), 1), test)
  stats::predict(model, x[test,], type="class")
}
