#' Relative Landmarking Meta-features
#'
#' Relative Landmarking measures are landmarking measures using ranking 
#' strategy.
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
#' @param size The percentage of examples subsampled. Values different from 1
#' generate the subsampling-based relative landmarking metafeatures. 
#' (Default: 1.0)
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
#'  Johannes Furnkranz, Johann Petrak, Pavel Brazdil, and Carlos Soares. On the 
#'  use of Fast Subsampling Estimates for Algorithm Recommendation. Technical
#'  Report, pages 1-9, 2002.
#'
#' @examples
#' ## Extract all meta-features using formula
#' relative(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' relative(iris[1:4], iris[5], c("bestNode", "randomNode", "worstNode"))
#'
#' ## Use another summarization function
#' relative(Species ~ ., iris, summary=c("min", "median", "max"))
#'
#' ## Use 2 folds and balanced accuracy
#' relative(Species ~ ., iris, folds=2, score="balanced.accuracy")
#'
#' ## Extract the subsapling relative landmarking
#' relative(Species ~ ., iris, size=0.7)
#' @export
relative <- function(...) {
  UseMethod("relative")
}

#' @rdname relative
#' @export
relative.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), size=1, folds=10,
                                   score="accuracy", ...) {
  performance <- landmarking(x, y, features, summary, size, folds, score)
  performance <- apply(do.call("rbind", performance), 2, base::rank)
  split(data.frame(performance), rownames(performance))
}

#' @rdname relative
#' @export
relative.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), size=1, folds=10,
                                   score="accuracy", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  relative.default(modFrame[-1], modFrame[1], features, summary, size, 
    folds, score, ...)
}

#' List the relative meta-features
#'
#' @return A list of relative meta-features names.
#' @export
#'
#' @examples
#' ls.relative()
ls.relative <- function() {
  c("bestNode", "eliteNN", "linearDiscr", "naiveBayes", "oneNN", "randomNode", 
    "worstNode")
}

ls.relative.multiples <- function() {
  ls.relative()
}
