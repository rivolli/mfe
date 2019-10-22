#' Concept Meta-features
#'
#' Concept characterization features measure the sparsity of the input space and 
#' the irregularity of the input-output distribution measures extract 
#' information about validation index.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param transform A logical value indicating if the categorical attributes
#'  should be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"cohesiveness"}{Example Cohesiveness is a different version of the
#'      wgDist measure.}
#'    \item{"conceptvar"}{Concept variation estimates the variability of class 
#'      labels among examples.}
#'    \item{"impconceptvar"}{Improved concept variation is a different version
#'       of the conceptvar measure.}
#'    \item{"wgDist"}{Weighted distance captures how dense or sparse is the 
#'      example distribution.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#' Vilalta, R., & Drissi, Y. (2002). A characterization of difficult 
#'  problems in classification. In M. A. Wani, H. R. Arabnia, K. J. 
#'  Cios, K. Hafeez, G. Kendall (Eds.), Proceedings ofthe 2002 
#'  international conference on machine learning and applications - 
#'  ICMLA 2002, June 24-27, 2002, Las Vegas, Nevada (pp. 133-138).
#'  
#' Vilalta, R., 1999. Understanding accuracy performance through 
#'  concept characterization and algorithm analysis. In: ECML 
#'  Workshop on Recent Advances in Meta-Learning and Future Work. 
#'  pp. 3-9.
#'
#' @examples
#' ## Extract all meta-features using formula
#' concept(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' concept(iris[1:4], iris[5], c("conceptvar"))
#'
#' ## Use another summarization function
#' concept(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
concept <- function(...) {
  UseMethod("concept")
}

#' @rdname concept
#' @export
concept.default <- function(x, y, features="all",
                               summary=c("mean", "sd"),
                               transform=TRUE, ...) {
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
    features <- ls.concept()
  }
  features <- match.arg(features, ls.concept(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)

  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  if(transform) {
    x <- binarize(x)
  } else {
    x <- x[sapply(x, is.numeric)]
  }

  x <- as.matrix(x)
  y <- as.integer(y)

  alpha <- 1 #Hyperparameter
  nfs <- apply(x, 2, function(col) max(col) - min(col))
  d <- apply(x, 1, function(row) sqrt(rowSums(t(row - t(x))/nfs) ^2))
  
  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- eval(call(fn, x=x, y=y, d=d, alpha=alpha))
    post.processing(measure, summary, f %in% ls.concept.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname concept
#' @export
concept.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"),
                                   transform=TRUE, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  concept.default(modFrame[-1], modFrame[1], features, summary, transform, 
    ...)
}

#' List the best concept meta-features
#'
#' @return A list of concept meta-features names.
#' @export
#'
#' @examples
#' ls.concept()
ls.concept <- function() {
  c("cohesiveness", "conceptvar", "impconceptvar", "wgDist")
}

ls.concept.multiples <- function() {
  ls.concept()
}

m.cohesiveness <- function(y, d, alpha, ...) {
  cls <- sapply(y, function(yr) yr != y)
  sapply(seq(ncol(d)), function(i){
    row <- cls[i,-i]
    radius <- ceiling(d[i,-i])
    radius[radius == 0] <- 1
    sum((1/2^(alpha*unique(radius))) * table(radius))
  })
}

m.conceptvar <- function(x, y, d, alpha, ...) {
  sn <- sqrt(ncol(x))
  W <- 1 / (2 ^ (alpha * (d/(sn-d))))
  diag(W) <- 0
  W[is.infinite(W)] <- 0 #TODO Think better what to do with these cases
  
  rowSums(W * sapply(y, function(yr) yr != y)) / rowSums(W)
}

m.impconceptvar <- function(y, d, alpha=1, ...) {
  cls <- sapply(y, function(yr) yr != y)
  sapply(seq(ncol(d)), function(i){
    row <- cls[i,-i]
    radius <- ceiling(d[i,-i])
    radius[radius == 0] <- 1
    sum(sapply(unique(radius), function(r){
      mean(row[radius == r]) * (1/2^(alpha*r))
    }))
  })
}

m.wgDist <- function(x, d, alpha, ...) {
  sn <- sqrt(ncol(x))
  W <- 1 / (2 ^ (alpha * (d/(sn-d))))
  diag(W) <- 0
  W[is.infinite(W)] <- 0 #TODO Think better what to do with these cases
  
  rowSums(W * d) / rowSums(W)
}