#' Discriminant meta-features
#'
#' Discriminant meta-features are computed using the discriminant analysis. It
#' is computed using just the numerical attributes.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y a factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param ... Not used.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"cancor"}{Represents the first canonical discriminant correlations
#'       of the numeric attributes in the dataset.}
#'    \item{"center.of.gravity"}{Represents the distance between minority and
#'       majority classes in the dataset.}
#'    \item{"discfct"}{Represents the number of discriminant functions
#'       normalized by the number of classes"}
#'    \item{"cancor.fract"}{Represents a measure of collinearity of the class
#'       means. It is computed using the squares of the canonical correlations
#'       instead of the eigenvalues.}
#'    \item{"eigen.fract"}{Represents the relative importance of the largest
#'       eigenvalue of the attribute covariance matrix computed from the
#'       numeric attributes in the dataset.}
#'    \item{"sdratio"}{Represents the test statistic for homogeneity of
#'       covariances. It uses the Box's M test and it is is strictly greater
#'       than unity if the covariances differ, and is equal to unity if and only
#'       if the M-statistic is zero.}
#'  }
#'
#'  The categorical attributes is replaced by binaries attributes.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Michie, E. D., Spiegelhalter, D. J., & Taylor, C. C. (1994).
#'    Machine Learning , Neural and Statistical Classification.
#'    Technometrics, 37(4), 459.
#'
#'  Castiello, C., Castellano, G., & Fanelli, A. M. (2005). Meta-data:
#'    Characterization of Input Features for Meta-learning. In Proceedings of
#'    the 2nd International Conference on Modeling Decisions for Artificial
#'    Intelligence (Vol. 3558, pp. 457-468).
#'
#'  Lindner, G., & Studer, R. (1999). AST: Support for Algorithm Selection with
#'    a CBR Approach. Principles of Data Mining and Knowledge Discovery,
#'    1704, 418-423.
#'
#'  Ali, S., & Smith, K. a. (2006). On learning algorithm selection for
#'    classification. Applied Soft Computing, 6(2), 119-138.
#'
#' @examples
#' ## Extract all metafeatures
#' mf.discriminant(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' mf.discriminant(iris[1:4], iris[5], c("cancor", "cancor.fract"))
#' @export
mf.discriminant <- function(...) {
  UseMethod("mf.discriminant")
}

#' @rdname mf.discriminant
#' @export
mf.discriminant.default <- function(x, y, features="all", ...) {
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

  if(features[1] == "all") {
    features <- ls.discriminant()
  }
  features <- match.arg(features, ls.discriminant(), TRUE)

  numdata <- replace.nominal.columns(x) #TODO control by user parameter
  y.num <- replace.nominal.columns(as.data.frame(y))
  x.cov <- stats::cov(numdata)
  extra <- list(
    y.num = y.num,
    cancor = stats::cancor(numdata, y.num),
    x.cov = x.cov,
    eigenvalues = base::eigen(x.cov)
  )

  sapply(features, function(f) {
    measure <- eval(call(f, x=numdata, y=y, extra=extra))
    post.processing(measure, summary, ...)
  }, simplify=FALSE)
}

#' @rdname mf.discriminant
#' @export
mf.discriminant.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula,data)
  attr(modFrame, "terms") <- NULL

  mf.discriminant.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' List the discriminant meta-features
#'
#' @return A list of discriminant meta-features names
#' @export
#'
#' @examples
#' ls.discriminant()
ls.discriminant <- function() {
  c("cancor", "cancor.fract", "center.of.gravity", "discfct", "eigen.fract",
    "max.eigenvalue", "min.eighenvalue", "sdratio", "wlambda")
}

cancor <- function(x, y, extra, ...) {
  extra$cancor$cor[1]
}

center.of.gravity <- function(x, y, ...) {
  #TODO Change to another group
  classes <- table(y)
  minc <- which.min(classes)
  maxc <- which.max(classes[-minc])
  centers <- t(sapply(names(c(minc, maxc)), function(class){
    apply(x[y == class, ], 2, mean)
  }))

  c(stats::dist(centers))
}

discfct <- function(x, y, extra, ...) {
  #TODO Confirm if the use of levels normalizes the measure
  length(extra$cancor$cor) / nlevels(y)
}

eigen.fract <- function(x, y, extra, ...) {
  #Castiello Version
  values <- extra$eigenvalues$values
  values[1] / sum(values)
}

cancor.fract <- function(x, y, extra, ...) {
  #Michie version
  values <- extra$cancor$cor ^ 2
  values[1] / sum(values)
}

max.eigenvalue <- function(x, y, extra, ...) {
  max(extra$eigenvalues$values)
}

min.eighenvalue <- function(x, y, extra, ...) {
  min(extra$eigenvalues$values)
}

sdratio <- function(x, y, extra, ...) {
  #The Michie formulation there are some wrongs
  p <- ncol(x)
  q <- nlevels(y)
  n <- length(y)
  ni <- table(y) - 1

  Si <- lapply(levels(y), function(class) stats::cov(x[y == class,]))
  S <- Reduce('+', mapply(function(Si, ni) ni*Si, S=Si, n=ni, SIMPLIFY=FALSE)) /
    (n - q)

  tryCatch({
    M <- (1 - ((2*p^2+3*p-1)/(6*(p+1)*(q-1))) * (sum(1/ni)-1/(n-q))) *
      ((n - q) * log(det(S)) - sum(ni * log(sapply(Si, det))))

    #Sometimes the det is zero and log is Inf, then return 0
    ifelse(is.na(M) | is.infinite(M), 0, exp(M / (p * sum(ni - 1))))
  }, warning = function(e) {
    #Sometimes the det is negative, then return 0
    0
  })
}

wlambda <- function(x, y, extra, ...) {
  prod(1 / (1 + extra$cancor$cor))
}
