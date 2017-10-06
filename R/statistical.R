#' Statistical meta-features
#'
#' Statistical meta-features are the standard statistical measures to describe
#' the numerical properties of a distribution of data. As it requires only
#' numerical attributes, the categorical data are transformed to numerical.
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
#' @param by.class A logical value indicating if the meta-features must be
#'  computed for each group of samples belonging to different output classes.
#'  (Default: TRUE)
#' @param ... Further arguments passed to or from other methods like the
#'  meta-features and post-processing.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"correlation"}{Represents the absolute correlation between each pair
#'       of the numeric attributes in the dataset. This measure accepts an extra
#'       argument called \code{method} that values can be \code{c("pearson",
#'       "kendall", "spearman")}, see \code{\link{cor}} for more details.}
#'    \item{"covariance"}{Represents the absolute covariance between each pair
#'       of the numeric attributes in the dataset. This measure accepts an extra
#'       argument called \code{method} that values can be \code{c("pearson",
#'       "kendall", "spearman")}, see \code{\link{cov}} for more details.}
#'    \item{"discreteness.degree"}{Represents the degree of discreetness of each
#'       attribute in the dataset. It is measured using a sparsity measure.}
#'    \item{"geometric.mean"}{Represents the geometric mean of the numeric
#'       attributes in the dataset.}
#'    \item{"harmonic.mean"}{Represents the harmonic mean of the numeric
#'       attributes in the dataset.}
#'    \item{"iqr"}{Represents the interquartile range divided by the standard
#'       deviation of the numeric attributes in the dataset.}
#'    \item{"kurtosis"}{Represents the kurtosis of the numeric attributes in
#'       the dataset.}
#'    \item{"mad"}{Represents the median absolute deviation of the numeric
#'       attributes in the dataset.}
#'    \item{"normality"}{Represents the p-values of the attributes according to
#'       a normal distribution test. For that it is used the Shapiro-Wilk
#'       Normality Test. Values lower than 0.05 could be considered provenient
#'       from a normal distribution.}
#'    \item{"outliers"}{"Ratio of the variances of mean value and the
#'       alfa-trimmed mean. Values smaller than 0.7 can be considered outliers.
#'       The default alpha value is 0.05 but it can be setted using extra
#'       parameters."}
#'    \item{"skewness"}{Represents the skewness of the numeric attributes in
#'       the dataset.}
#'    \item{"standard.deviation"}{Represents the standard deviation of the
#'       numeric attributes in the dataset.}
#'    \item{"trim.mean"}{Represents the trim mean of the numeric attributes in
#'       the dataset. It is the aritimetic mean excluding the 20% of the lowest
#'       and hieghest instances.}
#'    \item{"variance"}{Represents the variance (normalization of the standard
#'       deviation) of the numeric attributes in the dataset.}
#'  }
#'  Each one of these meta-features generate multiple values (by attribute
#'  and/or class value) and then it is post processed by the summary methods.
#'  See the \link{post.processing} method for more details about it.
#'
#'  The categorical attributes is replaced by binaries attributes.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Castiello, C., Castellano, G., & Fanelli, A. M. (2005). Meta-data:
#'    Characterization of Input Features for Meta-learning. In Proceedings of
#'    the 2nd International Conference on Modeling Decisions for Artificial
#'    Intelligence (Vol. 3558, pp. 457-468).
#'
#'  Ali, S., & Smith, K. a. (2006). On learning algorithm selection for
#'    classification. Applied Soft Computing, 6(2), 119-138.
#'
#' @examples
#' ## Extract all meta-features
#' mf.statistical(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' mf.statistical(iris[1:4], iris[5], c("correlation", "variance"))
#'
#' ## Use another summary methods
#' mf.statistical(Species ~ ., iris, summary=c("min", "median", "max"))
#'
#' ## Compute the mean for each measure without consider the class information
#' mf.statistical(Species ~ ., iris, summary="mean", by.class=FALSE)
#' @export
mf.statistical <- function(...) {
  UseMethod("mf.statistical")
}

#' @rdname mf.statistical
#' @export
mf.statistical.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), by.class=TRUE,
                                    ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if (min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.statistical()
  }
  features <- match.arg(features, ls.statistical(), TRUE)
  colnames(x) <- make.names(colnames(x))

  numdata <- binarize(x)

  if(by.class) {
    measures <- sapply(levels(y), function(class) {
      new.data <- numdata[y==class, , drop=FALSE]
      new.data <- new.data[, apply(new.data, 2, stats::sd) != 0, drop=FALSE]
      aux <- sapply(features, function(f) {
        do.call(f, c(list(x=new.data), list(...)))
      }, simplify=FALSE)
    }, simplify=FALSE)

    sapply(features, function(f) {
      values <- lapply(measures, function (values) values[[f]])
      post.processing(unlist(values), summary, ...)
    }, simplify=FALSE)
  } else {
    sapply(features, function(f) {
      measure <- do.call(f, c(list(x=numdata), list(...)))
      post.processing(measure, summary, ...)
    }, simplify=FALSE)
  }
}

#' @rdname mf.statistical
#' @export
mf.statistical.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), by.class=TRUE,
                                   ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  mf.statistical.default(modFrame[, -1], modFrame[, 1], features, summary,
                         by.class, ...)
}

#' List the statistical meta-features
#'
#' @return A list of statistical meta-features names
#' @export
#'
#' @examples
#' ls.statistical()
ls.statistical <- function() {
  c("correlation", "covariance", "discreteness.degree", "geometric.mean",
    "harmonic.mean", "iqr", "kurtosis", "mad", "normality", "outliers",
    "skewness", "standard.deviation", "trim.mean", "variance")
}

correlation <- function(x, ...) {
  args <- list(...)
  method <- ifelse(is.null(args$method), "pearson", args$method)
  aux <- stats::cor(x, method=method)
  abs(aux[upper.tri(aux)])
}

covariance <- function(x, ...) {
  args <- list(...)
  method <- ifelse(is.null(args$method), "pearson", args$method)
  aux <- stats::cov(x, method=method)
  values <- abs(aux[upper.tri(aux)])
}

discreteness.degree <- function(x, ...) {
  apply(x, 2, function(col) mean(table(col), ...))
}

geometric.mean <- function(x, ...) {
  res1 <- apply(x, 2, prod)^(1/nrow(x))

  x[x < 1] <- NA
  res2 <- apply(x, 2, function(col) {
    exp(mean(log(col), ...))
  })

  coalesce(res1, res2)
}

harmonic.mean <- function(x, ...) {
  res <- apply(x, 2, function(col) length(col) / sum(1/col))
  res[!is.finite(res)] <- NA
  res
}

iqr <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::IQR, na.rm=na.rm) / apply(x, 2, stats::sd, na.rm=na.rm)
}

kurtosis <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  abs(apply(x, 2, e1071::kurtosis, na.rm=na.rm))
}

mad <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::mad, na.rm=na.rm)
}

normality <- function(x, ...) {
  apply(x, 2, function(col) {
    stats::shapiro.test(sample(col, min(length(col), 5000)))$p.value
  })
}

outliers <- function(x, alpha=0.05, ...) {
  apply(x, 2, function(x) mean(x, ...)/mean(x, trim=alpha, ...))
}

skewness <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  abs(apply(x, 2, e1071::skewness, na.rm=na.rm))
}

standard.deviation <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::sd, na.rm=na.rm)
}

trim.mean <- function(x, ...) {
  apply(x, 2, mean, trim=0.2, ...)
}

variance <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::var, na.rm=na.rm)
}
