#' Statistical meta-features
#'
#' Statistical meta-features are the standard statistical measures to
#' describe the numerical properties of a distribution of data. As it requires
#' only numerical attributes, the categorical data are transformed to numerical.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The details section describes the valid values for this parameter.
#' @param summary A list of methods to summarize the results as post-processing
#'  functions. See \link{post.processing} method to more information. (Default:
#'  \code{c("mean", "sd")})
#' @param by.class A logical value indicating if the meta-features must be
#'  computed for each group of samples belonging to different output classes.
#'  (Default: TRUE)
#' @param ... Optional arguments to the summary methods.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"correlation"}{Represents the absolute correlation between each pair
#'       of the numeric attributes in the dataset.}
#'    \item{"covariance"}{Represents the absolute covariance between each pair
#'       of the numeric attributes in the dataset.}
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
#'    \item{"normality"}{Represents the number of attributes that have a normal
#'       distribution of values. For that it is used the Shapiro-Wilk Normality
#'       Test with \code{alpha=0.05}.}
#'    \item{"outliers"}{"Represents the proportion of numeric attributes with
#'      outliers."}
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

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.statistical()
  }
  features <- match.arg(features, ls.statistical(), TRUE)
  numdata <- replace.nominal.columns(x) #TODO control by user parameter

  if(by.class) {
    measures <- lapply(unique(y), function(class) {
      new.data <- numdata[y==class, ]
      #Remove constant columns
      new.data <- new.data[, apply(new.data, 2, stats::sd) != 0]

      sapply(features, function(f) {
        eval(call(f, x=new.data))
      }, simplify=FALSE)
    })

    sapply(features, function(f) {
      #TODO alternatives: mean by attribute or class
      values <- unlist(lapply(measures, function (values) values[[f]]))

      post.processing(values, summary, ...)
    }, simplify=FALSE)
  }else {
    sapply(features, function(f) {
      measure <- eval(call(f, x=numdata))
      post.processing(measure, summary, ...)
    }, simplify=FALSE)
  }
}

#' @rdname mf.statistical
#' @export
mf.statistical.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"),
                                   by.class=TRUE, ...) {
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
  #TODO index of dispersion
  #TODO z-score, normal cumulative distribution, Chi-square test
  #TODO degree of fuzziness,
}

correlation <- function(x, ...) {
  aux <- stats::cor(x)
  values <- abs(aux[upper.tri(aux)])
  if (length(values)) {
    values <- c(values, values)
  }
  values
}

covariance <- function(x, ...) {
  aux <- stats::cov(x)
  #TODO include diag
  values <- abs(aux[upper.tri(aux)])
  if (length(values)) {
    values <- c(values, values)
  }
  values
}

discreteness.degree <- function(x, ...) {
  sapply(x, function(col) mean(table(col)))
}

geometric.mean <- function(x, ...) {
  #TODO http://r.789695.n4.nabble.com/
  #geometric-mean-to-handle-large-number-and-negative-values-td885574.html
  apply(x, 2, function(col) {
    if(all(col > 0)) {
      exp(mean(log(col)))
    } else {
      0
    }
  })
}

harmonic.mean <- function(x, ...) {
  sapply(x, function(col) length(col) / sum(1/col))
}

iqr <- function(x, ...) {
  apply(x, 2, stats::IQR) / apply(x, 2, stats::sd)
}

kurtosis <- function(x, ...) {
  abs(apply(x, 2, e1071::kurtosis))
}

mad <- function(x, ...) {
  apply(x, 2, stats::mad)
}

normality <- function(x, ...) {
  #TODO original method uses Kolmogorov-Smirnov test
  res <- apply(x, 2, function(col) {
    #shapiro.test requires sample size between 3 and 5000
    stats::shapiro.test(sample(col, min(length(col), 5000)))$p.value
  })
  sum(res < 0.05)
}

outliers <- function(x, alpha=0.05, ...) {
  #Johannes, F., & Petrak, J. (2002). Extended Data Characteristics.
  values <- apply(x, 2, function(x) mean(x)/mean(x, trim=alpha) < 0.7)
  sum(values / ncol(x), na.rm=TRUE)
}

skewness <- function(x, ...) {
  abs(apply(x, 2, e1071::skewness))
}

standard.deviation <- function(x, ...) {
  apply(x, 2, stats::sd)
}

trim.mean <- function(x, ...) {
  apply(x, 2, mean, trim=0.2)
}

variance <- function(x, ...) {
  apply(x, 2, stats::var)
}
