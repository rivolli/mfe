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
#'  The details section describes the valid values for this group.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param by.class A logical value indicating if the meta-features must be
#'  computed for each group of samples belonging to different output classes.
#'  (Default: FALSE)
#' @param transform A logical value indicating if the categorical attributes
#'  should be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"canCor"}{Canonical correlations between the predictive attributes 
#'    and the class (multi-valued).}
#'    \item{"gravity"}{Center of gravity, which is the distance between the 
#'    instance in the center of the majority class and the instance-center of 
#'    the minority class.}
#'    \item{"cor"}{Absolute attributes correlation, which measure the 
#'    correlation between each pair of the numeric attributes in the dataset 
#'    (multi-valued). This measure accepts an extra argument called 
#'    \code{method = c("pearson", "kendall", "spearman")}. See 
#'    \code{\link[stats]{cor}} for more details.}
#'    \item{"cov"}{Absolute attributes covariance, which measure the covariance 
#'    between each pair of the numeric attributes in the dataset 
#'    (multi-valued).}
#'    \item{"nrDisc"}{Number of the discriminant functions.}
#'    \item{"eigenvalues"}{Eigenvalues of the covariance matrix (multi-valued).}
#'    \item{"gMean"}{Geometric mean of attributes (multi-valued).}
#'    \item{"hMean"}{Harmonic mean of attributes (multi-valued).}
#'    \item{"iqRange"}{Interquartile range of attributes (multi-valued).}
#'    \item{"kurtosis"}{Kurtosis of attributes (multi-valued).}
#'    \item{"mad"}{Median absolute deviation of attributes (multi-valued).}
#'    \item{"max"}{Maximum value of attributes (multi-valued).}
#'    \item{"mean"}{Mean value of attributes (multi-valued).}
#'    \item{"median"}{Median value of attributes (multi-valued).}
#'    \item{"min"}{Minimum value of attributes (multi-valued).}
#'    \item{"nrCorAttr"}{Number of attributes pairs with high correlation 
#'    (multi-valued when \code{by.class=TRUE}).}
#'    \item{"nrNorm"}{Number of attributes with normal distribution. The 
#'    Shapiro-Wilk Normality Test is used to assess if an attribute is or not is
#'    normally distributed (multi-valued only when \code{by.class=TRUE}).}
#'    \item{"nrOutliers"}{Number of attributes with outliers values. The 
#'    Turkey's boxplot algorithm is used to compute if an attributes has or does 
#'    not have outliers (multi-valued only when \code{by.class=TRUE}).}
#'    \item{"range"}{Range of Attributes (multi-valued).}
#'    \item{"sd"}{Standard deviation of the attributes (multi-valued).}
#'    \item{"sdRatio"}{Statistic test for homogeneity of covariances.}
#'    \item{"skewness"}{Skewness of attributes (multi-valued).}
#'    \item{"sparsity"}{Attributes sparsity, which represents the degree of 
#'    discreetness of each attribute in the dataset (multi-valued).}
#'    \item{"tMean"}{Trimmed mean of attributes (multi-valued). It is the 
#'    arithmetic mean excluding the 20\% of the lowest and highest instances.}
#'    \item{"var"}{Attributes variance (multi-valued).}
#'    \item{"wLambda"}{Wilks Lambda.}
#'  }
#'  This method uses simple binarization to transform the categorical attributes
#'  when \code{transform=TRUE}.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Ciro Castiello, Giovanna Castellano, and Anna M. Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#'  Shawkat Ali, and Kate A. Smith. On learning algorithm selection for 
#'  classification. Applied Soft Computing, volume 6, pages 119 - 138, 2006.
#'
#' @examples
#' ## Extract all meta-features
#' statistical(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' statistical(iris[1:4], iris[5], c("cor", "nrNorm"))
#'
#' ## Extract all meta-features without summarize the results
#' statistical(Species ~ ., iris, summary=c())
#' 
#' ## Use another summarization function
#' statistical(Species ~ ., iris, summary=c("min", "median", "max"))
#'
#' ## Extract statistical measures using by.class approach
#' statistical(Species ~ ., iris, by.class=TRUE)
#' 
#' ## Do not transform the data (using only categorical attributes)
#' statistical(Species ~ ., iris, transform=FALSE)
#' @export
statistical <- function(...) {
  UseMethod("statistical")
}

#' @rdname statistical
#' @export
statistical.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), by.class=FALSE,
                                   transform=TRUE, ...) {
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
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  if (transform) {
    numdata <- binarize(x)
  } else {
    numdata <- x[, sapply(x, is.numeric), drop=FALSE]

    if (length(numdata) == 0) {
      if (by.class) {
        multiples <- c(ls.statistical.multiples(), 
                       setdiff(setdiff(ls.statistical(), 
                                       ls.statistical.multiples()), 
                               ls.statistical.exclude.byclass()))
      } else {
        multiples <- ls.statistical.multiples()
      }
      return(
        sapply(features, function(f) {
          post.processing(NA, summary, f %in% multiples, ...)
        }, simplify=FALSE)
      )
    }
  }
  
  y.num <- binarize(as.data.frame(y))
  x.cov <- stats::cov(numdata)
  
  extra <- list(
    y.num = y.num,
    cancor = tryCatch(
      stats::cancor(numdata, y.num),
      error=function(e) {
        warning(e)
        list(cor=c())
      }),
    x.cov = x.cov,
    eigenvalues = base::eigen(x.cov)
  )

  if(by.class) {
    exclude <- ls.statistical.exclude.byclass()
    measures <- sapply(levels(y), function(class) {
      new.data <- numdata[y==class, , drop=FALSE]
      new.x <- x[y==class, , drop=FALSE]
      nex.xcov <- stats::cov(new.data)
      new.extra <- list(
        x.cov = nex.xcov,
        eigenvalues = base::eigen(nex.xcov)
      )
      
      #new.data <- new.data[, apply(new.data, 2, stats::sd) != 0, drop=FALSE]
      aux <- sapply(features, function(f) {
        fn <- paste("m", f, sep=".")
        if (f %in% exclude) {
          do.call(fn, c(list(x=numdata, y=y, xorig=x, extra=extra), list(...)))
        } else {
          do.call(fn, c(list(x=new.data, xorig=new.x, extra=new.extra), list(...)))
        }
      }, simplify=FALSE)
      
      aux
    }, simplify=FALSE)
    
    sapply(features, function(f) {
      if (f %in% exclude) {
        values <- measures[[1]][[f]]
      } else {
        values <- lapply(measures, function(values) values[[f]])
      }
      
      post.processing(unlist(values), summary, 
                      !f %in% setdiff(exclude, ls.statistical.multiples()), ...)
    }, simplify=FALSE)
  } else {
    sapply(features, function(f) {
      fn <- paste("m", f, sep=".")
      measure <- do.call(fn, c(list(x=numdata, y=y, xorig=x, extra=extra), list(...)))
      post.processing(measure, summary, f %in% ls.statistical.multiples(), ...)
    }, simplify=FALSE)
  }
}

#' @rdname statistical
#' @export
statistical.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), by.class=FALSE,
                                   transform=TRUE, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  statistical.default(modFrame[-1], modFrame[1], features, summary, 
    by.class, transform, ...)
}

#' List the statistical meta-features
#'
#' @return A list of statistical meta-features names
#' @export
#'
#' @examples
#' ls.statistical()
ls.statistical <- function() {
  c("canCor", "gravity", "cor", "cov", "nrDisc", "eigenvalues", "gMean", 
    "hMean", "iqRange", "kurtosis", "mad", "max", "mean", "median", "min", 
    "nrCorAttr", "nrNorm", "nrOutliers", "range", "sd", "sdRatio", "skewness", 
    "sparsity", "tMean", "var", "wLambda")
}

ls.statistical.multiples <- function() {
  c("canCor", "cor", "cov", "eigenvalues", "gMean", "hMean", "iqRange", 
    "kurtosis", "mad", "max", "mean", "median", "min", "range", "sd", 
    "skewness", "sparsity", "tMean", "var")
}

ls.statistical.exclude.byclass <- function() {
  c("canCor", "gravity", "nrDisc", "sdRatio", "wLambda")
}

m.canCor <- function(x, y, extra, ...) {
  if (length(extra$cancor$cor) == 0) return(NA)
  else return(extra$cancor$cor)
}

m.gravity <- function(x, y, ...) {
  classes <- table(y)
  minc <- which.min(classes)
  maxc <- which.max(classes[-minc])
  
  centers <- t(sapply(names(c(minc, maxc)), function(class){
    apply(x[y == class, , drop=FALSE], 2, base::mean)
  }))

  c(stats::dist(centers))
}

m.cor <- function(x, ...) {
  args <- list(...)
  method <- ifelse(is.null(args$method), "pearson", args$method)
  aux <- stats::cor(x, method=method)
  abs(aux[upper.tri(aux)])
}

m.cov <- function(x, y, extra, ...) {
  abs(extra$x.cov[upper.tri(extra$x.cov)])
}

m.nrDisc <- function(x, y, extra, ...) {
  length(extra$cancor$cor)
}

m.eigenvalues <- function(x, y, extra, ...) {
  extra$eigenvalues$values
}

m.gMean <- function(x, ...) {
  res1 <- apply(x, 2, prod)^(1/nrow(x))

  x[x < 1] <- NA
  res2 <- apply(x, 2, function(col) {
    exp(base::mean(log(col), ...))
  })

  coalesce(res1, res2)
}

m.hMean <- function(x, ...) {
  apply(x, 2, function(col) length(col) / sum(1/col))
}

m.iqRange <- function(x, ...) {
  apply(x, 2, stats::IQR)
}

m.kurtosis <- function(x, ...) {
  apply(x, 2, e1071::kurtosis)
}

m.mad <- function(x, ...) {
  apply(x, 2, stats::mad)
}

m.max <- function(x, ...) {
  apply(x, 2, base::max)
}

m.mean <- function(x, ...) {
  apply(x, 2, base::mean)
}

m.median <- function(x, ...) {
  apply(x, 2, stats::median)
}

m.min  <- function(x, ...) {
  apply(x, 2, base::min)
}

m.nrCorAttr <- function(x, ...) {
  sum(abs(m.cor(x, ...)) >= 0.5) / (ncol(x) * (ncol(x) - 1) / 2)
}

m.nrNorm <- function(x, ...) {
  sum(unlist(apply(x, 2, function(col) {
    p.value <- NA
    tryCatch(
      p.value <- stats::shapiro.test(col[seq(min(length(col), 5000))])$p.value, 
      error = function(e) e
    )
    p.value
  })) > 0.1, na.rm = TRUE)
}

m.nrOutliers <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  sum(apply(x, 2, function(x) {
    qs <- stats::quantile(x, na.rm=na.rm)
    iqr <- (qs[4] - qs[2]) * 1.5
    (qs[2] - iqr) > qs[1] | (qs[4] + iqr) < qs[5] 
  }))
}

m.range <- function(x, ...) {
  res <- apply(x, 2, base::range)
  res[2,] - res[1,]
}

m.sd <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::sd, na.rm=na.rm)
}

m.sdRatio <- function(x, y, extra, ...) {
  p <- ncol(x)
  q <- nlevels(y)
  n <- length(y)
  ni <- table(y) - 1
  
  Si <- lapply(levels(y), function(class) stats::cov(x[y == class,, drop=FALSE]))
  S <- Reduce('+', mapply(function(Si, ni) ni*Si, S=Si, n=ni, SIMPLIFY=FALSE)) /
    (n - q)
  
  tryCatch({
    M <- (1 - ((2*p^2+3*p-1)/(6*(p+1)*(q-1))) * (sum(1/ni)-1/(n-q))) *
      ((n - q) * log(det(S)) - sum(ni * log(sapply(Si, det))))
    
    ifelse(is.na(M) | is.infinite(M), NA, exp(M / (p * sum(ni - 1))))
  }, warning = function(e) {
    NA
  })
}

m.skewness <- function(x, ...) {
  apply(x, 2, e1071::skewness)
}

m.sparsity <- function(xorig, ...) {
  (apply(xorig, 2, function(col) base::mean(table(col))) - 1) / (nrow(xorig) - 1)
}

m.tMean <- function(x, ...) {
  apply(x, 2, base::mean, trim=0.2)
}

m.var <- function(x, ...) {
  apply(x, 2, stats::var)
}

m.wLambda <- function(x, y, ...) {
  if (ncol(x) > 1) {
    as.numeric(rrcov::Wilks.test(x, grouping=y)$statistic)
  } else {
    return(NA)
  }
}
