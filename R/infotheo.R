#' Information-theoretic meta-features
#'
#' Information-theoretic meta-features are particularly appropriate to describe
#' discrete (categorical) attributes, but they also fit continuous ones so a
#' discretization is required.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#'  The supported values are described in the details section. (Default: 
#'  \code{"all"})
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param transform A logical value indicading if the numeric attributes should 
#'  be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details 
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"attrConc"}{Attributes concentration. It is the Goodman and 
#'      Kruskal's tau measure otherwise known as the concentration coefficient
#'      computed for each pair of attributes (multi-valued).}
#'    \item{"attrEnt"}{Attributes entropy, a measure of randomness of each 
#'      attributes in the dataset (multi-valued).}
#'    \item{"classConc"}{Class concentration, similar to "attrConc", however, it
#'      is computed for each attribute and the class (multi-valued).}
#'    \item{"classEnt"}{Class entropy, which describes how much information is 
#'      necessary to specify the class in the dataset.}
#'    \item{"eqNumAttr"}{Equivalent number of attributes, which represents the 
#'      number of attributes suitable to optimally solve the classification task 
#'      using the dataset.}
#'    \item{"jointEnt"}{Joint entropy, which represents the total entropy of 
#'      each attribute and the class (multi-valued).}
#'    \item{"mutInf"}{Mutual information, that is the common information shared
#'      between each attribute and the class in the dataset (multi-valued).}
#'    \item{"normAttrEnt"}{Normalized attribute entropy, a normalized version of
#'      "attrEnt" (multi-valued).}
#'    \item{"normClassEnt"}{Normalized class entropy, a normalized version of
#'      "classEnt".}
#'    \item{"nsRatio"}{Noise ratio, which describes the amount of irrelevant 
#'      information contained in the dataset.}
#'  }
#'  This method uses the unsupervized data discretization procedure provided by
#'  \link[infotheo]{discretize} function, where the default values are used when 
#'  \code{transform=TRUE}.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Michie, E. D., Spiegelhalter, D. J., & Taylor, C. C. (1994).
#'    Machine Learning , Neural and Statistical Classification.
#'    Technometrics, 37(4), 459.
#'
#'  Kalousis, A., & Hilario, M. (2001). MODEL SELECTION VIA META-LEARNING: A
#'    COMPARATIVE STUDY. International Journal on Artificial Intelligence Tools,
#'    10(4), 525-554.
#'
#'  Castiello, C., Castellano, G., & Fanelli, A. M. (2005). Meta-data:
#'    Characterization of Input Features for Meta-learning. In Proceedings of
#'    the 2nd International Conference on Modeling Decisions for Artificial
#'    Intelligence (Vol. 3558, pp. 457-468).
#'
#' @examples
#' ## Extract all metafeatures
#' mf.infotheo(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' mf.infotheo(iris[1:4], iris[5], c("classEnt", "jointEnt"))
#'
#' ## Extract all meta-features without summarize the results
#' mf.infotheo(Species ~ ., iris, summary=c())
#'
#' ## Use another summarization functions
#' mf.infotheo(Species ~ ., iris, summary=c("min", "median", "max"))
#' 
#' ## Do not transform the data (using only categorical attributes)
#' mf.infotheo(Species ~ ., iris, transform=FALSE)
#' @export
mf.infotheo <- function(...) {
  UseMethod("mf.infotheo")
}

#' @rdname mf.infotheo
#' @export
mf.infotheo.default <- function(x, y, features="all", summary=c("mean", "sd"),
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
  
  if (nlevels(y) > length(y) / 10) {
    stop("y must contain classes values")
  }

  if(features[1] == "all") {
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  if (transform) {
    x.dis <- categorize(x)
  } else {
    x.dis <- x[, !sapply(x, is.numeric), drop=FALSE]
    
    if (length(x.dis) == 0) {
      return(
        sapply(features, function(f) {
          post.processing(NA, summary, f %in% ls.infotheo.multiples(), ...)
        }, simplify=FALSE)
      )
    }
  }
  
  #Remove constant attributes
  x.dis <- x.dis[, sapply(x.dis, nlevels) > 1, drop=FALSE]
  
  extra <- list(
    y.entropy = entropy(y),
    y.log = base::log2(nlevels(y)),
    x.entropy = sapply(x.dis, entropy),
    x.log = sapply(sapply(x.dis, nlevels), base::log2),
    mutinf = sapply(x.dis, mutinf, y=y)
  )

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, c(list(x=x.dis, y=y, extra=extra), list(...)))
    post.processing(measure, summary, f %in% ls.infotheo.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname mf.infotheo
#' @export
mf.infotheo.formula <- function(formula, data, features="all",
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

  mf.infotheo.default(modFrame[, -1], modFrame[, 1], features, summary, 
                      transform, ...)
}

#' List the information theoretical meta-features
#'
#' @return A list of information theoretical meta-features names
#' @export
#'
#' @examples
#' ls.infotheo()
ls.infotheo <- function () {
  c("attrConc", "attrEnt", "classConc", "classEnt",
    "eqNumAttr", "jointEnt", "mutInf",  "normAttrEnt", "normClassEnt", "nsRatio")
}

ls.infotheo.multiples <- function () {
  c("attrConc", "attrEnt", "classConc", "jointEnt", "mutInf", "normAttrEnt")
}

m.attrConc <- function(x, ...) {
  if (ncol(x) == 1) return(NA)
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]

  mapply(function (i, j) {
    concentration.coefficient(x[, i], x[, j])
  }, i=comb$i, j=comb$j)
}

m.attrEnt <- function(extra, ...) {
  extra$x.entropy
}

m.classConc <- function(x, y, ...) {
  apply(x, 2, concentration.coefficient, y)
}

m.classEnt <- function(extra, ...) {
  extra$y.entropy
}

m.eqNumAttr <- function(extra, ...) {
  extra$y.entropy / mean(extra$mutinf)
}

m.jointEnt <- function(x, y, ...) {
  joint.data <- sapply(as.data.frame(sapply(x, paste, y)), as.factor)
  sapply(as.data.frame(joint.data), entropy)
}

m.mutInf <- function(extra, ...) {
  extra$mutinf
}

m.normAttrEnt <- function(extra, ...) {
  extra$x.entropy / extra$x.log
}

m.normClassEnt <- function(extra, ...) {
  extra$y.entropy / extra$y.log
}

m.nsRatio <- function(extra, ...) {
  mutinf <- mean(extra$mutinf)
  (mean(extra$x.entropy) - mutinf) / mutinf
}

concentration.coefficient <- function(x, y) {
  nij <- table(y, x) / length(x)
  isum <- rowSums(nij)
  jsum2 <- sum(colSums(nij)^2)
  nij2 <- nij^2
  
  (sum(nij2 / isum) - jsum2) / (1 - jsum2)
}

entropy <- function(x) {
  qi <- table(x) / length(x)
  -sum(qi * sapply(qi, log2))
}

mutinf <- function(x, y) {
  entropy(x) + entropy(y) - entropy(paste(x, y))
}