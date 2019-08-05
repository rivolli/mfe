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
#' @param transform A logical value indicating if the numeric attributes should 
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
#'    \item{"nsRatio"}{Noise ratio, which describes the amount of irrelevant 
#'      information contained in the dataset.}
#'  }
#'  This method uses the unsupervised data discretization procedure provided by
#'  \link[infotheo]{discretize} function, where the default values are used when
#'  \code{transform=TRUE}.
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Donald Michie, David J. Spiegelhalter, Charles C. Taylor, and John Campbell. 
#'  Machine Learning, Neural and Statistical Classification, volume 37. Ellis 
#'  Horwood Upper Saddle River, 1994.
#'
#'  Alexandros Kalousis and Melanie Hilario. Model selection via meta-learning: 
#'  a comparative study. International Journal on Artificial Intelligence Tools,
#'  volume 10, pages 525 - 554, 2001.
#'
#'  Ciro Castiello, Giovanna Castellano, and Anna Maria Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#' @examples
#' ## Extract all metafeatures
#' infotheo(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' infotheo(iris[1:4], iris[5], c("classEnt", "jointEnt"))
#'
#' ## Extract all meta-features without summarize the results
#' infotheo(Species ~ ., iris, summary=c())
#'
#' ## Use another summarization functions
#' infotheo(Species ~ ., iris, summary=c("min", "median", "max"))
#' 
#' ## Do not transform the data (using only categorical attributes)
#' infotheo(Species ~ ., iris, transform=FALSE)
#' @export
infotheo <- function(...) {
  UseMethod("infotheo")
}

#' @rdname infotheo
#' @export
infotheo.default <- function(x, y, features="all", summary=c("mean", "sd"),
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
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
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

#' @rdname infotheo
#' @export
infotheo.formula <- function(formula, data, features="all",
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

  infotheo.default(modFrame[-1], modFrame[1], features, summary, transform, ...)
}

#' List the information theoretical meta-features
#'
#' @return A list of information theoretical meta-features names
#' @export
#'
#' @examples
#' ls.infotheo()
ls.infotheo <- function() {
  c("attrConc", "attrEnt", "classConc", "classEnt", "eqNumAttr", "jointEnt", 
    "mutInf", "nsRatio")
}

ls.infotheo.multiples <- function() {
  c("attrConc", "attrEnt", "classConc", "jointEnt", "mutInf")
}

m.attrConc <- function(x, ...) {
  if (ncol(x) == 1) return(NA)
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]

  mapply(function(i, j) {
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

m.nsRatio <- function(extra, ...) {
  mutinf <- mean(extra$mutinf)
  (mean(extra$x.entropy) - mutinf) / mutinf
}

mutinf <- function(x, y) {
  entropy(x) + entropy(y) - entropy(paste(x, y))
}
