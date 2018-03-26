#' General meta-features
#'
#' General meta-features include general information related to the dataset. It
#' is also known as simple measures.
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
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#' @param ... Not used.
#' 
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"attrToInst"}{Ratio of the number of attributes per the number of 
#'    instances, also known as dimensionality.}
#'    \item{"catToNum"}{Ratio of the number of categorical attributes per the 
#'    number of numeric attributes.}
#'    \item{"defError"}{Default error.}
#'    \item{"instPerAttr"}{Ratio of the number of instances per the number of 
#'    attributes.}
#'    \item{"ntAttr"}{Number of attributes.}
#'    \item{"nrBin"}{Number of binary attributes.}
#'    \item{"nrCat"}{Number of categorical attributes.}
#'    \item{"nrClass"}{Number of classes.}
#'    \item{"nrInst"}{Number of instances.}
#'    \item{"nrNum"}{Number of numeric attributes.}
#'    \item{"numToCat"}{Ratio of the number of numeric attributes per the number
#'    of categorical attributes.}
#'    \item{"propBin"}{Proportion of binary attributes.}
#'    \item{"propCat"}{Proportion of categorical attributes.}
#'    \item{"propClass"}{Proportion of the classes values (multi-valued).}
#'    \item{"propNum"}{Proportion of numeric attributes.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Michie, E. D., Spiegelhalter, D. J., & Taylor, C. C. (1994).
#'    Machine Learning , Neural and Statistical Classification.
#'    Technometrics, 37(4), 459.
#'
#'  Lindner, G., & Studer, R. (1999). AST: Support for Algorithm Selection with
#'    a CBR Approach. Principles of Data Mining and Knowledge Discovery, 1704,
#'    418-423.
#'
#'  Castiello, C., Castellano, G., & Fanelli, A. M. (2005). Meta-data:
#'    Characterization of Input Features for Meta-learning. In Proceedings of
#'    the 2nd International Conference on Modeling Decisions for Artificial
#'    Intelligence (Vol. 3558, pp. 457-468).
#'
#'
#' @examples
#' ## Extract all metafeatures
#' mf.general(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' mf.general(iris[1:100, 1:4], iris[1:100, 5], c("defError", "nrClass"))
#' 
#' ## Extract all meta-features without summarize prop.class
#' mf.general(Species ~ ., iris, summary=c())
#' 
#' ## Use another summarization functions
#' mf.general(Species ~ ., iris, summary=c("sd","min","iqr"))
#' @export
mf.general <- function(...) {
  UseMethod("mf.general")
}

#' @rdname mf.general
#' @export
mf.general.default <- function(x, y, features="all", summary=c("mean", "sd"), 
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
  
  if (nlevels(y) > length(y) / 10) {
    stop("y must contain classes values")
  }

  if(features[1] == "all") {
    features <- ls.general()
  }
  features <- match.arg(features, ls.general(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, c(list(x=x, y=y), list(...)))
    post.processing(measure, summary, f %in% ls.general.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname mf.general
#' @export
mf.general.formula <- function(formula, data, features="all", 
                               summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  mf.general.default(modFrame[, -1], modFrame[, 1], features, summary, ...)
}

#' List the general meta-features
#'
#' @return A list of general meta-features names
#' @export
#'
#' @examples
#' ls.general()
ls.general <- function() {
  c("attrToInst", "catToNum", "defError", "instToAttr", "nrAttr", "nrBin", 
    "nrCat", "nrClass", "nrInst", "nrNum",  "numToCat", "propBin", "propCat", 
    "propClass", "propNum")
}

ls.general.multiples <- function() {
  c("propClass")
}

#Meta-features
m.attrToInst <- function(x, ...) {
  m.nrAttr(x) / m.nrInst(x)
}

m.catToNum <- function (x, ...) {
  nnum <- m.nrNum(x)
  if (nnum == 0) return(NA)
  m.nrCat(x) / nnum
}

m.defError <- function(y, ...) {
  1 - (max(table(y)) / length(y))
}

m.instToAttr <- function(x, ...) {
  m.nrInst(x) / m.nrAttr(x)
}

m.nrAttr <- function(x, ...) {
  ncol(x)
}

m.nrBin <- function (x, ...) {
  sum(apply(x, 2, function (col) length(table(col)) == 2))
}

m.nrCat <- function(x, ...) {
  m.nrAttr(x) - m.nrNum(x)
}

m.nrClass <- function(y, ...) {
  length(unique(y))
}

m.nrInst <- function(x, ...) {
  nrow(x)
}

m.nrNum <- function(x, ...) {
  sum(sapply(x, is.numeric))
}

m.numToCat <- function (x, ...) {
  ncat <- m.nrCat(x)
  if (ncat == 0) return(NA)
  m.nrNum(x) / ncat
}

m.propBin <- function (x, ...) {
  m.nrBin(x) / m.nrAttr(x)
}

m.propCat <- function(x, ...) {
  m.nrCat(x) / m.nrAttr(x)
}

m.propClass <- function(y, ...) {
  as.numeric(table(y)) / length(y)
}

m.propNum <- function(x, ...) {
  m.nrNum(x) / m.nrAttr(x)
}