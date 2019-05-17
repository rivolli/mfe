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
#'    \item{"freqClass"}{Proportion of the classes values (multi-valued).}
#'    \item{"instToAttr"}{Ratio of the number of instances per the number of 
#'    attributes.}
#'    \item{"nrAttr"}{Number of attributes.}
#'    \item{"nrBin"}{Number of binary attributes.}
#'    \item{"nrCat"}{Number of categorical attributes.}
#'    \item{"nrClass"}{Number of classes.}
#'    \item{"nrInst"}{Number of instances.}
#'    \item{"nrNum"}{Number of numeric attributes.}
#'    \item{"numToCat"}{Ratio of the number of numeric attributes per the number
#'    of categorical attributes.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Donald Michie, David J. Spiegelhalter, Charles C. Taylor, and John Campbell.
#'  Machine Learning, Neural and Statistical Classification, volume 37. Ellis 
#'  Horwood Upper Saddle River, 1994.
#'
#'  Guido Lindner and Rudi Studer. AST: Support for algorithm selection with a 
#'  CBR approach. In European Conference on Principles of Data Mining and 
#'  Knowledge Discovery (PKDD), pages 418 - 423, 1999.
#'
#'  Ciro Castiello, Giovanna Castellano, and Anna M. Fanelli. Meta-data: 
#'  Characterization of input features for meta-learning. In 2nd International 
#'  Conference on Modeling Decisions for Artificial Intelligence (MDAI), 
#'  pages 457 - 468, 2005.
#'
#' @examples
#' ## Extract all metafeatures
#' general(Species ~ ., iris)
#'
#' ## Extract some metafeatures
#' general(iris[1:100, 1:4], iris[1:100, 5], c("nrAttr", "nrClass"))
#' 
#' ## Extract all meta-features without summarize prop.class
#' general(Species ~ ., iris, summary=c())
#' 
#' ## Use another summarization functions
#' general(Species ~ ., iris, summary=c("sd","min","iqr"))
#' @export
general <- function(...) {
  UseMethod("general")
}

#' @rdname general
#' @export
general.default <- function(x, y, features="all", summary=c("mean", "sd"), 
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
    features <- ls.general()
  }
  features <- match.arg(features, ls.general(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, c(list(x=x, y=y), list(...)))
    post.processing(measure, summary, f %in% ls.general.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname general
#' @export
general.formula <- function(formula, data, features="all", 
                               summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  general.default(modFrame[-1], modFrame[1], features, summary, ...)
}

#' List the general meta-features
#'
#' @return A list of general meta-features names
#' @export
#'
#' @examples
#' ls.general()
ls.general <- function() {
  c("attrToInst", "catToNum", "freqClass", "instToAttr", "nrAttr", "nrBin", 
    "nrCat", "nrClass", "nrInst", "nrNum",  "numToCat")
}

ls.general.multiples <- function() {
  c("freqClass")
}

#Meta-features
m.attrToInst <- function(x, ...) {
  m.nrAttr(x) / m.nrInst(x)
}

m.catToNum <- function(x, ...) {
  nnum <- m.nrNum(x)
  if (nnum == 0) return(NA)
  m.nrCat(x) / nnum
}

m.freqClass <- function(y, ...) {
  as.numeric(table(y)) / length(y)
}

m.instToAttr <- function(x, ...) {
  m.nrInst(x) / m.nrAttr(x)
}

m.nrAttr <- function(x, ...) {
  ncol(x)
}

m.nrBin <- function(x, ...) {
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

m.numToCat <- function(x, ...) {
  ncat <- m.nrCat(x)
  if (ncat == 0) return(NA)
  m.nrNum(x) / ncat
}
