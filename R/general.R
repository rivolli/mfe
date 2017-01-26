#' General meta-features
#'
#' General meta-features include general information related to the dataset at
#' hand. It is also known as simple measures.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y a factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param ... Not used
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"defective.instances"}{Represents the proportion of instances with
#'      missing values in the dataset.}
#'    \item{"dimensionality"}{Represents the ratio between the number of
#'      attributes and the number of instances constituting the dataset.}
#'    \item{"majority.class"}{Represents the proportion of instances that
#'      belongs to the majority class. It is also known as default accuracy.}
#'    \item{"missing.values"}{Represents the proportion of missing values in
#'      the dataset.}
#'    \item{"nattribute"}{Represents the total number of attributes in the
#'      dataset.}
#'    \item{"nbinary"}{Represents the total of binary attributes in the
#'      dataset. It includes categorical and numeric values that have just 2
#'      different values.}
#'    \item{"nclasse"}{Represents the total number of output values (classes) in
#'      the dataset.}
#'    \item{"ninstance"}{Represents the total number of instances (also named
#'      samples or observations) in the dataset.}
#'    \item{"nnumeric"}{Represents the number of numeric attributes in the
#'      dataset.}
#'    \item{"nsymbolic"}{Represents the number of categorical attributes in the
#'      dataset. This is the same as the number of factor columns.}
#'    \item{"pbinary"}{Represents the proportion of binary attributes in the
#'      dataset.}
#'    \item{"pnumeric"}{Represents the proportion of numeric attributes in the
#'      dataset.}
#'    \item{"psymbolic"}{Represents the proportion of symbolic attributes in the
#'      dataset.}
#'    \item{"sdclass"}{Represents the standard deviation of the class
#'      distribution in the dataset.}
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
#' small.iris <- iris[1:100, ]
#' mf.general(small.iris[1:4], small.iris[5], c("nclasse", "dimensionality"))
#' @export
mf.general <- function(...) {
  UseMethod("mf.general")
}

#' @rdname mf.general
#' @export
mf.general.default <- function(x, y, features="all", ...) {
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

  sapply(features, function(f) {
    eval(call(f, x=x, y=y))
  }, simplify=FALSE)
}

#' @rdname mf.general
#' @export
mf.general.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  mf.general.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' List the general meta-features
#'
#' @return A list of general meta-features names
#' @export
#'
#' @examples
#' ls.general()
ls.general <- function() {
  c("defective.instances", "dimensionality", "majority.class", "missing.values",
    "nattribute", "nbinary", "nclasse", "ninstance", "nnumeric", "nsymbolic",
    "pbinary", "pnumeric", "psymbolic", "sdclass")
}

defective.instances <- function(x, ...) {
  sum(apply(x, 1, anyNA)) / nrow(x)
}

dimensionality <- function(x, ...) {
  nattribute(x) / ninstance(x)
}

majority.class <- function(y, ...) {
  max(table(y)) / length(y)
}

missing.values <- function(x, ...) {
  sum(is.na(x)) / (nrow(x) * ncol(x))
}

nattribute <- function(x, ...) {
  ncol(x)
}

nbinary <- function (x, ...) {
  sum(apply(x, 2, function (col) length(table(col)) == 2))
}

nclasse <- function(y, ...) {
  nlevels(y)
}

ninstance <- function(x, ...) {
  nrow(x)
}

nnumeric <- function(x, ...) {
  sum(sapply(x, is.numeric))
}

nsymbolic <- function(x, ...) {
  sum(sapply(x, is.factor))
}

pbinary <- function (x, ...) {
  nbinary(x) / nattribute(x)
}

pnumeric <- function(x, ...) {
  nnumeric(x) / nattribute(x)
}

psymbolic <- function(x, ...) {
  nsymbolic(x) / nattribute(x)
}

sdclass <- function(y, ...) {
  stats::sd(table(y) / length(y))
}
