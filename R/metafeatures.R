#' Extract meta-features from a dataset
#'
#' This is a simple way to extract the meta-features from a dataset, where all
#' meta-features from each group is extracted.
#'
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param groups A list of meta-features groups or \code{"all"} to include all
#'  them. The details section describes the valid values for this parameter.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @param ... Optional arguments to the summary methods.
#' @details
#'  The following groups are allowed for this method:
#'  \describe{
#'    \item{"infotheo"}{Include all information theoretical meta-features. See
#'      \link{infotheo} for more details.}
#'    \item{"general"}{Include all general (simple) meta-features. See
#'      \link{general} for more details.}
#'    \item{"landmarking"}{Include all landmarking meta-features. See
#'      \link{landmarking} for more details.}
#'    \item{"model.based"}{Include all model based meta-features. See
#'      \link{model.based} for more details.}
#'    \item{"statistical"}{Include all statistical meta-features. See
#'      \link{statistical} for more details.}
#'    \item{"clustering"}{Include all clustering meta-features. See
#'      \link{clustering} for more details.}
#'  }
#'
#' @return A numeric vector named by the meta-features from the specified 
#' groups.
#' @export
#'
#' @examples
#' ## Extract all meta-features
#' metafeatures(Species ~ ., iris)
#'
#' ## Extract some groups of meta-features
#' metafeatures(iris[1:4], iris[5], c("general", "statistical", "infotheo"))
#'
#' ## Use another summary methods
#' metafeatures(Species ~ ., iris, summary=c("min", "median", "max"))
metafeatures <- function(...) {
  UseMethod("metafeatures")
}

#' @rdname metafeatures
#' @export
metafeatures.default <- function(x, y, groups="all",
                                 summary=c("mean", "sd"), ...) {
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

  if(groups[1] == "all") {
    groups <- ls.metafeatures()
  }
  groups <- match.arg(groups, ls.metafeatures(), TRUE)
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  unlist(sapply(groups, function(group) {
    do.call(paste(group, "default", sep='.'),
            list(x=x, y=y, summary=summary, ...))
  }, simplify = FALSE))
}

#' @rdname metafeatures
#' @export
metafeatures.formula <- function(formula, data, groups="all",
                                 summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  metafeatures.default(modFrame[-1], modFrame[1], groups, summary, ...)
}

#' List the meta-features groups
#'
#' @return A list of meta-features groups
#' @export
#'
#' @examples
#' ls.metafeatures()
ls.metafeatures <- function() {
  c("general", "statistical", "infotheo", "model.based", "landmarking", 
    "relative", "clustering")
}
