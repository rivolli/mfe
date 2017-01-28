#' Decision Tree Model Based Meta-features
#'
#' Decision Tree (DT) Model Based meta-features are the measures desined to
#' extract characteristics like the depth, the shape and size of a DT model
#' induced from a dataset.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of methods to summarize the results as post-processing
#'  functions. See \link{post.processing} method to more information. (Default:
#'  \code{c("mean", "sd")})
#' @param ... Optional arguments to the summary methods.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"average.leaf.corrobation"}{Represents the.number of examples that
#'      belong to each leaf in the DT model divided by the number of examples in
#'      the dataset.}
#'    \item{"branch.length"}{Represents the.size of each leaf in the DT model.}
#'    \item{"depth"}{Represents the size of each path in the DT model.}
#'    \item{"homogeneity"}{Represents the number of leaves divided by the
#'      strutural shape of the DT model.}
#'    \item{"max.depth"}{Represents the size of the longest path of the DT
#'      model.}
#'    \item{"nleave"}{Represents the number of leaves of the DT model.}
#'    \item{"nnode"}{Represents the number of nodes in the DT model.}
#'    \item{"nodes.per.attribute"}{Represents the number of nodes in the DT
#'      model divided by the number of predictive attributes.}
#'    \item{"nodes.per.instance"}{Represents the number of leaves in the DT
#'      model divided by the number of examples in the dataset.}
#'    \item{"nodes.level"}{Represents the number of nodes per level.}
#'    \item{"repeated.nodes"}{Represents the number of repeated attributes that
#'      appear in the DT model.}
#'    \item{"shape"}{Represents the probability of arrive in each leaf given a
#'      random walk. We call this as the strutural shape of the DT model.}
#'    \item{"variable.importance"}{Represents the variable importance calculated
#'      by Gini index to construct the DT model.}
#'  }
#' @return Each one of these meta-features generate multiple values (by leaves
#'  and/or nodes) and then it is post processed by the summary methods.
#'  See the \link{post.processing} method for more details about it.
#'
#' @references
#'  Bensusan, H., Giraud-Carrier, C. G., & Kennedy, C. J. (2000). A Higher-order
#'  Approach to Meta-learning. In Proceedings of the 10th International
#'  Conference on Inductive Logic Programming (Vol. 35, pp. 1-10).
#'
#'  Peng, Y., Flach, P. A., Soares, C., & Brazdil, P. (2002). Improved Dataset
#'  Characterisation for Meta-learning. In Proceedings of the 5th International
#'  Conference on Discovery Science (Vol 2534, pp. 141-152)
#'
#' @examples
#' ## Extract all meta-features using formula
#' mf.model.based(Species ~ ., iris)
#'
#' ## Extract all meta-features using data.frame
#' mf.model.based(iris[1:4], iris[5])
#'
#' ## Extract some meta-features
#' mf.model.based(Species ~ ., iris, features=c("nnode", "nleave", "depth"))
#'
#' ## Extract all meta-features with different summary methods
#' mf.model.based(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
mf.model.based <- function(...) {
  UseMethod("mf.model.based")
}

#' @rdname mf.model.based
#' @export
mf.model.based.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  y <- as.data.frame(y)

  if(nrow(x) != nrow(y)) {
    stop("x and y must have same number of rows")
  }

  data <- cbind(Class=y, x)
  formula <- stats::as.formula(paste(colnames(data[1]), "~."))
  mf.model.based.formula(formula, data, features, summary, ...)
}

#' @rdname mf.model.based
#' @export
mf.model.based.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame,"terms") <- NULL

  if(features[1] == "all") {
    features <- ls.model.based()
  }
  features <- match.arg(features, ls.model.based(), TRUE)

  model <- dt.model(formula, data)
  sapply(features, function(f) {
    measure <- eval(call(f, model=model, data=data))
    post.processing(measure, summary)
  }, simplify=FALSE)
}

#' List the DT model based meta-features
#'
#' @return A list of DT model based meta-features names
#' @export
#'
#' @examples
#' ls.model.based()
ls.model.based <- function() {
  c("average.leaf.corrobation", "branch.length", "depth", "homogeneity",
    "max.depth", "nleave", "nnode", "nodes.per.attribute", "nodes.per.instance",
    "nodes.per.level", "repeated.nodes", "shape", "variable.importance")
}

dt.model <- function(formula, data, ...) {
  rpart::rpart(formula, data, method="class",
    control=rpart::rpart.control(maxsurrogate=0))
}

nnode <- function(model, ...) {
  sum(model$frame$var != "<leaf>")
}

nodes.per.attribute <- function(model, data, ...) {
  nnode(model, ...) / (ncol(data)-1)
}

nodes.per.instance <- function(model, data, ...) {
  nnode(model, ...) / nrow(data)
}

average.leaf.corrobation <- function(model, data, ...) {
  model$frame$n[model$frame$var == "<leaf>"] / nrow(data)
}

variable.importance <- function(model, ...) {
  aux <- attr(model$terms, "term.labels")
  aux <- stats::setNames(rep(0, length(aux)), aux)
  aux[names(model$variable.importance)] <- model$variable.importance
  return(aux)
}

depth <- function(model, ...) {
  nodes <- as.numeric(rownames(model$frame))
  depths <- floor(log2(nodes) + 1e-7)
  return(depths - min(depths))
}

max.depth <- function(model, ...) {
  max(depth(model, ...))
}

repeated.nodes <- function(model, data, ...) {
  aux <- attr(model$terms, "term.labels")
  aux <- stats::setNames(rep(0, length(aux)), aux)
  tmp <- table(factor(model$frame$var[model$frame$var != "<leaf>"]))
  aux[names(tmp)] <- tmp
  return(aux)
}

shape <- function(model, ...) {
  aux <- depth(model)[model$frame$var == "<leaf>"]
  prob <- -(1 / 2 ^ aux) * log2(1 / 2 ^ aux)
  return(prob)
}

nleave <- function(model, ...) {
  nrow(model$frame[model$frame$var == "<leaf>",])
}

homogeneity <- function(model, ...) {
  nleave(model, ...) / shape(model, ...)
}

branch.length <- function(model, ...) {
  depth(model)[model$frame$var == "<leaf>"]
}

nodes.per.level <- function(model, ...) {
  aux <- depth(model)[model$frame$var != "<leaf>"]
  if(length(aux) <= 1) {
    return(c(0, 0))
  }
  sapply(0:max(aux), function(d) {
    sum(aux == d)
  })
}
