#' Decision Tree Model Based Meta-features
#'
#' Decision Tree (DT) Model Based meta-features are the measures designed to
#' extract characteristics of a DT model induced from a dataset.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"leaves"}{Number of leaves of the DT model.}
#'    \item{"leavesBranch"}{Size of branches, which consists in the level of all
#'    leaves of the DT model (multi-valued).}
#'    \item{"leavesCorrob"}{Leaves corroboration, which is the proportion of
#'    examples that belong to each leaf of the DT model (multi-valued).}
#'    \item{"leavesHomo"}{Homogeneity, which is the number of leaves divided by 
#'    the structural shape of the DT model (multi-valued).}
#'    \item{"leavesPerClass"}{Leaves per class, which is the proportion of 
#'    leaves of the DT model associated with each class (multi-valued).}
#'    \item{"nodes"}{Number of nodes of the DT model.}
#'    \item{"nodesPerAttr"}{Ratio of the number of nodes of the DT model per the
#'    number of attributes.}
#'    \item{"nodesPerInst"}{Ratio of the number of nodes of the DT model per the
#'    number of instances.}
#'    \item{"nodesPerLevel"}{Number of nodes of the DT model per level 
#'    (multi-valued).}
#'    \item{"nodesRepeated"}{Repeated nodes, which is the number of repeated
#'    attributes that appear in the DT model (multi-valued).}
#'    \item{"treeDepth"}{Tree depth, which is the level of all tree nodes and 
#'    leaves of the DT model (multi-valued).}
#'    \item{"treeImbalance"}{Tree imbalance (multi-valued).}
#'    \item{"treeShape"}{Tree shape, which is the probability of arrive in each 
#'    leaf given a random walk. We call this as the structural shape of the DT 
#'    model (multi-valued).}
#'    \item{"varImportance"}{Variable importance. It is calculated using the 
#'    Gini index to estimate the amount of information used in the DT model 
#'    (multi-valued).}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Hilan Bensusan, Christophe Giraud-Carrier, and Claire Kennedy. A 
#'  higher-order approach to meta-learning. In 10th International Conference 
#'  Inductive Logic Programming (ILP), pages 33 - 42, 2000.
#'
#'  Yonghong Peng, Peter A. Flach, Carlos Soares, and Pavel Brazdil. Improved 
#'  dataset characterization for meta-learning. In 5th International Conference 
#'  on Discovery Science (DS), pages 141 - 152, 2002.
#'
#' @examples
#' ## Extract all meta-features using formula
#' model.based(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' model.based(iris[1:4], iris[5], c("nodes", "leaves", "treeShape"))
#'
#' ## Use another summarization function
#' model.based(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
model.based <- function(...) {
  UseMethod("model.based")
}

#' @rdname model.based
#' @export
model.based.default <- function(x, y, features="all",
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
  colnames(x) <- make.names(colnames(x), unique=TRUE)

  data <- cbind(class=y, x)
  model.based.formula(stats::formula(data), data, features, summary, ...)
}

#' @rdname model.based
#' @export
model.based.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame,"terms") <- NULL

  if(min(table(modFrame[,1])) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }
  
  if(features[1] == "all") {
    features <- ls.model.based()
  }
  features <- match.arg(features, ls.model.based(), TRUE)
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  model <- dt(formula, data)
  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- eval(call(fn, model=model, data=data))
    post.processing(measure, summary, f %in% ls.model.based.multiples(), ...)
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
  c("leaves", "leavesBranch", "leavesCorrob", "leavesHomo", "leavesPerClass", 
    "nodes", "nodesPerAttr", "nodesPerInst", "nodesPerLevel", "nodesRepeated", 
    "treeDepth", "treeImbalance", "treeShape", "varImportance")
}

ls.model.based.multiples <- function() {
  c("leavesBranch", "leavesCorrob", "leavesHomo", "leavesPerClass", 
    "nodesPerLevel", "nodesRepeated", "treeDepth", "treeImbalance", 
    "treeShape", "varImportance")
}

m.leaves <- function(model, ...) {
  nrow(model$frame[model$frame$var == "<leaf>",])
}

m.leavesBranch <- function(model, ...) {
  m.treeDepth(model)[model$frame$var == "<leaf>"]
}

m.leavesCorrob <- function(model, data, ...) {
  model$frame$n[model$frame$var == "<leaf>"] / nrow(data)
}

m.leavesHomo <- function(model, ...) {
  m.leaves(model, ...) / m.treeShape(model, ...)
}

m.leavesPerClass <- function(model, ...) {
  as.numeric(table(model$frame[model$frame$var == "<leaf>", "yval"])) / m.leaves(model)
}

m.nodes <- function(model, ...) {
  sum(model$frame$var != "<leaf>")
}

m.nodesPerAttr <- function(model, data, ...) {
  m.nodes(model, ...) / (ncol(data)-1)
}

m.nodesPerInst <- function(model, data, ...) {
  m.nodes(model, ...) / nrow(data)
}

m.nodesPerLevel <- function(model, ...) {
  as.numeric(table(factor(m.treeDepth(model)[model$frame$var != "<leaf>"])))
}

m.nodesRepeated <- function(model, ...) {
  as.numeric(table(factor(model$frame$var[model$frame$var != "<leaf>"])))
}

m.treeDepth <- function(model, ...) {
  nodes <- as.numeric(rownames(model$frame))
  depths <- floor(log2(nodes) + 1e-7)
  depths - min(depths)
}

m.treeImbalance <- function(model, ...) {
  aux <- 1 / 2 ^ m.treeDepth(model)[model$frame$var == "<leaf>"]
  aux <- as.numeric(table(aux)) * sort(unique(aux))
  -(1 / 2 ^ aux) * log2(1 / 2 ^ aux)
}

m.treeShape <- function(model, ...) {
  aux <- m.treeDepth(model)[model$frame$var == "<leaf>"]
  -(1 / 2 ^ aux) * log2(1 / 2 ^ aux)
}

m.varImportance <- function(model, ...) {
  model$variable.importance / sum(model$variable.importance)
}
