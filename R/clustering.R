#' Clustering Meta-features
#'
#' Clustering measures extract information about validation index.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param transform A logical value indicating if the categorical attributes
#'  should be transformed. If \code{FALSE} they will be ignored. (Default: 
#'  \code{TRUE})
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"vdu"}{Calculate the Dunn Index.}
#'    \item{"vdb"}{Calculate the Davies and Bouldin Index.}
#'    \item{"int"}{Calculate the INT index.}
#'    \item{"sil"}{Calculate the mean silhouette value from data.}
#'    \item{"pb"}{Pearson Correlation between class matching and instance 
#'      distances.}
#'    \item{"ch"}{Calinski and Harabaz index.}
#'    \item{"nre"}{Normalized relative entropy.}
#'    \item{"sc"}{Mean of the number of examples per class.}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references

#'  Bruno A. Pimentel, and Andre C. P. L. F. de Carvalho. A new data 
#'  characterization for selecting clustering algorithms using meta-learning. 
#'  Information Sciences, volume 477, pages 203 - 219, 2019.
#'
#' @examples
#' ## Extract all meta-features using formula
#' clustering(Species ~ ., iris)
#'
#' ## Extract some meta-features
#' clustering(iris[1:4], iris[5], c("vdu", "vdb", "sil"))
#'
#' ## Use another summarization function
#' clustering(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
clustering <- function(...) {
  UseMethod("clustering")
}

#' @rdname clustering
#' @export
clustering.default <- function(x, y, features="all",
                               summary=c("mean", "sd"),
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
    features <- ls.clustering()
  }
  features <- match.arg(features, ls.clustering(), TRUE)
  colnames(x) <- make.names(colnames(x), unique=TRUE)

  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  if(transform) {
    x <- binarize(x)
  } else {
    x <- x[sapply(x, is.numeric)]
  }

  x <- as.matrix(x)
  y <- as.integer(y)

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- eval(call(fn, x=x, y=y))
    post.processing(measure, summary, f %in% ls.clustering.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname clustering
#' @export
clustering.formula <- function(formula, data, features="all",
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

  clustering.default(modFrame[-1], modFrame[1], features, summary, transform, 
    ...)
}

#' List the best clustering meta-features
#'
#' @return A list of best neighbor meta-features names.
#' @export
#'
#' @examples
#' ls.clustering()
ls.clustering <- function() {
  c("vdu", "vdb", "int", "sil", "pb", "ch", "nre", "sc")
}

ls.clustering.multiples <- function() {
  c()
}

m.vdu <- function(x, y) {
  aux <- clusterCrit::intCriteria(x, y, "Dunn")
  aux$dunn
}

m.vdb <- function(x, y) {
  aux <- clusterCrit::intCriteria(x, y, "Davies_Bouldin")
  aux$davies_bouldin
}

m.int <- function(x, y) {

  dfs <- ovo(x, factor(y))
  dst <- lapply(dfs, function(i) {
    dist(i$x)
  })

  aux <- mapply(function(dfs, dst) {
    inter(dfs, dst)
  }, dfs=dfs, dst=dst)

  c <- length(unique(y))
  aux <- sum(aux)/(c*(c-1)/2)
  return(aux)
}

m.sil <- function(x, y) {
  aux <- clusterCrit::intCriteria(x, y, "Silhouette")
  aux$silhouette
}

m.pb <- function(x, y) {
  aux <- clusterCrit::intCriteria(x, y, "Point_Biserial")
  aux$point_biserial
}

m.ch <- function(x, y) {
  aux <- clusterCrit::intCriteria(x, y, "Calinski_Harabasz")
  aux$calinski_harabasz
}

m.nre <- function(x, y) {
  aux <- table(y)/length(y)
  -sum(aux * log2(aux))
}

m.sc <- function(x, y) {
  mean(table(y))
}
