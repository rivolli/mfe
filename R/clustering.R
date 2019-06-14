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
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"vdb"}{}
#'    \item{"int"}{}
#'    \item{"sil"}{}
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
#' clustering(iris[1:4], iris[5], c("vdb", "int", "sil"))
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
                                   summary=c("mean", "sd"), ...) {
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

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- eval(call(fn, x=x, y=y))
    post.processing(measure, summary, f %in% ls.clustering.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname clustering
#' @export
clustering.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  clustering.default(modFrame[-1], modFrame[1], features, summary, ...)
}

#' List the best clustering meta-features
#'
#' @return A list of best neighbor meta-features names.
#' @export
#'
#' @examples
#' ls.clustering()
ls.clustering <- function() {
  c("vdb", "int", "sil")
}

ls.clustering.multiples <- function() {
  c()
}

m.vdb <- function(x, y) {

  data <- ovo(x, y)
  dst <- lapply(data, function(i) {
    dist(i$x)
  })

  aux <- mapply(function(data, dst) {
    l <- levels(data$y)
    (intra(data, dst, l[1]) + intra(data, dst, l[2]))/inter(data, dst)
  }, data=data, dst=dst)

  c <- levels(y)
  vet <- utils::combn(c, 2)
  tmp <- sapply(c, function(i) {
    max(aux[apply(vet == i, 2, any)])
  })

  aux <- sum(tmp)/length(c)
  return(aux)
}

m.int <- function(x, y) {

  dfs <- ovo(x, y)
  dst <- lapply(dfs, function(i) {
    dist(i$x)
  })

  aux <- mapply(function(dfs, dst) {
    inter(dfs, dst)
  }, dfs=dfs, dst=dst)

  c <- nlevels(y)
  aux <- sum(aux)/(c*(c-1)/2)
  return(aux)
}

m.sil <- function(x, y) {

  dst <- dist(x)
  aux <- mean(cluster::silhouette(as.numeric(y), dst)[,3])
  aux <- sum(aux)/length(c)
  return(aux)
}
