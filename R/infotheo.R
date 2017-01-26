#' Information-theoretic meta-features
#'
#' Information-theoretic meta-features are particularly appropriate to describe
#' discrete (categorical) attributes, but they also fit continuous ones.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes
#' @param y a factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param summary A list of methods to summarize the results as post-processing
#'  functions. See \link{post.processing} method to more information. (Default:
#'  \code{c("mean", "sd")})
#' @param ... Optional arguments to the summary methods.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class
#'  The details section describes the valid values for this group.
#' @details
#'  TODO describe discretization method
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"attributes.concentration"}{Represents the association between the
#'      nominal attributes. It is the Goodman and Kruskal's tau measure
#'      otherwise known as the concentration coefficient.}
#'    \item{"attribute.entropy"}{Represents the normalized entropy (a measure of
#'      randomness) of each attributes in the dataset.}
#'    \item{"class.concentration"}{Represents the association between the
#'      nominal attributes and the class. It is the Goodman and Kruskal's tau
#'      measure otherwise known as the concentration coefficient.}
#'    \item{"class.entropy"}{Represents the normalized entropy of the class that
#'      describes how much information is necessary to specify one class in
#'      the dataset.}
#'    \item{"equivalent.attributes"}{Represents the the number of attributes
#'      suitable to optimally solve the classification task using the dataset.}
#'    \item{"joint.entropy"}{Represents the total entropy of each attribute and
#'      the classe in the dataset.}
#'    \item{"mutual.information"}{Represents the common information shared
#'      between each attribute and the class in the dataset.}
#'    \item{"noise.signal"}{Represents the amount of irrelevant information
#'      contained in the dataset.}
#'  }
#'  Each one of these meta-features generate multiple values (by attribute
#'  and/or class value) and then it is post processed by the summary methods.
#'  See the \link{post.processing} method for more details about it.
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
#' mf.infotheo(iris[1:4], iris[5], c("class.entropy", "joint.entropy"))
#'
#' ## Use another summary methods
#' mf.infotheo(Species ~ ., iris, summary=c("min", "median", "max"))
#' @export
mf.infotheo <- function(...) {
  UseMethod("mf.infotheo")
}

#' @rdname mf.infotheo
#' @export
mf.infotheo.default <- function(x, y, features="all", summary=c("mean", "sd"),
                                ...) {
  if(!is.data.frame(x)){
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)){
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(nrow(x) != length(y)){
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all"){
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), TRUE)

  catdata <- replace.numeric.columns(x) #TODO control by user paramete
  #Remove constant columns
  catdata <- catdata[, apply(catdata, 2, stats::sd) != 0]

  #Pre processed data
  extra <- list(
    y.entropy = entropy(y),
    y.log = base::log2(nlevels(y)),
    x.entropy = sapply(catdata, entropy),
    x.log = sapply(sapply(catdata, nlevels), base::log2),
    mutinf = sapply(catdata, mutinf, y=y)
  )

  sapply(features, function(f){
    measure <- eval(call(f, x=catdata, y=y, extra=extra))
    post.processing(measure, summary, ...)
  }, simplify=FALSE)
}

#' @rdname mf.infotheo
#' @export
mf.infotheo.formula <- function(formula, data, features="all",
                                summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")){
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)){
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula,data)
  attr(modFrame, "terms") <- NULL

  mf.infotheo.default(modFrame[, -1], modFrame[, 1], features, summary, ...)
}

#' List the information theoretical meta-features
#'
#' @return A list of information theoretical meta-features names
#' @export
#'
#' @examples
#' ls.infotheo()
ls.infotheo <- function () {
  c("attributes.concentration", "attribute.entropy", "class.concentration",
    "class.entropy", "equivalent.attributes", "joint.entropy",
    "mutual.information", "noise.signal")
  #TODO attribute_entropy, class_entropy is the normalized version
  #TODO irrelevant.attributes
}

attributes.concentration <- function(x, ...) {
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]

  mapply(function (i, j) {
    concentration.coefficient(x[, i], x[, j])
  }, i=comb$i, j=comb$j)
}

attribute.entropy <- function(extra, ...) {
  #TODO by.class makes senses
  extra$x.entropy / extra$x.log
}

class.concentration <- function(x, y, ...) {
  apply(x, 2, concentration.coefficient, y)
}

class.entropy <- function(extra, ...) {
  extra$y.entropy / extra$y.log
}

concentration.coefficient <- function(x, y) {
  nij <- (table(as.data.frame(cbind(x, y))) / length(x))
  isum <- rowSums(nij)
  jsum <- colSums(nij)
  isum2 <- isum^2
  jsum2 <- jsum^2
  nij2 <- nij^2

  total <- mapply(function (i, j) {
    nij2[i, j] / isum[i]
  }, i=rep(seq(nrow(nij)), each=ncol(nij)), j=rep(seq(ncol(nij)), nrow(nij)))

  (sum(total) - sum(jsum2)) / (1 - sum(jsum2))
}

entropy <- function(x) {
  # infotheo::entropy generate different values
  qi <- table(x) / length(x)
  -sum(qi * sapply(qi, log2))
}

equivalent.attributes <- function(extra, ...) {
  extra$y.entropy / mean(extra$mutinf)
}

irrelevant.attributes <- function(x, y, extra, ...) {
  #TODO
}

joint.entropy <- function(x, y, ...) {
  #TODO without normalization
  joint.data <- sapply(as.data.frame(sapply(x, paste, y)), as.factor)
  sapply(as.data.frame(joint.data), entropy)
}

mutinf <- function(x, y) {
  # infotheo::mutinformation generate different values
  entropy(x) + entropy(y) - entropy(paste(x, y))
}

mutual.information <- function(extra, ...) {
  extra$mutinf
}

noise.signal <- function(extra, ...) {
  mutinf <- mean(extra$mutinf)
  (mean(extra$x.entropy) - mutinf) / mutinf
}
